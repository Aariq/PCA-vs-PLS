library(glue)
library(dplyr)
library(tibble)
library(ropls)
library(rlang)
library(rsample)
library(holodeck)
library(purrr)

#' Predict PC axis scores of new data from loadings
#'
#' @param pca_mod 
#' @param .newdata 
#' @param .scale 
#'
#' @return
#' @export
#'
#' @examples
mypredict <- function(pca_mod, .newdata, .scale = TRUE) {
  #get loadings
  load <-
    get_loadings(pca_mod) %>%
    gather(-Variable, key = axis, value = loading) %>%
    spread(Variable, loading) %>% 
    select(axis, colnames(.newdata))
  
  #scale newdata
  if(.scale){
    .newdata <- .newdata %>% mutate_all(~scale(.))
  }
  
  #check that columns are the same
  stopifnot(identical(colnames(.newdata), colnames(load %>% select(-axis))))
  
  #calc scores from loadings
  #clunky, but works
  pred.scores <-
    load %>% 
    group_by(axis) %>% 
    group_map(~{
      map2_dfc(.x = .newdata, .y = ., ~.x*.y) %>% rowSums(.) %>% as_tibble()
    }) %>% 
    mutate(sample = paste0("s", 1:nrow(.newdata))) %>% 
    spread(axis, value)
  
  return(pred.scores)
}



#' Calculate RMSEP on rsplit object
#'
#' @param split 
#' @param ... not used
#' @import rsample
#'
#' @return
#' @export
#'
#' @examples
pca_RMSEP <- function(split, X_vars, Y_var) {
  #do pca on analysis(data)
  
  X <- enquo(X_vars)
  Y <- enquo(Y_var)
  # Do PCA on X
  pca <- opls(select(analysis(split), !!X), plotL = FALSE, printL = FALSE)
  
  # Get scores and bind with Y
  scores <-
    get_scores(pca) %>% 
    add_column(!!Y := analysis(split)[[quo_name(Y)]])
  
  #predict pc axis scores on assessment data
  scores.pred <- mypredict(pca, assessment(split) %>% select(!!X))
  
  # Make formula
  npcs <- pca@summaryDF$pre
  pcs <- glue("p{1:npcs}")
  mod_form <- as.formula(glue("{quo_name(Y)} ~ {glue_collapse(pcs, sep = '+')}"))
  
  #do glm on analysis data
  m <- glm(mod_form, family = gaussian, data = scores)
  
  #use glm to predict `group` for newdata
  scores.pred %>%
    mutate(group.pred = predict(m, newdata = scores.pred)) %>%
    add_column(group.actual = assessment(split)[[quo_name(Y)]]) %>%
    mutate(sq_err = (group.actual - group.pred)^2) %>%
    summarize(RMSEP = sqrt(mean(sq_err))) %>%
    as.numeric()
}


#' Do PCA regression using ropls::opls() and gaussian glm.
#'
#' @param X_vars 
#' @param Y_var 
#' @param data 
#' 
#' @import glue
#' @import rlang
#' @import ropls
#' @import holodeck
#' 
#' @return
#' @export
#'
#' @examples
#' 
pcreg <- function(X_vars, Y_var, CV = 7, data){
  X <- enquo(X_vars)
  Y <- enquo(Y_var)
  
  #check if Y_var is factor or numeric
  if(is.character(data[[quo_name(Y)]])){
    data <-
      data %>% 
      mutate(!!Y := as.factor(!!Y)) %>% 
      mutate(!!Y := as.numeric(!!Y))
  } 
  
  if(is.factor(data[[quo_name(Y)]])){
    data <-
      data %>%
      mutate(!!Y := as.numeric(!!Y))
  }
  if(!is.numeric(data[[quo_name(Y)]])){
    stop("grouping variable must be character, factor, or numeric")
  }

  # Do PCA on X
  pca <- opls(select(data, !!X), plotL = FALSE, printL = FALSE)
  
  # Get scores and bind with Y
  scores <- get_scores(pca) %>% 
    add_column(!!Y := data[[quo_name(Y)]])
  
  # Make formula
  npcs <- pca@summaryDF$pre
  pcs <- glue("p{1:npcs}")
  mod_form <- as.formula(glue::glue("{quo_name(Y)} ~ {glue_collapse(pcs, sep = '+')}"))
  
  # Do regression
  m <- lm(mod_form, data = scores)
  
  # Do CV
  df.cv <- rsample::vfold_cv(data, CV)
  RMSEP <- df.cv %>%
    mutate(RMSEP = map_dbl(.$splits, ~pca_RMSEP(., !!X, !!Y))) %>% 
    summarize(RMSEP = mean(RMSEP)) %>%
    as.numeric()
  
  return(list(pca = pca, lm = m, RMSEP = RMSEP))
}
set.seed(400)
test.df <- sim_cat(N = 30, n_groups = 2) %>% 
  sim_covar(p = 5, var = 1, cov = 0.5, name = "cov") %>% 
  sim_covar(p = 5, var = 1, cov = 0.5, name = "cov2") %>% 
  # sim_covar(p = 5, var = 1, cov = 0, name = "noise") %>% 
  group_by(group) %>% 
  sim_discr(p = 5, var = 1, cov = 0.1, group_means = c(-1, 1), name = "discr") %>%
  ungroup() #%>% 
# mutate(group = as.numeric(as.factor(group)))
m <- pcreg(X_vars = -group, Y_var = group, CV = 10, data = test.df)
m

