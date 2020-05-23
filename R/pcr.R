library(glue)
library(dplyr)
library(tibble)
library(ropls)
library(rlang)
library(rsample)
library(purrr)
library(here)
library(lmtest)
source(here("R", "ropls_helpers.R"))

#' Do PCA logistic regression (PCA-LR) using ropls::opls() and logistic glm.
#'
#' @param X_vars 
#' @param Y_var 
#' @param data 
#' 
#' @import glue
#' @import rlang
#' @import ropls
#' @import holodeck
#' @importFrom lmtest lrtest
#' @return
#' @export
#'
#' @examples
#' 
pca_lr <- function(data, X_vars, Y_var, CV = 7){
  X <- enquo(X_vars)
  Y <- enquo(Y_var)
  
  #check if Y_var is factor or numeric
  if(is.character(data[[quo_name(Y)]])){
    data <-
      data %>% 
      mutate(!!Y := as.factor(!!Y)) %>% 
      mutate(!!Y := as.numeric(!!Y) - 1)
  } 
  
  if(is.factor(data[[quo_name(Y)]])){
    data <-
      data %>%
      mutate(!!Y := as.numeric(!!Y) - 1)
  }
  if(!is.numeric(data[[quo_name(Y)]])){
    stop("grouping variable must be character, factor, or integer")
  }
  
  if(any(!data[[quo_name(Y)]] %in% c(0,1))){
    stop("grouping variable must be binary")
  }
  
  # Do PCA on X
  pca <- opls(select(data, !!X), info.txtC = "none", fig.pdfC = "none")
  
  # Get scores and bind with Y
  scores <- get_scores(pca) %>% 
    add_column(!!Y := data[[quo_name(Y)]])
  
  # Make formula
  npcs <- pca@summaryDF$pre
  pcs <- glue("p{1:npcs}")
  mod_form <- as.formula(glue::glue("{quo_name(Y)} ~ {glue_collapse(pcs, sep = '+')}"))
  
  # Do regression
  m <- glm(mod_form, family = "binomial", data = scores)
  m0 <- glm(group ~ 1, family = "binomial", data = scores)
  # get p-value and calc R2Y
  lik.test <- lrtest(m, m0)

  mod.stats <- broom::glance(m) %>%
    # mutate(R2Y = 1 - (deviance/null.deviance)) %>% 
    add_column(R2Y = broom::augment(m, type.predict = "response") %>% 
                 group_by(group) %>% 
                 summarize(means = mean(.fitted)) %>%
                 summarize(R2_tjur = diff(means)) %>%
                 as.numeric() %>% 
                 ungroup(),
               p.value = lik.test$`Pr(>Chisq)`[2])
  
  return(list(pca = pca, scores = scores, glm = m, mod.stats = mod.stats, data = data))
}



# set.seed(400)
# library(holodeck)
# data <- test.df <- sim_cat(n_obs = 30, n_groups = 2) %>%
#   sim_covar(n_vars = 5, var = 1, cov = 0.5, name = "cov") %>%
#   sim_covar(n_vars = 5, var = 1, cov = 0.5, name = "cov2") %>%
#   # sim_covar(n_vars = 5, var = 1, cov = 0, name = "noise") %>%
#   group_by(group) %>%
#   sim_discr(n_vars = 5, var = 1, cov = 0.1, group_means = c(-1, 1), name = "discr") %>%
#   ungroup() #%>%
# 
# data <-
#   data %>%
#   mutate(group = as.factor(group)) %>%
#   mutate(group = as.numeric(group) - 1)
# 
# pca <- opls(select(data, -group), plotL = FALSE, printL = FALSE)
# 
# scores <- get_scores(pca) %>%
#   add_column(group = data[["group"]])
# 
# npcs <- pca@summaryDF$pre
# pcs <- glue("p{1:npcs}")
# mod_form <- as.formula(glue::glue("group ~ {glue_collapse(pcs, sep = '+')}"))
# m <- glm(mod_form, family = "binomial", data = scores)
# 
# glm <- broom::glance(m) %>%
#   mutate(R2Y = 1 - (deviance/null.deviance)) %>% 
#   add_column(p.value = lrtest(m)$`Pr(>Chisq)`[2])
# 
# test <-lrtest(m)
# p.value <- test$`Pr(>Chisq)`[2]
# # mutate(group = as.numeric(as.factor(group)))
# m <- pca_lr(X_vars = -group, Y_var = group, CV = 10, data = test.df)
# m

