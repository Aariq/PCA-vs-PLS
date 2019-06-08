library(ggfortify)
library(rlang)
library(rsample)
library(ropls)


safe_opls <- purrr::possibly(ropls::opls, NA)

#' Calculate mean squared error of prediction from a single fold for RMSE-CV calculation for PLS-DA
#'
#' @param split 
#' @param X_vars 
#' @param Y_var 
#' @param ncomp 
#'
#' @return
#' @export
#'
#' @examples
.plsda_MSEP <- function(split, X_vars, Y_var, ncomp){
  X_vars = enquo(X_vars)
  Y_var = enquo(Y_var)
  
  X1 <- dplyr::select(rsample::analysis(split), !!X_vars)
  Y1 <- rsample::analysis(split)[[quo_name(Y_var)]] %>% as.factor() %>% as.numeric()
  
  X2 <- dplyr::select(rsample::assessment(split), !!X_vars)
  
  plsda <- ropls::opls(X1, Y1, predI = ncomp, permI = 0, plotL = FALSE, printL = FALSE)
  
  
  #on a single split
  assessment(split) %>%
    # predict values
    add_column(group.pred = predict(plsda, X2)) %>% 
    # calculate squared errors
    mutate(sq_err = (as.factor(group) %>% as.numeric() - group.pred)^2) %>%
    # Mean squared error for this split
    summarize(MSEP = mean(sq_err)) %>% 
    as.numeric()
}

#' Calculate external cross validation RMSEP (RMSE-CV) for a PLS-DA model
#'
#' @param plsda 
#' @param X_vars 
#' @param Y_var 
#' @param CV 
#'
#' @return
#' @export
#'
#' @examples
plsda_RMSEP <- function(plsda, X_vars, Y_var, CV = 7){
  X_vars <- enquo(X_vars)
  Y_var <- enquo(Y_var)
  #data gets passed to vfold_cv()
  data <- bind_cols(!!Y_var := plsda@suppLs$y,  ggfortify::unscale(plsda@suppLs$xModelMN))
  ncomp <- plsda@summaryDF$pre #passed to .plsda_MSEP()
  # Do CV
  df.cv <- rsample::vfold_cv(data, CV)
  
  #map MSEP calculation on each fold/split
  purrr::map_dbl(df.cv$splits, ~.plsda_MSEP(., !!X_vars, !!Y_var, ncomp)) %>%
    #average over folds
    mean() %>% 
    # take square root
    sqrt()
}

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
pca_scorepred <- function(pca_mod, .newdata, .scale = TRUE) {
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



#' Calculate mean squared error of prediction from a single fold for RMSE-CV calculation vor PCA-LR
#'
#' @param split 
#' @param X_vars 
#' @param Y_var 
#' @param ncomp
#'
#' @return
#' @export
#'
#' @examples
.pca_MSEP <- function(split, X_vars, Y_var, ncomp) {
  #do pca on analysis(data)
  
  X <- enquo(X_vars)
  Y <- enquo(Y_var)
  # Do PCA on X
  pca <- safe_opls(select(rsample::analysis(split), !!X), predI = ncomp, plotL = FALSE, printL = FALSE)
  
  # sometimes PCA fails for a split just by chance (system is singular error) Catch these and return NA
  
  if(!isS4(pca)) {
    return(NA)
  } else {
  
    # Get scores and bind with Y
    scores <-
      get_scores(pca) %>% 
      add_column(!!Y := rsample::analysis(split)[[quo_name(Y)]])
    
    #predict pc axis scores on assessment data
    scores.pred <- pca_scorepred(pca, assessment(split) %>% select(!!X))
    
    # Make formula for logistic regression
    npcs <- pca@summaryDF$pre
    pcs <- glue("p{1:npcs}")
    mod_form <- as.formula(glue("{quo_name(Y)} ~ {glue_collapse(pcs, sep = '+')}"))
    
    #do glm on analysis data
    m <- glm(mod_form, family = "binomial", data = scores)
    
    #use glm to predict `group` for newdata
    MSEP <- scores.pred %>%
      mutate(group.pred = predict(m, newdata = scores.pred, type = "response")) %>%
      add_column(group.actual = assessment(split)[[quo_name(Y)]]) %>%
      mutate(sq_err = (group.actual - group.pred)^2) %>%
      summarize(MSEP = mean(sq_err)) %>%
      as.numeric()
    
    return(MSEP)
  }
}

#' Calculate external cross validation RMSEP (RMSE-CV) for a PCA-LR model
#'
#' @param pca_lr 
#' @param X_vars 
#' @param Y_var 
#' @param CV 
#'
#' @return
#' @export
#'
#' @examples
pca_lr_RMSEP <- function(pca_lr, X_vars, Y_var, CV = 7){
  X_vars <- enquo(X_vars)
  Y_var <- enquo(Y_var)
  data <- pca_lr$data
  ncomp <- pca_lr$pca@summaryDF$pre
  # Do CV
  df.cv <- rsample::vfold_cv(data, CV)
  
  #map MSEP calculation on each fold/split
  map_dbl(df.cv$splits, ~.pca_MSEP(., !!X_vars, !!Y_var, ncomp)) %>%
    #average over folds
    mean(., na.rm = TRUE) %>% 
    # take square root
    sqrt()
}
# set.seed(5)
# pca_lr_RMSEP(testpcr, -group, group) #maybe working correctly?  Double check!
