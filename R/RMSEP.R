library(ggfortify)
library(rlang)
library(rsample)
library(ropls)
library(furrr)


safe_opls <- purrr::possibly(ropls::opls, NA)


#' Calculate external cross validation RMSEP (RMSE-CV) for a PLS-DA model
#'
#' First column must be grouping variable (Y)
#'
#' @param plsda a ropls model
#' @param CV number of folds
#'
#' @return length 1 vector
#'
plsda_RMSEP <- function(plsda, CV = 7){
  
  #data gets passed to vfold_cv()
  data <- bind_cols(group = plsda@suppLs$y,  ggfortify::unscale(plsda@suppLs$xModelMN))
  ncomp <- plsda@summaryDF$pre #passed to .plsda_MSEP()
  # Do CV
  df.cv <- rsample::vfold_cv(data, CV)
  
  .plsda_MSEP <- function(split, ncomp){

    X1 <- rsample::analysis(split)[ , -1]
    Y1 <- rsample::analysis(split)[[1]] %>% as.factor() %>% as.numeric()
    X2 <- rsample::assessment(split)[ , -1]
    
    plsda <- ropls::opls(X1, Y1, predI = ncomp, permI = 0, info.txtC = "none", fig.pdfC = "none")
    
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
  
  #map MSEP calculation on each fold/split
 sqrt(mean(furrr::future_map_dbl(df.cv$splits, ~.plsda_MSEP(., ncomp))))
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
  load2 <- split(load[, -1], as.factor(load$axis))
  #for each axis, use loadings to calculate predicted scores from .newdata
  pred.scores <-
    future_map(load2, ~{
      #for one axis, mutiply every column of .newdata by the loading for that column
      future_map2_dfc(.x = .x, .y = .newdata, ~.x*.y) %>% 
        #then add them together, rowwise, to get scores for each observation (row) in .newdata
        rowSums()
    }) %>% 
    as_tibble() %>% 
    add_column(sample = paste0("s", 1:nrow(.newdata)), .before = 1)
  
  return(pred.scores)
}



#' Calculate external cross validation RMSEP (RMSE-CV) for a PCA-LR model
#'
#' First column must be grouping variable named "group"
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
pca_lr_RMSEP <- function(pca_lr, CV = 7){

  data <- pca_lr$data
  ncomp <- pca_lr$pca@summaryDF$pre
  # Do CV
  df.cv <- rsample::vfold_cv(data, CV)
  
  
  .pca_MSEP <- function(split, ncomp) {
    #do pca on analysis(data)
    # Do PCA on X
    pca <- try(opls(rsample::analysis(split)[ , -1], predI = ncomp, fig.pdfC = "none", info.txtC = "none"))
    
    # sometimes PCA fails for a split just by chance (system is singular error) Catch these and return NA
    
    if(inherits(pca, "try-error")) {
      return(NA)
    } else {
      
      # Get scores and bind with Y
      scores <-
        get_scores(pca) %>% 
        add_column(group = rsample::analysis(split)[[1]])
      
      #predict pc axis scores on assessment data
      scores.pred <- pca_scorepred(pca, assessment(split)[ , -1])
      
      # Make formula for logistic regression
      npcs <- pca@summaryDF$pre
      pcs <- glue("p{1:npcs}")
      mod_form <- as.formula(glue("group ~ {glue_collapse(pcs, sep = '+')}"))
      
      #do glm on analysis data
      m <- try(glm(mod_form, family = "binomial", data = scores))
      if (inherits(m, "try-error")){
        return(NA)
      } else {
        
        #use glm to predict `group` for newdata
        MSEP <- scores.pred %>%
          mutate(group.pred = predict(m, newdata = scores.pred, type = "response")) %>%
          add_column(group.actual = assessment(split)[[1]]) %>%
          mutate(sq_err = (group.actual - group.pred)^2) %>%
          summarize(MSEP = mean(sq_err)) %>%
          as.numeric()
        return(MSEP)
      }
    }
  }
  
  #map MSEP calculation on each fold/split
  future_map_dbl(df.cv$splits, ~.pca_MSEP(.x, ncomp)) %>%
    #average over folds
    mean(., na.rm = TRUE) %>% 
    # take square root
    sqrt()
}
# set.seed(5)
# pca_lr_RMSEP(testpcr, -group, group) #maybe working correctly?  Double check!


#' Summarize results from multiple PLS-DA models
#' 
#'
#' @param pls a list of PLS-DA models from the ropls package
#'
#' @return a tibble summarizing results

pls.summary <- function(pls) {
  pls_name <- enquo(pls)
  
  meansd <- function(x) {
    glue("{round(mean(x, na.rm = TRUE), 3)} ± {round(sd(x, na.rm = TRUE), 3)}")
  }
  
  summary_wide <- 
    pls %>%
    # do RMSEP on successful models and store in summaryDF 
    future_map(~.x@summaryDF %>%
                 mutate(RMSEP = plsda_RMSEP(.x, CV = 7))) %>% 
    bind_rows(.id = "dataset") %>% 
    select(dataset, Q2 = `Q2(cum)`, R2Y = `R2Y(cum)`, R2X = `R2X(cum)`, pR2 = pR2Y, pQ2 = pQ2, RMSEP, ncomp = pre) %>% 
    summarize(n = as.character(n()),
              `%pR2 < 0.05` = as.character(round(sum(pR2<0.05)/n()*100), 2),
              `%pQ2 < 0.05` = as.character(round(sum(pQ2<0.05)/n()*100), 2),
              across(where(is.numeric), meansd))
    
  summary_long <- 
    summary_wide %>%
    pivot_longer(everything(), names_to = "Statistic", values_to = as_name(pls_name))
  return(summary_long)
}

#' Summarize results from multiple PCA-LR models
#'
#' @param pcr a list of models made with pca_lr()
#'
#' @return a tibble summarizing results

pcr.summary <- function(pcr) {
  pcr_name <- enquo(pcr)
  
  meansd <- function(x) {
    glue("{round(mean(x, na.rm = TRUE), 3)} ± {round(sd(x, na.rm = TRUE), 3)}")
  }
  
  summary_wide <-
    pcr %>% 
    future_map_dfr(~.$mod.stats, .id = "dataset") %>% 
    mutate(RMSEP = future_map_dbl(pcr, ~pca_lr_RMSEP(.x, CV = 7)),
           R2X = future_map_dbl(pcr, ~.$pca@summaryDF$`R2X(cum)`),
           ncomp = future_map_dbl(pcr, ~.$pca@summaryDF$pre)) %>% 
    select(dataset, R2Y, p = p.value, RMSEP, R2X, ncomp) %>% 
    summarize(n = as.character(n()),
              `%p < 0.05` = as.character(round(sum(p<0.05)/n()*100), 2),
              across(where(is.numeric), meansd))
  
  summary_long <- 
    summary_wide %>%
    pivot_longer(everything(), names_to = "Statistic", values_to = as_name(pcr_name))
  return(summary_long)
}
