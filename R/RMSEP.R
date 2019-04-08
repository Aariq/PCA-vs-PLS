library(ggfortify)
library(rlang)

.plsda_RMSEP <- function(split, X_vars, Y_var, ncomp){
  X_vars = enquo(X_vars)
  Y_var = enquo(Y_var)
  
  X1 <- select(analysis(split), !!X_vars)
  Y1 <- analysis(split)[[quo_name(Y_var)]] %>% as.factor() %>% as.numeric()
  
  X2 <- select(assessment(split), !!X_vars)
  
  plsda <- opls(X1, Y1, predI = ncomp, permI = 0, plotL = FALSE, printL = FALSE)
  
  #predict
  predict(plsda, X2)
  
  assessment(split) %>%
    add_column(group.pred = predict(plsda, X2)) %>% 
    mutate(sq_err = (as.factor(group) %>% as.numeric() - group.pred)^2) %>%
    summarize(RMSEP = sqrt(mean(sq_err))) %>%
    as.numeric()
}

plsda_RMSEP <- function(plsda, X_vars, Y_var, CV = 7){
  X_vars <- enquo(X_vars)
  Y_var <- enquo(Y_var)
  data <- bind_cols(!!Y_var := plsda@suppLs$y,  ggfortify::unscale(plsda@suppLs$xModelMN))
  ncomp <- plsda@summaryDF$pre
  # Do CV
  df.cv <- rsample::vfold_cv(data, CV)
  
  #map RMSEP calculation
  map_dbl(df.cv$splits, ~.plsda_RMSEP(., !!X_vars, !!Y_var, ncomp)) %>% mean()
}


