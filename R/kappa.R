#Functions for calculating kappa coefficients and confusion matrix
library(car)
library(broom)
library(dplyr)
library(rlang)
library(here)
source(here("R", "ropls_helpers.R"))

conf_pcr <- function(pcr){
  # Separate PCA and regression parts
  pca <- pcr$pca
  reg <- pcr$glm
  
  # Which PCs were significant in the regression?
  pcs <- reg %>% 
    car::Anova() %>% 
    broom::tidy() %>% 
    filter(p.value < 0.05 & term != "(Intercept)") %>% 
    .$term %>%
    syms() #saves as expression for later unquoting with !!!
  
  #get scores and data from PCA part of model
  scores <- pca %>%
    get_scores() %>%
    select(!!!pcs)
  
  data <- pcr$pca@suppLs$xModelMN
  
  #figure out appropriate cutoff by solving for the r that gives p < 0.05 given n
  n = pca@descriptionMC[1,1] %>% as.numeric()
  t = qt(0.05, n)
  r = sqrt(-(t^2)/(-t^2 - n + 2)) #solved t stat calculation for r
  
  #create confusion matrix
  #variables are detected as discriminating if they have significant correlation with significant PCs
  cor(scores, data) %>%
    t() %>%
    as_tibble(rownames = "Variable") %>% 
    mutate_if(is.numeric, funs(abs(.) > r)) %>% 
    rowwise() %>% 
    mutate(detect_discr = any(!!!pcs),
           is_discr = str_detect(Variable, "D"))
}

conf_pls <- function(pls){
  #get scores and data
  scores <- pls %>%
    get_scores() %>% 
    select(-y1, -sample) #just keep significant predictive axes columns
  
  #get predictive axis names
  predcomps <- colnames(scores) %>% syms()
  
  data <- pls@suppLs$xModelMN
  
  #figure out appropriate cutoff by solving for the r that gives p < 0.05 given n
  
  n = pls@descriptionMC[1,1] %>% as.numeric()
  t = qt(0.05, n)
  r = sqrt(-(t^2)/(-t^2 - n + 2)) #solved t stat calculation for r
  
  cor(scores, data) %>%
    t() %>%
    as_tibble(rownames = "Variable") %>% 
    mutate_if(is.numeric, list(~abs(.) > r)) %>% 
    rowwise() %>% 
    mutate(detect_discr = any(!!!predcomps),
           is_discr = str_detect(Variable, "D"))
}


conf_pls_vip <- function(pls.model){
  get_VIP(pls.model) %>%
    mutate(detect_discr = ifelse(VIP > 1, TRUE, FALSE),
           is_discr = ifelse(str_detect(Variable, "D"), TRUE, FALSE))
}


calc_kappa <- function(confusion_df) {
  confusion_df %>% 
    select(detect_discr, is_discr) %>%
    as.matrix() %>%
    cohen.kappa() %>%
    .$kappa %>% 
    data_frame(kappa = .)
}