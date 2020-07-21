library(here)
library(tidyverse)
library(glue)
library(furrr)
library(rlang)
library(beepr) # just makes a sound when long-running processes are done.

source(here("R", "ropls_helpers.R"))
source(here("R", "RMSEP.R"))

set.seed(464)


# Setup parallelization with furrr -----------
plan(multiprocess)

# Get Summary Stats

## No Predictors ---------

none.pls <- read_rds(here('data', 'models', 'none-pls.rds'))
none.pls.auto <- read_rds(here("data", "models", "none-pls-auto.rds"))
none.pcr <- read_rds(here('data', 'models', 'none-pcr.rds'))
none.pcr.auto <- read_rds(here("data", "models", "none-pcr-auto.rds"))

none.pls.stats <- 
  pls.stats(none.pls) %>% 
  add_column(Scenario = "No predictors",
             analysis = "PLS")
none.pls.auto.stats <-
  pls.stats(none.pls.auto) %>% 
  add_column(Scenario = "No predictors",
             analysis = "PLS (autofit)")
none.pcr.stats <-
  pcr.stats(none.pcr) %>% 
  add_column(Scenario = "No predictors",
             analysis = "PCA")
none.pcr.auto.stats <- 
  pcr.stats(none.pcr.auto) %>% 
  add_column(Scenario = "No predictors",
             analysis = "PCA (autofit)")
beep(4)

### Combine 
none.stats <- bind_rows(none.pls.stats, none.pls.auto.stats, none.pcr.stats, none.pcr.auto.stats)

### how often is each axis significant?
none.anova <-
  none.pcr.auto %>%
  future_map("anova") %>%
  bind_rows(.id = "dataset") %>% 
  group_by(term) %>% 
  summarize(n = n(),
            n.sig = sum(p.value < 0.05),
            `%p < 0.05` = n.sig/n*100)

rm(none.pls, none.pcr, none.pls.auto, none.pcr.auto)


## Apparent Predictors------------

apparent.pls <- read_rds(here('data', 'models', 'apparent-pls.rds'))
apparent.pls.auto <- read_rds(here("data", "models", "apparent-pls-auto.rds"))
apparent.pcr <- read_rds(here('data', 'models', 'apparent-pcr.rds'))
apparent.pcr.auto <- read_rds(here("data", "models", "apparent-pcr-auto.rds"))

apparent.pls.stats <-
  pls.stats(apparent.pls) %>% 
  add_column(Scenario = "Apparent predictors",
             analysis = "PLS")
apparent.pls.auto.stats <- 
  pls.stats(apparent.pls.auto) %>% 
  add_column(Scenario = "Apparent predictors",
             analysis = "PLS (autofit)")
apparent.pcr.stats <- 
  pcr.stats(apparent.pcr) %>% 
  add_column(Scenario = "Apparent predictors",
             analysis = "PCA")
apparent.pcr.auto.stats <- 
  pcr.stats(apparent.pcr.auto) %>% 
  add_column(Scenario = "Apparent predictors",
             analysis = "PCA (autofit)")
beep(4)

## Combine
apparent.stats <- bind_rows(apparent.pls.stats, apparent.pls.auto.stats, apparent.pcr.stats, apparent.pcr.auto.stats)

### how often is each axis significant?
apparent.anova <-
  apparent.pcr.auto %>%
  future_map("anova") %>%
  bind_rows(.id = "dataset") %>% 
  group_by(term) %>% 
  summarize(n = n(),
            n.sig = sum(p.value < 0.05),
            `%p < 0.05` = n.sig/n*100)

rm(apparent.pls, apparent.pcr, apparent.pls.auto, apparent.pcr.auto)


## Hidden Predictors


hidden.pls <- read_rds(here('data', 'models', 'hidden-pls.rds'))
hidden.pls.auto <- read_rds(here("data", "models", "hidden-pls-auto.rds"))
hidden.pcr <- read_rds(here('data', 'models', 'hidden-pcr.rds'))
hidden.pcr.auto <- read_rds(here("data", "models", "hidden-pcr-auto.rds"))

hidden.pls.stats <-
  pls.stats(hidden.pls) %>% 
  add_column(Scenario = "Hidden predictors",
             analysis = "PLS")
hidden.pls.auto.stats <-
  pls.stats(hidden.pls.auto) %>% 
  add_column(Scenario = "Hidden predictors",
             analysis = "PLS (autofit)")
hidden.pcr.stats <-
  pcr.stats(hidden.pcr) %>% 
  add_column(Scenario = "Hidden predictors",
             analysis = "PCA")
hidden.pcr.auto.stats <-
  pcr.stats(hidden.pcr.auto) %>% 
  add_column(Scenario = "Hidden predictors",
             analysis = "PCA (autofit)")
beep(4)

## Combine
hidden.stats <- bind_rows(hidden.pls.stats, hidden.pls.auto.stats, hidden.pcr.stats, hidden.pcr.auto.stats)

### how often is each axis significant?
hidden.anova <-
  hidden.pcr.auto %>%
  future_map("anova") %>%
  bind_rows(.id = "dataset") %>% 
  group_by(term) %>% 
  summarize(n = n(),
            n.sig = sum(p.value < 0.05),
            `%p < 0.05` = n.sig/n*100)

rm(hidden.pls, hidden.pcr, hidden.pls.auto, hidden.pcr.auto)

# Combine into one table

meansd <- function(x) {
  if (all(is.na(x))) {
    NA
  } else {
    glue("{round(mean(x, na.rm = TRUE), 3)} ± {round(sd(x, na.rm = TRUE), 3)}")
  }
}

all.stats <- bind_rows(none.stats, apparent.stats, hidden.stats)
summary_wide <- all.stats %>% 
  group_by(Scenario, analysis) %>% 
  summarize(n = as.character(n()),
            `%p < 0.05` = first(ifelse(str_detect(analysis, "PCA"),
                                       as.character(round(sum(p < 0.05)/500*100), 2),
                                       as.character(round(sum(pQ2 < 0.05)/500*100), 2))),
            num.ncomp1 = as.character(sum(ncomp == 1)),
            num.ncomp2 = as.character(sum(ncomp == 2)),
            num.ncomp3 = as.character(sum(ncomp ==3)),
            num.ncompmore = as.character(sum(ncomp > 3)),
            across(where(is.numeric), meansd))

# rearrange so columns are analyses and rows are different stats
stat_order <-
  c(
    "n",
    "RMSEP",
    "R2X",
    "R2Y",
    "Q2",
    "%p < 0.05",
    "ncomp",
    "num.ncomp1",
    "num.ncomp2",
    "num.ncomp3",
    "num.ncompmore"
  )

outtable <- 
  summary_wide %>% 
  pivot_longer(n:p, names_to = "Statistic") %>% 
  pivot_wider(names_from = analysis, values_from = value) %>% 
  filter(Statistic %in% stat_order) %>%
  mutate(Statistic = fct_relevel(Statistic, stat_order),
         Scenario = fct_inorder(Scenario)) %>%
  arrange(Scenario, Statistic)

write_excel_csv(outtable, here("out", "model statistics table.csv"), na = "-")

none.anova
apparent.anova
hidden.anova

# Plots ------

ggplot(all.stats, aes(x = Scenario, y = R2Y, fill = analysis)) +
  # geom_violin(draw_quantiles = 0.5)
  geom_boxplot()
