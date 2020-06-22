library(here)
library(tidyverse)
library(glue)
library(furrr)
library(rlang)
library(beepr) # just makes a sound when long-running processes are done.

source(here("R", "ropls_helpers.R"))
source(here("R", "RMSEP.R"))

set.seed(464)


# Setup parallelization with furrr
plan(multiprocess)

# Get Summary Stats

## No Predictors

none.pls <- read_rds(here('data', 'models', 'none-pls.rds'))
none.pls.auto <- read_rds(here("data", "models", "none-pls-auto.rds"))
none.pcr <- read_rds(here('data', 'models', 'none-pcr.rds'))
none.pcr.auto <- read_rds(here("data", "models", "none-pcr-auto.rds"))

none.pls.summary <- pls.summary(none.pls)
none.pls.auto.summary <- pls.summary(none.pls.auto)
none.pcr.summary <- pcr.summary(none.pcr)
none.pcr.auto.summary <- pcr.summary(none.pcr.auto)
beep(4)
none.summary <-
  full_join(none.pls.summary, none.pls.auto.summary, by = "Statistic") %>% 
  full_join(none.pcr.summary) %>% full_join(none.pcr.auto.summary)
# write_excel_csv(none.summary, here("out", "none summary.csv"))

rm(none.pls, none.pcr, none.pls.auto, none.pcr.auto)


## Apparent Predictors

apparent.pls <- read_rds(here('data', 'models', 'apparent-pls.rds'))
apparent.pls.auto <- read_rds(here("data", "models", "apparent-pls-auto.rds"))
apparent.pcr <- read_rds(here('data', 'models', 'apparent-pcr.rds'))
apparent.pcr.auto <- read_rds(here("data", "models", "apparent-pcr-auto.rds"))

apparent.pls.summary <- pls.summary(apparent.pls)
apparent.pls.auto.summary <- pls.summary(apparent.pls.auto)
apparent.pcr.summary <- pcr.summary(apparent.pcr)
apparent.pcr.auto.summary <- pcr.summary(apparent.pcr.auto)
beep(4)
apparent.summary <-
  full_join(apparent.pls.summary, apparent.pls.auto.summary, by = "Statistic") %>% 
  full_join(apparent.pcr.summary) %>% full_join(apparent.pcr.auto.summary)
# write_excel_csv(apparent.summary, here("out", "apparent summary.csv"))

rm(apparent.pls, apparent.pcr, apparent.pls.auto, apparent.pcr.auto)


## Hidden Predictors


hidden.pls <- read_rds(here('data', 'models', 'hidden-pls.rds'))
hidden.pls.auto <- read_rds(here("data", "models", "hidden-pls-auto.rds"))
hidden.pcr <- read_rds(here('data', 'models', 'hidden-pcr.rds'))
hidden.pcr.auto <- read_rds(here("data", "models", "hidden-pcr-auto.rds"))

hidden.pls.summary <- pls.summary(hidden.pls)
hidden.pls.auto.summary <- pls.summary(hidden.pls.auto)
hidden.pcr.summary <- pcr.summary(hidden.pcr)
hidden.pcr.auto.summary <- pcr.summary(hidden.pcr.auto)
beep(4)
hidden.summary <-
  full_join(hidden.pls.summary, hidden.pls.auto.summary, by = "Statistic") %>% 
  full_join(hidden.pcr.summary) %>% full_join(hidden.pcr.auto.summary)
# write_excel_csv(hidden.summary, here("out", "hidden summary.csv"))

rm(hidden.pls, hidden.pcr, hidden.pls.auto, hidden.pcr.auto)


# Combine into one table

stat_order <-
  c(
    "n",
    "RMSEP",
    "R2X",
    "R2Y",
    "Q2",
    "%pR2 < 0.05",
    "%pQ2 < 0.05",
    "%p < 0.05",
    "ncomp",
    "num.ncomp1",
    "num.ncomp2",
    "num.ncomp3",
    "num.ncompmore"
  )

outtable <-
  list(none.summary, apparent.summary, hidden.summary) %>%
  future_map(
    ~ select(
      .x,
      Statistic,
      "PCA" = ends_with("pcr"),
      "PLS" = ends_with("pls"),
      "PCA (autofit)" = ends_with("pcr.auto"),
      "PLS (autofit)" = ends_with("pls.auto")
    )
  ) %>%
  set_names(c("No predictors", "Apparent predictors", "Hidden predictors")) %>%
  bind_rows(.id = "Scenario") %>%
  filter(Statistic %in% stat_order) %>%
  mutate(Statistic = fct_relevel(Statistic, stat_order),
         Scenario = fct_inorder(Scenario)) %>%
  arrange(Scenario, Statistic)

write_excel_csv(outtable, here("out", "model statistics table.csv"), na = "-")



