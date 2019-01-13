library(tidyverse)
library(here)

TRYdata <- read_tsv(here("data", "TRY database", "5512.txt"))
# require(data.table)
# TRYdata <- fread(here("data", "TRY database", "5512.txt"), header = T, sep = "\t", dec = ".", quote = "", data.table = T)

View(TRYdata)

TRYdata %>% 
  filter(!is.na(X28))
#column 28 is empty, remove it.

TRYdata <- TRYdata %>% select(-X28)

# Get only georeferenced entries

georeferenced <-
  TRYdata %>% 
  filter(DataName %in% c("Altitude", "Latitude", "Longitude")) %>%
  group_by(ObservationID) %>% 
  summarize() %>%
  .$ObservationID

TRYdata.geo <-
  TRYdata %>% 
  filter(ObservationID %in% georeferenced)

View(TRYdata.geo)

#try to figure out which variables to use
## only ones with standard values?
TRYdata.geo %>% 
  # filter(!is.na(StdValue)) %>% 
  group_by(DataName) %>% 
  count() %>% 
  arrange(desc(n)) %>% View()

# Looks like not enough georeferenced entries to be worth it... hmmm....