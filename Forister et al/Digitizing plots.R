# Digitizing plots from Forister et al.
library(here)
library(tidyverse)
library(digitize)

# Digitize Fig 2 a
cala <- ReadAndCal(here("Forister et al", "F2a.jpg"))
species_NS <- DigitData()

cala <- ReadAndCal(here("Forister et al", "F2a.jpg"))
species_SM <- DigitData()

cala <- ReadAndCal(here("Forister et al", "F2a.jpg"))
species_WS <- DigitData()

cala <- ReadAndCal(here("Forister et al", "F2a.jpg"))
species_RC <- DigitData()

# Tidy data and write to .rds
Fig2a_data <- list(species_NS, species_SM, species_WS, species_RC) %>%
  set_names(c("NS", "SM", "WS", "NS")) %>% 
  map_dfr(~Calibrate(., cala, 1975, 2005, 20, 35),.id = "County") %>% 
  mutate(year = round(x)) %>% 
  rename(Effective_species = y)
write_rds(Fig2a_data, here("Forister et al", "Fig2a_data.rds"))

# Digitize figure d b
calb <- ReadAndCal(here("Forister et al", "F2b.jpg"))
other_1 <- DigitData()

calb <- ReadAndCal(here("Forister et al", "F2b.jpg"))
other_2 <- DigitData()

calb <- ReadAndCal(here("Forister et al", "F2b.jpg"))
other_3 <- DigitData()

calb <- ReadAndCal(here("Forister et al", "F2b.jpg"))
other_4 <- DigitData()

calb <- ReadAndCal(here("Forister et al", "F2b.jpg"))
yolo <- DigitData()

calb <- ReadAndCal(here("Forister et al", "F2b.jpg"))
sactown <- DigitData()

calb <- ReadAndCal(here("Forister et al", "F2b.jpg"))
solano <- DigitData()

# Tidy and write to .rds
Fig2b_data <- list(other_1, other_2, other_3, other_4, yolo, sactown, solano) %>%
  set_names("organophosphates", "carbamates", "pyrethroids", "organochlorines", "Yolo", "Sacramento", "Solano") %>% 
  map_dfr(~Calibrate(., calb, 1995, 2010, 0, 5), .id = "group") %>% 
  mutate(year = round(x)) %>% 
  rename(log_insecticide = y)
write_rds(Fig2b_data, here("Forister et al", "Fig2b_data.rds"))