# Wrangling data from Muir et al.
# Outputs tomatoes.rds which includes the trait data and weather data used in further analyses.
# To fully reproduce this, you'll need to download the data from https://doi.org/10.5061/dryad.1r8c2 and unzip the contents into "data/muir/".  Only the files "data.csv" and "TableS1.csv" are needed.

library(raster)
library(tidyverse)
library(here)

#read in leaf trait data
tomato.raw <- read_csv(here::here("data", "muir", "data.csv"))
tomato <-
  tomato.raw %>%
  dplyr::select(-X1, -PC2, -PC1)
# tomato

#read in species abbreviations and lat long:
location <- read_csv(here::here("data", "muir", "TableS1.csv"))

#join
tomato.1 <- 
  left_join(tomato, location %>% dplyr::select(Spe, Latitude, Longitude, Taxon)) %>%
  filter(!is.na(Latitude))
# tomato.1

# download climate data and join

r <- getData("worldclim",var = "bio", res = 5, path = here("data", "muir"))
r <- r[[c(1,12)]]
names(r) <- c("temp","precip")

coords <- data.frame(x = tomato.1$Longitude, y = tomato.1$Latitude)

points <- SpatialPoints(coords, proj4string = r@crs)
weather <- 
  raster::extract(r, points) %>% 
  as_tibble()

tomato.2 <- 
  tomato.1 %>%
  add_column(temp = weather$temp, precip = weather$precip)

# Write to RDS
write_rds(tomato.2, here::here("data", "tomatoes.rds"))

