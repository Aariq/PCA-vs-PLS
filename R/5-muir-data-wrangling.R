#Wrangling data from Muir et al.

library(tidyverse)

#read in leaf trait data
tomato.raw <- read_csv(here::here("data", "literature", "Muir et al.", "data.csv"))
tomato <-
  tomato.raw %>%
  dplyr::select(-X1, -PC2, -PC1)
# tomato

#read in species abbreviations and lat long:
location <- read_csv(here::here("data", "literature", "Muir et al.", "TableS1.csv"))

#join
tomato.1 <- 
  left_join(tomato, location %>% dplyr::select(Spe, Latitude, Longitude, Taxon)) %>%
  filter(!is.na(Latitude))
# tomato.1

# download climate data and join
library(raster)

r <- getData("worldclim",var = "bio", res = 5, path = here("data"))
r <- r[[c(1,12)]]
names(r) <- c("temp","precip")

coords <- data.frame(x = tomato.1$Longitude, y = tomato.1$Latitude)

points <- SpatialPoints(coords, proj4string = r@crs)
weather <- 
  extract(r, points) %>% 
  as_tibble()

tomato.2 <- 
  tomato.1 %>%
  add_column(temp = weather$temp, precip = weather$precip)

# Write to RDS
write_rds(tomato.2, here::here("data", "literature", "Muir et al.", "tomatoes.rds"))

