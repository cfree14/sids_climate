

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(countrycode)

# Directories
datadir <- "data"
plotdir <- "figures"
tabledir <- "tables"

# Country table
country_key_orig <- readxl::read_excel(file.path(tabledir, "Table1_countries.xlsx"))


# Format
################################################################################

# Format country key
country_key <- country_key_orig %>% 
  # Reduce
  rename(country_orig=country) %>% 
  select(region, country_orig) %>% 
  # Fix country names
  mutate(country=recode(country_orig, 
                        "Micronesia"="Federated States of Micronesia",
                        "Saō Tome and Principe"="São Tomé and Príncipe"), 
         country=countrycode(country, "country.name", "country.name"),
         iso3=countrycode(country, "country.name", "iso3c")) %>% 
  # Simplify
  select(region, country, iso3) %>% 
  arrange(region, country)

# Find lat/long for country centroids
################################################################################

# Country polygons
world <- rnaturalearth::ne_countries(scale="large", returnclass = "sf")

countries1 <- world %>% 
  # Countries of interest
  filter(iso_a3 %in% country_key$iso3)

countries1_centroids <- countries1 %>% 
  sf::st_centroid() %>% 
  sf::st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long_dd=X, lat_dd=Y) %>% 
  mutate(iso3=countries1$iso_a3)

# Country points
world_tiny <- rnaturalearth::ne_countries(type="tiny_countries", returnclass = "sf")

countries2 <- world_tiny %>% 
  # Countries of interest
  filter(iso_a3 %in% country_key$iso3)

countries2_centroids <- countries2 %>% 
  sf::st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long_dd=X, lat_dd=Y) %>% 
  mutate(iso3=countries2$iso_a3)

# Country centers
countries3_centroids <- countries2_centroids %>% 
  # Merge centers - prioritze centers from tiny countries
  bind_rows(countries1_centroids %>% filter(!iso3 %in% countries2_centroids$iso3)) %>% 
  # Manually change Fiji coordinates
  mutate(lat_dd=ifelse(iso3=="FJI", -17.844311, lat_dd),
         long_dd=ifelse(iso3=="FJI", 177.986987, long_dd))

# Add lat/long
country_key_final <- country_key %>% 
  left_join(countries3_centroids)

# Export
################################################################################

# Export country key
saveRDS(country_key_final, file=file.path(datadir, "country_key.Rds"))

