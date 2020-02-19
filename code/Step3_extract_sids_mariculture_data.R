
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(countrycode)

# Directories
indir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/aquacast/output/processed/"
datadir <- "data"
plotdir <- "figures"

# Read country list
sids <- read.csv(file.path(datadir, "sids_country_list.csv"), as.is=T) %>% 
  mutate(country_orig=recode(country_orig, "Micronesia"="Federated States of Micronesia"),
         country=countrycode(country_orig, "country.name", "country.name"),
         iso3=countrycode(country, "country.name", "iso3c"))

# Read data
fin_orig <- read.csv(file.path(indir, "finfish_mariculture_potential_by_eez_rcp_feed_scenario.csv"))
biv_orig <- read.csv(file.path(indir, "bivalve_mariculture_potential_by_eez_rcp.csv"))


# Build data
################################################################################

# NOTE: These need to be summed by country

# SIDS finfish mariculture data
fin <- fin_orig %>% 
  # Reduce to SIDs and equal development pattern
  filter(ter1_iso %in% sids$iso3 & dev_pattern=="Equal development") %>% 
  # Reduce columns
  select(feed_scen:year, ter1_iso, ter1_name, ncells:profits_usd) %>% 
  # Rename columns
  rename(country=ter1_name, iso3=ter1_iso)

# SIDS bivalve mariculture data
biv <- biv_orig %>% 
  # Reduce to SIDs
  filter(ter1_iso %in% sids$iso3) %>% 
  # Reduce columns
  select(rcp:year, ter1_iso, ter1_name, ncells:profits_usd) %>% 
  # Rename columns
  rename(country=ter1_name, iso3=ter1_iso)

# Export data
write.csv(fin, file.path(datadir, "sids_finfish_mariculture_by_rcp_feed_scenario.csv"), row.names=F)
write.csv(biv, file.path(datadir, "sids_bivalve_mariculture_by_rcp.csv"), row.names=F)



