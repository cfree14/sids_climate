
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(countrycode)

# Directories
indir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/edf_climate/cc_trade/data/gaines"
indir2 <- "/Users/cfree/Dropbox/Chris/UCSB/projects/edf_climate/cc_trade/imperfect/data"
datadir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/sids_climate/data"
plotdir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/sids_climate/figures"

# Read FAO data
fao_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/data/fao/capture/processed/1950_2017_fao_landings_data.Rds")


# Build data
################################################################################

# Build data
data <- fao_orig %>% 
  filter(country_use=="Nauru")

# Plot data
g <- ggplot(data, aes(x=year, y=quantity, fill=comm_name)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Year", y="Landings (mt)") +
  # Theme
  theme_bw()
g
