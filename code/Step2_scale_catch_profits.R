
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

# Read SID observed catch 
catch_orig <- rio::import(file.path(datadir, "MASTER_Seafood_Catch_SIDS_Study.xlsx"))

# Load SID project catch/profits
data_orig <- readRDS(file.path(datadir, "sid_total_catch_profit_timeseries.Rds"))

# Read FAO data
fao_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/data/fao/capture/processed/1950_2017_fao_landings_data.Rds")


# Build data
################################################################################

# Columns
# Q = imports
# R = fishing in another country's EEZ
# H = fishing in the high seas
# P = fishing in country's own EEZ (local catch)
# R+ H + P = catch

# Format FAO catch
fao2012 <- fao_orig %>% 
  filter(area_type=="marine" & year == 2012 & units=="t") %>% 
  group_by(country, iso3) %>% 
  summarise(catch_mt=sum(quantity, na.rm=T)) %>% 
  filter(iso3 %in% data_orig$country_iso3) %>% 
  rename(catch_mt_2012fao=catch_mt) %>% 
  ungroup()

# Format SID catch
catch <- catch_orig %>% 
  # Reduce columns
  select(1:8) %>% 
  # Rename columns
  rename(country=CName, iso3=ISO3, year=Year, 
         imports_mt=Country_Q, catch_mt_int=Country_R, catch_mt_hsa=Country_H, catch_mt_own=Country_P, 
         seafood_tot_mt="R+H+P+Q")

# 2012 values
proj2012 <- data_orig %>% 
  filter(scenario=="No Adaptation" & rcp=="RCP 2.6" & year==2012) %>% 
  select(country, country_iso3, catch_mt) %>% 
  rename(catch_mt_2012proj=catch_mt)
obs2012 <- catch %>% 
  filter(year==2012) %>% 
  rename(catch_mt_2012rw=catch_mt_own)

# Build scalars data frame
scalars <- proj2012 %>% 
  # Add FAO 2012 catch
  left_join(fao2012 %>% select(iso3, catch_mt_2012fao), by=c("country_iso3"="iso3")) %>% 
  # Add Reg Watson 2012 catch
  left_join(obs2012 %>% select(iso3, catch_mt_2012rw), by=c("country_iso3"="iso3")) %>% 
  # Observed catch and scalar from projected to observed
  mutate(catch_mt_2012obs=ifelse(!is.na(catch_mt_2012rw), catch_mt_2012rw, catch_mt_2012fao),
         scalar=catch_mt_2012obs/catch_mt_2012proj)

# Histrogram of scalars
hist(scalars$scalar, breaks=seq(0,14,0.2), col="grey80")
abline(v=1, lwd=2)

# Format data 
data <- data_orig %>% 
  # Add scalars
  left_join(scalars %>% select(-country), by="country_iso3") %>% 
  # Scale catch, profits, and MSY
  mutate(catch_mt_scaled=catch_mt * scalar,
         profits_usd_scaled=profits_usd * scalar,
         msy_mt_scaled=msy_mt * scalar)


# Export scaled dataset
saveRDS(data, file.path(datadir, "sid_total_catch_profit_timeseries_scaled.Rds"))
  




