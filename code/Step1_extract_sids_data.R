
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

# Read country list
sids <- read.csv(file.path(datadir, "sids_country_list.csv"), as.is=T) %>% 
  mutate(country_orig=recode(country_orig, "Micronesia"="Federated States of Micronesia"),
         country=countrycode(country_orig, "country.name", "country.name"),
         iso3=countrycode(country, "country.name", "iso3c"))



# Build MSY projections
################################################################################

# Read MSY projections
tmsy <- readRDS(file.path(indir, "gaines_territory_level_msy_time_series_by_type.Rds"))

# MSY projections
msy_use <- tmsy %>% 
  # Remove High Seas, Disputed, Join
  filter(!sovereign_iso3 %in% c("ABNJ", "Disputed", "Joint")) %>% 
  # Add country ISO
  mutate(country_orig=recode(country, "Micronesia"="Federated States of Micronesia"),
         country_iso3=countrycode(country_orig, "country.name", "iso3c")) %>% 
  # Reduce to countries of interest
  filter(country_iso3 %in% sids$iso3) %>% 
  mutate(country=countrycode(country_iso3, "iso3c", "country.name")) %>% 
  # Summarize MSY by country
  group_by(rcp, country, country_iso3, year) %>% 
  summarize(msy_mt=sum(msy, na.rm=T)) %>% 
  # Format columns
  ungroup() %>% 
  mutate(rcp=recode(rcp, "RCP26"="RCP 2.6", "RCP45"="RCP 4.5", "RCP60"="RCP 6.0", "RCP85"="RCP 8.5"))

# Check that all SID ISO3s are represented
iso3s_avail <- sort(unique(msy_use$country_iso3))
sids$iso3[!(sids$iso3 %in% iso3s_avail)] # they are all in there!

# Export data
write.csv(msy_use, file.path(datadir, "sids_msy_projections.csv"), row.names=F)

  
# Build catch/profit projections
################################################################################

# Read data
# (takes a long time to read)
# data_orig <- readRDS(file.path(indir, "gaines_data_for_eez_analysis.Rds"))
data_orig2 <- readRDS(file.path(indir2, "gaines_data_for_eez_analysis_imperfect.Rds"))

# Reduce to countries of interest
data_full_sids <- data_orig2 %>% 
  filter(scenario != "Imperfect Full Adaptation 15 yr" & (country_iso3 %in% sids$iso3 | country=="Micronesia"))


# Sum catch/profits by SID nation (and RCP, scenario, and year)
data_sids <- data_full_sids %>% 
  mutate(country_iso3=ifelse(country=="Micronesia", "FSM", country_iso3)) %>% 
  group_by(rcp, scenario, country_iso3, year) %>% 
  summarize(catch_mt=sum(harvest_eez, na.rm=T),
            profits_usd=sum(profit_eez, na.rm=T),
            msy_mt=sum(msy_eez, na.rm=T)) %>% 
  ungroup()

# Format data nicely after summary
data_sids_final <- data_sids %>% 
  ungroup() %>% 
  mutate(rcp=recode(rcp, "RCP26"="RCP 2.6", "RCP45"="RCP 4.5", "RCP60"="RCP 6.0", "RCP85"="RCP 8.5"),
         scenario=recode(scenario, 
                         "Imperfect Full Adaptation 10 yr"="Realistic Adaptation (10 yr)", 
                         "Imperfect Full Adaptation 20 yr"="Realistic Adaptation (20 yr)", 
                         "Imperfect Full Adaptation 5 yr"="Realistic Adaptation (5 yr)"), 
         country=countrycode(country_iso3, "iso3c", "country.name")) %>% 
  select(rcp, scenario, country, everything())

# Check that all SID ISO3s are represented
iso3s_avail2 <- sort(unique(data_sids_final$country_iso3))
sids$iso3[!(sids$iso3 %in% iso3s_avail2)] # they are all in there!

# Export data
saveRDS(data_sids_final, file.path(datadir, "sid_total_catch_profit_timeseries.Rds"))


# Plot one
################################################################################

# Setup theme
my_theme <- theme(axis.text=element_text(size=8),
                  axis.title=element_text(size=10),
                  plot.title=element_text(size=12),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Check scenarios
#########################

# Data
check <- data_sids_final %>% 
  filter(country=="Antigua & Barbuda")

# Plot
g <- ggplot(check, aes(x=year, y=catch_mt/1000, color=scenario, group=scenario)) +
  geom_line() +
  facet_wrap(~rcp, ncol=2) +
  labs(x="", y="Catch (1000s mt)", title="Antigua & Barbuda") +
  scale_color_discrete(name="Scenario") +
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "figure_catch_with_climate_adaptation.png"), 
       width=6.5, height=4.5, units="in", dpi=600)

# Check MSY
#########################

# Data
check2 <- msy_use %>% 
  filter(country=="Antigua & Barbuda")

# Plot
g <- ggplot(check2, aes(x=year, y=msy_mt, color=rcp, group=rcp)) +
  geom_line() +
  labs(x="", y="MSY (mt)", title="Antigua & Barbuda") +
  scale_color_discrete(name="Climate scenario") +
  theme_bw() + my_theme
g
  
  