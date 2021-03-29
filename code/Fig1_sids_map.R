
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

# Read data
country_key <- readRDS(file.path(datadir, "country_key.Rds"))

# Read EEZ data
eezs_orig <- sf::st_read(dsn="/Users/cfree/Dropbox/Chris/UCSB/data/eezs/World_EEZ_v11_20191118_LR", layer="eez_v11_lowres") 

# EEZs use
eezs <- eezs_orig %>% 
  filter(ISO_TER1 %in% country_key$iso3)

# Country polygons
world <- rnaturalearth::ne_countries(scale="large", returnclass = "sf")


# Plot
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_blank(),
                   axis.title=element_blank(),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_blank(),
                   plot.title=element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"))

# Map countries
g <- ggplot() +
  # Plot world
  geom_sf(data=world, fill="grey80", color="white", lwd=0.1) +
  # Plot EEZs
  geom_sf(data=eezs, mapping=aes(fill=ISO_SOV1), show.legend = F, color="grey30", lwd=0.1) +
  # Plot country labels
  geom_point(data=country_key, mapping=aes(x=long_dd, y=lat_dd), size=0.8) +
  ggrepel::geom_text_repel(data=country_key, mapping=aes(x=long_dd, y=lat_dd, label=country), 
                           size=1, segment.size=0.2, min.segment.length = 0, max.overlaps = 1000) +
  # Crop
  coord_sf(y=c(-55, NA)) +
  # Theme
  theme_bw() + my_theme
#g

# Export figure
ggsave(g, filename=file.path(plotdir, "Fig1_eez_map.png"), 
       width=6.5, height=2.75, units="in", dpi=600)
