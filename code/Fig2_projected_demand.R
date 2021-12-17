
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(countrycode)

# Directories
datadir <- "data/final_data_from_lida"
plotdir <- "figures"

# Read data
data_orig <- readxl::read_excel(file.path(datadir, "Data_for_Figures_Climate_Smart_Small_Island_Fisheries_August2021.xlsx"), sheet=1)


# Format data
################################################################################

# Column names
colnames(data_orig) 
colnames_new <- c("region", "country", "demand_2021", "demand_2021_sd", "demand_2050", "demand_2050_lo", "demand_2050_hi", "growth_avg", "growth_lo", "growth_hi")

# Format data
data <- data_orig %>% 
  # Rename
  setNames(colnames_new) %>% 
  # Fill region
  fill(region, .direction="down") %>% 
  # Fix region
  mutate(region=recode(region, "AIS"="Atlantic & Indian\nOceans")) %>% 
  # Remove Timor-Leste
  filter(country!="Timor-Leste") %>% 
  # Arrange
  arrange(region, growth_avg) %>% 
  mutate(country=factor(country, levels=country))


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"))

# Plot data
g <- ggplot(data, aes(x=growth_avg, y=country, fill=region)) +
  facet_grid(region~., space="free_y", scales = "free_y") +
  # geom_bar(stat="identity", alpha=0.5) +
  # Error bars
  geom_point(mapping=aes(color=region)) + 
  geom_errorbar(mapping=aes(y=country, xmin=growth_lo, xmax=growth_hi, color=region), width=0) +
  # Reference line
  geom_vline(xintercept = 0, linetype="solid") +
  # Labels
  labs(x="Percent change in seafood demand\nfrom 2021 to 2050", y="") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position="none")
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig2_2050_projected_growth_in_demand.png"), 
       width=6.5, height=4.5, units="in", dpi=600)

