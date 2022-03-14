
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
data_orig <- readxl::read_excel(file.path(datadir, "Data_for_Figures_Climate_Smart_Small_Island_Fisheries_August2021.xlsx"), sheet=2, skip=2)


# Format data
################################################################################

# Column names
colnames(data_orig) 

# Format data
data <- data_orig %>% 
  # Rename
  rename("region"="...1",
         "country"="...2") %>% 
  # Gather
  gather(key="metric", value="value", 3:ncol(.)) %>% 
  # Fill
  fill(region, .direction="down") %>% 
  # Reduce to metric of interest
  filter(grepl("% surplus seafood projected for 2046-2050", metric)) %>% 
  # Eliminate duplicated 5% scenario - not sure what Lida did here
  filter(metric!="% surplus seafood projected for 2046-2050 if 5-yr Adaptation...23") %>% 
  # Add mgmt scenario
  mutate(mgmt=ifelse(grepl("5-yr", metric), "5-yr adaptation",
                     ifelse(grepl("10-yr", metric), "10-yr adaptation",
                            ifelse(grepl("20-yr", metric), "20-yr adaptation", "Full adaptation")))) %>% 
  # Remove full
  filter(mgmt!="Full adaptation") %>% 
  # Remove Timor-Leste
  filter(country!="Timor-Leste") %>% 
  # Code mgmt
  mutate(mgmt=factor(mgmt, levels=c("5-yr adaptation", "10-yr adaptation", "20-yr adaptation"))) %>% 
  # Fix region
  mutate(region=recode(region, "AIS"="Atlantic & Indian\nOceans")) %>% 
  # Arrange
  select(region, country, mgmt, value)

# Set plotting order
country_order <- data %>% 
  group_by(region, country) %>% 
  summarize(value=mean(value)) %>% 
  arrange(region, desc(value))

# Set plot order
data <- data %>% 
  mutate(country=factor(country, levels=country_order$country))


# Plot data
################################################################################

# Labels
labels <- tibble(region="Atlantic & Indian\nOceans",
                 country=rep("Comoros",2),
                 value=c(-175, 750),
                 label=c("Seafood\ndeficit", "Seafood\nsurplus"), 
                 hjust=c(0,1))

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"))

# Plot data
g <- ggplot(data, aes(x=value, y=country, fill=mgmt)) +
  facet_grid(region~., space="free_y", scales = "free_y") +
  geom_bar(stat="identity", position="dodge") +
  # Reference line
  geom_vline(xintercept = 0, linetype="solid") +
  # Plot text
  geom_text(data=labels, mapping=aes(x=value, y=country, label=label, hjust=hjust), inherit.aes = F, size=2, vjust=1) +
  # Labels
  labs(x="Percent difference in seafood\nsupply and demand in 2050", y="") +
  # Legend
  scale_fill_discrete(name="Fisheries management") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "bottom")
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig3_2050_projected_surplus_deficit.png"), 
       width=6.5, height=5.5, units="in", dpi=600)

