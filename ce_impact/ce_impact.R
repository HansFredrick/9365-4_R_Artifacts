# Team 4: R Programming
# ARAGONA, BALOGO, BAUTISTA, CAYTON, MORALES, PALACIO

# =================================SETUP=======================================
# install needed packages
install.packages("tidyverse")
install.packages("dplyr") 
install.packages("ggplot2")

# load installed packages
library(tidyverse)
library(dplyr)
library(ggplot2)

#change this to you directory

setwd('C:\\Users\\CODE CLASSES\\R\\CLONE for FINALS\\9365-4_R_Artifacts')  

# ===============================DATASETS======================================
# load and clean datasets
# carbon emissions dataset
carbon_data <- read_csv("subnational_carbon_data.csv") %>%
  rename(threshold = 'umd_tree_cover_density_2000__threshold',
         ce2010 = 'gfw_forest_carbon_gross_emissions_2010__Mg_CO2e',
         ce2011 = 'gfw_forest_carbon_gross_emissions_2011__Mg_CO2e',
         ce2012 = 'gfw_forest_carbon_gross_emissions_2012__Mg_CO2e',
         ce2013 = 'gfw_forest_carbon_gross_emissions_2013__Mg_CO2e',
         ce2014 = 'gfw_forest_carbon_gross_emissions_2014__Mg_CO2e',
         ce2015 = 'gfw_forest_carbon_gross_emissions_2015__Mg_CO2e',
         ce2016 = 'gfw_forest_carbon_gross_emissions_2016__Mg_CO2e',
         ce2017 = 'gfw_forest_carbon_gross_emissions_2017__Mg_CO2e',
         ce2018 = 'gfw_forest_carbon_gross_emissions_2018__Mg_CO2e',
         ce2019 = 'gfw_forest_carbon_gross_emissions_2019__Mg_CO2e',
         ce2020 = 'gfw_forest_carbon_gross_emissions_2020__Mg_CO2e',
         subnational = 'subnational1') %>%
  select(country, threshold, subnational, ce2010:ce2020) %>%
  filter(country == "Philippines", threshold >= 30, subnational == "Benguet")

# tree cover loss dataset
tree_cover_data <- read_csv("subnational_tree_cover_loss.csv") %>%
  rename(tc2010 = 'tc_loss_ha_2010',
         tc2011 = 'tc_loss_ha_2011',
         tc2012 = 'tc_loss_ha_2012',
         tc2013 = 'tc_loss_ha_2013',
         tc2014 = 'tc_loss_ha_2014',
         tc2015 = 'tc_loss_ha_2015',
         tc2016 = 'tc_loss_ha_2016',
         tc2017 = 'tc_loss_ha_2017',
         tc2018 = 'tc_loss_ha_2018',
         tc2019 = 'tc_loss_ha_2019',
         tc2020 = 'tc_loss_ha_2020',
         subnational = 'subnational1',
         extent = 'extent_2010_ha') %>%
  select(country, threshold, subnational, extent, tc2010:tc2020) %>%
  filter(country == "Philippines", threshold >= 30, subnational == "Benguet")

# combine datasets into one dataset to use for analysis, visualization, etc.
tc_and_ce_data <- carbon_data %>%
  right_join(tree_cover_data, by = "threshold") %>%
  rename(country = 'country.x',
         subnational = 'subnational.x') %>%
  select(-country.y, -subnational.y)

# ================================SAVING======================================
# save a dataset into a csv
write_csv(tc_and_ce_data, "final_data.csv")

# =============================VISUALIZATION======================================
# scatter plot for year 2010
data_2010 <- tc_and_ce_data %>%
  select(country, threshold, subnational, extent, ce2010, tc2010)

# create the scatter plot for 2010 with thresholds indicated by color and extent as point size
ggplot(data_2010, aes(x = ce2010, y = tc2010, color = factor(threshold), size = extent)) +
  geom_point(alpha = 0.7) + 
  scale_color_manual(
    values = c("30" = "#CC0000", "50" = "#FF6666", "75" = "red"),
    name = "Threshold",
    breaks = c("30", "50", "75"),
    labels = c("30", "50", "75")
  ) +
  scale_size_continuous(
    range = c(3, 10),
    name = "Extent"
  ) +
  labs(
    title = "Carbon Emissions vs Tree Cover Loss (2010)",
    x = "Carbon Emissions (Mg CO2e) in 2010",
    y = "Tree Cover Loss (ha) in 2010",
    color = "Threshold",
    size = "Extent (ha)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# pivot the carbon data to long format
carbon_long <- carbon_data %>%
  pivot_longer(
    cols = starts_with("ce"),
    names_to = "year",
    names_prefix = "ce",
    names_transform = list(year = as.integer),
    values_to = "ce"
  )

# calculate the average carbon emissions per year
carbon_avg_per_year <- carbon_long %>%
  group_by(year) %>%
  summarise(avg_ce = mean(ce, na.rm = TRUE))

# create the line plot
ggplot(carbon_avg_per_year, aes(x = year, y = avg_ce, group = 1)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 2) +
  scale_x_continuous(breaks = 2010:2020,  # set year breaks
                     labels = as.character(2010:2020)) +
  labs(
    title = "Average Carbon Emissions from 2010 to 2020",
    x = "Year",
    y = "Average Carbon Emissions (Mg CO2e)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.ticks = element_line(size = 0.5),
    axis.ticks.length = unit(0.2, "cm")
  )

