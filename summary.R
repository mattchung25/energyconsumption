library(dplyr)
library(ggplot2)
library(tidyverse)

building_df<- read.csv("2020_Building_Energy_Benchmarking.csv", stringsAsFactors = FALSE)

#building data
building_data <- building_df %>% 
  group_by(EPAPropertyType) %>% 
  summarise("average_energy_used" = mean(SiteEnergyUse.kBtu., na.rm = TRUE)) %>% 
  top_n(10)

#hospital data and 
hospital_energy <- building_data %>% 
  filter(EPAPropertyType == "Hospital (General Medical & Surgical)") %>% 
  pull(average_energy_used)
write.csv(building_data, file = "average_energy.csv")

#finding the total energy
total_energy <- building_df %>% 
  summarise("total_energy" = sum(SiteEnergyUse.kBtu., na.rm = TRUE)) %>% 
  pull(total_energy)
total_energy_h <- building_df %>% 
  filter(EPAPropertyType == "Hospital (General Medical & Surgical)") %>% 
  summarise("total_energy_hos" = sum(SiteEnergyUse.kBtu., na.rm = TRUE)) %>% 
  pull(total_energy_hos)
#how much hospital is out of the whole
percent_hos <- total_energy_h / total_energy

#energy relationship
energy_relationship <- building_df %>% 
  select(c(Electricity.kBtu., SteamUse.kBtu., NaturalGas.kBtu., 
           TotalGHGEmissions, EPAPropertyType))
green_energy <- energy_relationship %>% group_by(EPAPropertyType) %>% 
  summarise_all(mean, na.rm = TRUE)

  
write.csv(green_energy, file = "energy_relationship.csv")

#looking at the most energy used per building
electrcity_data <- energy_relationship %>% group_by(EPAPropertyType) %>% 
  summarise("average_electricity" = mean(Electricity.kBtu., na.rm = TRUE)) %>% 
  top_n(10)
steam_data <- energy_relationship %>% group_by(EPAPropertyType) %>% 
  summarise("average_steam" = mean(SteamUse.kBtu., na.rm = TRUE)) %>% 
  top_n(10)
natural_data <- energy_relationship %>% group_by(EPAPropertyType) %>% 
  summarise("average_natural" = mean(NaturalGas.kBtu., na.rm = TRUE)) %>% 
  top_n(10)
write.csv(c(electrcity_data, steam_data, natural_data), file = "energy_comparative.csv")
