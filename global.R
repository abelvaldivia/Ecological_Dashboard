## RARE Ecological Data Dashboard ##
## By Abel Valdivia, PhD

#Load required libraries
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)
library(plyr)
library(dplyr)
library(readr)
library(ggplot2)
library(sf)
#library(ggvis)
library(grid)
library(gridExtra)
library(lme4)
library(car)
library(tools)
library(Hmisc)
library(tidyr)
library(scales)
library(leaflet)
library(leafem)
library(rgdal)
library(rintrojs)

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

source("R/read_data.R")
source("R/utils.R")


## FISH BIOMASS ####
  ##Agregate fish biomass data transect
    fish.biomass <- aggregate(biomass_kg_ha ~  country + level1_name + level2_name + ma_name +
                             location_name + location_status  + transect_no,
                          data = fish.surveys, FUN = sum)
  ##Agregate fish biomass by family and data transect
    fish.biomass.family <- aggregate(biomass_kg_ha ~  country  + level1_name + level2_name + ma_name+
                              location_name + location_status  + transect_no + family,
                            data = fish.surveys, FUN = sum)
  ## Fish biomass per survey location
    fish.biomass.mean <- aggregate(biomass_kg_ha ~  country  + level1_name + level2_name + ma_name +
                              location_name + location_status,
                            data = fish.biomass, FUN = mean)
 

### FISH SIZE STRUCTURE ####

  ### Agregate Fish data transect for density
  ## by species
  fish.density <- aggregate(density_ind_ha ~  country  + level1_name + level2_name + ma_name+
                        location_name + location_status + transect_no,
                        data = fish.surveys, FUN = sum, na.rm = T)
    
  fish.density.family <- aggregate(density_ind_ha ~  country  + level1_name + level2_name + ma_name+
                             location_name + location_status  + transect_no + family,
                                   data = fish.surveys, FUN = sum, na.rm = T)
  
  fish.density.sizeclass <- aggregate(density_ind_ha ~  country  + level1_name + level2_name + ma_name+
                             location_name + location_status + length + transect_no + sizeclass,
                            data = fish.surveys, FUN = sum, na.rm = T)
  
  fish.density.species <- aggregate(density_ind_ha ~  country  + level1_name + level2_name + ma_name+
                              location_name + location_status  + length +
                                water_depth + transect_no + species + sizeclass + lmax,
                                  data = fish.surveys, FUN = sum, na.rm = T)  

  
  
  
  ##Sort species species with the most data 
  Common_species <- data.frame(Frequency=sort(summary(factor(fish.surveys$species)), decreasing = TRUE),
                             stringsAsFactors = TRUE)
  ## Add row names as column
  Common_species <- tibble::rownames_to_column(Common_species, "species")


### FISH DIVERSITY ####
 fish.diversity <- aggregate(species ~  country  + level1_name + level2_name + ma_name +
                            location_name + location_status,
                            data = fish.surveys, FUN = count_unique)
  
 

### HABITAT COVER ####
  ## Sumarize benthic data by transect and category
  benthic.cover <- aggregate(percentage ~  country + sitename + level1_name + level2_name + ma_name+
                              location_name + location_status +  category + transectno,
                             data = benthic.surveys, FUN = sum, na.action = na.omit)
  
  ##Sumarize benthic data by MA and category
  benthic.cover.avg <- aggregate(percentage ~  country + sitename + level1_name + level2_name + ma_name +
                                  location_name + location_status +  category,
                                 data = benthic.cover, FUN = mean, na.action = na.omit)
  
  
#### HABITAT DIVERSITY ####
  ## Count the number of attributes (species, genus, families, groups) when percentage is >0 and calculate average
  
  benthic.diversity <- aggregate(attribute ~  country + sitename + level1_name + level2_name + ma_name+
                                   location_name + location_status  + transectno,
                                 data = subset(benthic.surveys, percentage >0), FUN = count_unique, na.action = na.omit)


 
  

  
  
 
  
  
  
  