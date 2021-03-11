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
  
  mean_size <- function (x,y) { sum(x*y, na.rm = TRUE)/sum(x, na.rm = TRUE) }
  
  
  
  ##Sort species species with the most data 
  Common_species <- data.frame(Frequency=sort(summary(factor(fish.surveys$species)), decreasing = TRUE),
                             stringsAsFactors = TRUE)
  ## Add row names as column
  Common_species <- tibble::rownames_to_column(Common_species, "species")


### FISH DIVERSITY ####
  ### Determine unique number of species per location name
  count_unique <- function(x) { length(unique (x))}

  fish.diversity <- aggregate(species ~  country  + level1_name + level2_name + ma_name +
                            location_name + location_status,
                            data = fish.surveys, FUN = count_unique)
  
 

### HABITAT COVER ####
  #Load data from dataworld
  benthic.surveys <- read.csv("https://query.data.world/s/xk5fxswlaztskxvg6s47htdybjds3h", 
                                  header=TRUE, stringsAsFactors=TRUE, encoding = "UTF-8")
  benthic.surveys$location_status <-  benthic.surveys$locationstatus
  benthic.surveys$location_name <- benthic.surveys$locationname
  
  
  benthic.surveys$location_status <- recode_factor(benthic.surveys$location_status, "ma" = "Outside \nReserve",
                                               "reserve"  = "Inside \nReserve")
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

##### VIEW MAP ##### 
  
  #Download footprintdata from dataworld
  foot_print <- read.csv("https://query.data.world/s/hpot5etpvflf4pmhrljcsensi226bf", 
                         header=TRUE, stringsAsFactors=TRUE, encoding = "UTF-8")
  #Download Coral Reef layer
  #idn_coralreefs <- geojsonio::geojson_read("./adm/IDN_coral_reefs.geojson")
  ### Download GeoJSON for Dashboard ####
  #idn_dist <- rgdal::readOGR("./adm/SE_Sulawesi_Districts_Boundary_Ar.json")
  #idn_dist <- geojsonio::geojson_read("./adm/SE_Sulawesi_Districts_Boundary_Ar.json", what = "sp")
  #idn_adm0 <- readOGR("./adm/IDN_adm0.shp")
  #idn_adm1 <- readOGR("./adm/IDN_adm1.shp")
  #idn_adm2 <- readOGR("./adm/IDN_adm2.shp")
  ses_aoi <- readOGR("./adm/SE_Sulawesi_AoI_Ar_2019_02.shp")
  
  portal_map <- aggregate(cbind(lat,lon) ~ country + level1_name + level2_name +  ma_name + location_name,
                                            data = fish.surveys, FUN = mean)
  

  ### FUNCTIONS FOR SERVER ###
  ## Function for summary tables #####
  summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                        conf.interval=.95, .drop=TRUE) {
    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
      if (na.rm) sum(!is.na(x))
      else       length(x)
    }
    # This does the summary. For each group's data frame, return a vector with N, mean, and sd
    datac <- plyr::ddply(data, groupvars, .drop=.drop,
                         .fun = function(xx, col) {
                           c(N    = length2(xx[[col]], na.rm=na.rm),
                             mean = mean   (xx[[col]], na.rm=na.rm),
                             SD   = sd     (xx[[col]], na.rm=na.rm)
                           )
                         },
                         measurevar
    )

    datac <- plyr::rename(datac, c("mean" = measurevar)) # Rename the "mean" column 
    datac$SE <- datac$SD / sqrt(datac$N)  # Calculate standard error of the mean
    
    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$CI <- datac$SE * ciMult
    
    return(datac)
  }
  
  ## Customized Theme for ggplots for the dashboard
  theme_rare <- theme_bw() +
        theme(panel.grid = element_blank(),
          plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
          axis.title.x = element_text(size=14),
          axis.title.y = element_text(size=14),
          axis.text  = element_text(size=14),
          strip.text = element_text(size=14),
          legend.position = "none",
          legend.title = element_blank())
  
  theme_rare_sizeclass <- theme_bw()+
        theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size=14),
          panel.grid = element_blank(),
          strip.text = element_text(size = 14),
          plot.title = element_text(hjust=0.5, face = 'bold.italic', size = 14),
          axis.title.x = element_text(size=14),
          axis.title.y = element_text(size=14),
          axis.text  = element_text(size=14))
  
  
  
  
  