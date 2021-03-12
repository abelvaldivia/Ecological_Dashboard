## Read and process raw data

library (data.table)
library (dplyr)
library (readr)

## Load fish surveys data ####
  fish.surveys <- data.table::fread("https://query.data.world/s/beo5xpafq24c45b4a6bdzvbgxa3c5l", 
                                   header= TRUE, stringsAsFactors = TRUE, encoding = "UTF-8")
  names(fish.surveys)
  #Recode location status
  fish.surveys$location_status <- dplyr::recode_factor(fish.surveys$location_status, 
                                              "ma" = "Outside \nReserve",
                                              "reserve"  = "Inside \nReserve",
                                              "Outside" = "Outside \nReserve",
                                              "outside" = "Outside \nReserve")
  ## Remove test sites
  fish.surveys <- fish.surveys %>%
                     filter(location_name != "Test" &
                            location_name != "") %>%
                        droplevels()
  
  ### Recode size range 
  fish.surveys$sizeclass <- recode_factor(
    factor(fish.surveys$size_class),
      "<10" = "0-10","0-5" = "0-5", "0-10"="0-10", "6-10"= "6-10", "10 - 20" = "11-20",
      "10-20" = "11-20", "20-30"="21-30", "20 - 30" = "21-30", "30-40" = "31-40",
      "30 - 40" = "31-40", "40 - 50" = "41-50", "50 - 60" = "51-60",
      "40-50" = "41-50", "50-60" = "51-60", "60 - 70" = "61-70",
      "70 - 80" = "71-80", "80 - 90" ="81-90", "90 - 100" = "91-100", 
      "110 - 120" = "111-120", "120 - 130" = "121-130", "130 - 140" = "131-140", 
      "140 - 150" = "141-150","150 - 160" = "151-160", "160 - 170" = "161-170", 
      "170 - 180" = "171-180", "180 - 190" = "181-190", "20-Oct" = "11-20")
  
  
  readr::write_rds(tibble(fish.surveys), "data/fish.surveys.rds")
  
  #Load Benthic data
  benthic.surveys <- data.table::fread("https://query.data.world/s/xk5fxswlaztskxvg6s47htdybjds3h", 
                              header=TRUE, 
                              stringsAsFactors=TRUE, 
                              encoding = "UTF-8")
  
  benthic.surveys$location_status <-  benthic.surveys$locationstatus
  benthic.surveys$location_name <- benthic.surveys$locationname
  
  benthic.surveys$location_status <- 
    dplyr::recode_factor(benthic.surveys$location_status, 
                         "ma" = "Outside \nReserve",
                         "reserve"  = "Inside \nReserve",
                         "Outside" = "Outside \nReserve",
                         "outside" = "Outside \nReserve")
  
  readr::write_rds(tibble(benthic.surveys), "data/benthic.surveys.rds")

  