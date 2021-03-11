###Load and clean data ####

### DOWNLOAD  global footprint data for each counity
#footprint <- read.csv("https://query.data.world/s/3sxh3ac642ttfue4d7puzmeufloe36",
#                 header=TRUE, stringsAsFactors=TRUE)
#write.csv (footprint, "footprint.csv", row.names=F)

#footprint <- read.csv("footprint.csv", header=TRUE, stringsAsFactors=TRUE)



#### INDOPACIFIC NEW FISH DATA ####

## Read fish data from DataWorld
#fish_master <- read.csv("https://query.data.world/s/yrhphplkfx6codouglzzgmmety3wm3", 
#                  header=TRUE, stringsAsFactors=FALSE)
#write.csv(fish_master, "fish_master.csv", row.names=F)

#fish_master <- read.csv("fish_master.csv", header = TRUE, stringsAsFactors = FALSE)

#fish_master <- read.csv("https://download.data.world/s/id5nsn6a7sangkdotklyoaurlemrze",
#                         header=TRUE, stringsAsFactors =FALSE)

#write.csv(fish_master1, "fish_master1.csv", row.names=F)
#fish_master <- read.csv("fish_master1.csv", header=TRUE, stringsAsFactors =FALSE)

## Convert sites names to Sentence case and 
#fish_master$community <- toTitleCase(tolower((fish.survey$community)))

#  levels(fish.surveys$community)

## Fix some sites with misspelling
# fish_master$community <- recode_factor(fish_master$community, "Siotapina" = "Siontapina",
#                                                               "Kapuntori" = "Kapontori")
## Recode country names
#fish_master$country <- recode_factor(fish_master$country, "IDN" = "Indonesia", 
#                                       "HND" = "Honduras", "PHL" = "Philippines")

## Subset fish data for Indonesia and Honduras
#IDN_fish_nofp <- droplevels(subset(fish_master, country == "Indonesia"))
#HND_fish_nofp <- droplevels(subset(fish_master, country == "Honduras"))

### Merge data with foot printdata to extract administrative divisions
#footprint_IDN <- droplevels(subset(footprint, country == "Indonesia"))
#footprint_HND <- droplevels(subset(footprint, country == "Honduras"))

#IDN_fish <- merge(x=IDN_fish_nofp, y= unique(footprint_IDN [,c("level1_name", "level2_name", "level3_name")]), 
#                    by.x = "community", by.y = "level3_name", all.x=TRUE )
#IDN_fish$level3_name <- IDN_fish$community
#dim(IDN_fish)

#HND_fish <- merge(x=HND_fish_nofp, y= unique(footprint_HND [,c("level1_name", "level2_name", 
#                                                            "level3_name", "level4_name")]), 
#                   by.x = "community", by.y = "level4_name", all.x=TRUE, all.y= FALSE)

## Calcualte mid range length for Honduras
#HND_fish$sizeclass <- factor(HND_fish$sizeclass, levels = c("0-5","6-10","11-20","21-30", "31-40", "41-50"))

### Recode Size range to midsize
#HND_fish$length <- as.numeric(as.character(recode_factor(HND_fish$sizeclass, 
#                                                         "0-5"="2.5","6-10"="8", "11-20"="15.5", "21-30"="25.5","31-40"="35.5","41-50"="45.5")))

### Combine both datasets
#fish.surveys <- rbind(IDN_fish, HND_fish)

### Calculate fish density per transect area in Hectares
#fish.surveys <- within(fish.surveys, density.ind.kg <- fishcount/transectarea*10000) #ind/ha

### Calculate biomass per hectare
#fish.surveys <-  within(fish.surveys, biomass.kg.ha <- a*(length)^b*density.ind.kg/1000) # Kg/ha




## MAR DATA (HRI)
mar_master <- read.csv("https://query.data.world/s/fizkuqu5f6wmsz3thakhf6mqjff246",
                       header=TRUE, stringsAsFactors=FALSE)
mar1 <- droplevels(subset(mar_master, Zone != "SPAG"))

#Rename T (total biomass) to Biomass
colnames(mar1)[colnames(mar1) == "T"] <- "TF"
mar1 <- droplevels(subset(mar1, Collector == "HRI"))
mar1 <- droplevels(subset(mar1, Reef.Zone =="Crest" | 
                            Reef.Zone  == "Fore reef" |
                            Reef.Zone == "Patch"))

mar1$Zone <- recode_factor(mar1$Zone, 'Fished' = "Fished",
                           'General Use Zone' = "Managed Access",
                           'No Fishing Zone' = "Reserve")

### INDONESIA FISH DATA FROM DATA WORLD FF1.0
IDN_fish <- read.csv("https://query.data.world/s/c5o766egxnxsac4j3yzyjqhtjvvgqf", 
                     header=TRUE, stringsAsFactors=FALSE)
#Recode trophic groups
IDN_fish$trophic_group <- recode_factor(IDN_fish$trophic_group,
                                        'benthic invertivore'= "Benthic invertivore",
                                        'Benthic Invertivore' = "Benthic invertivore",
                                        'carnivore'= "Carnivore",
                                        'planktivore' = "Planktivore",
                                        'omnivore' = "Omnivore",
                                        'herbivore' = "Herbivore",
                                        'coralivore' = "Coralivore",
                                        'detritivore' = "Detritivore")
#Order Management zones
IDN_fish$inside_or_outside_NTZ <- factor(IDN_fish$inside_or_outside_NTZ, 
                                         levels = c( "Outside","TURF","Inside"))
#Recode Management zones
IDN_fish$inside_or_outside_NTZ <- recode_factor(IDN_fish$inside_or_outside_NTZ,
                                                'Outside' = "Fished",
                                                'TURF' = "Managed Access",
                                                'Inside' = "Reserve")
#Exclude very sites that are not associated with management zones
IDN_fish1 <- subset(IDN_fish, inside_or_outside_NTZ != "NA")

#Exclude years with low survey effort
IDN_fish2 <- subset(IDN_fish1, Year==2015|Year==2016|Year==2017)


## Calculate lenght count based on transect
IDN_fish_sum <- aggregate(count_ha ~ country + Year + site_code + inside_or_outside_NTZ + 
                            transect_id + species_scientific_name, 
                          FUN = length, data = IDN_fish2, na.action= na.pass)

IDN_fish_count <- aggregate(count_ha ~ country + Year + site_code + inside_or_outside_NTZ,
                            FUN = length, data = IDN_fish_sum, na.action= na.pass)

### Calculate biomass per Site and year

IDN_fish_biomass_species <- aggregate(biomass_kg_ha ~ country + Year + site_code + inside_or_outside_NTZ +
                                        transect_id + species_scientific_name, 
                                      FUN = sum, data = IDN_fish2, na.action= na.pass)

IDN_fish_biomass_total <- aggregate(biomass_kg_ha ~ country + Year + site_code + inside_or_outside_NTZ +
                                      transect_id , 
                                    FUN = sum, data = IDN_fish_biomass_species, na.action= na.pass)


### INDOPACIFC BENTHIC DATA FROM DATAWORLD
IDN_benthic <- read.csv("https://query.data.world/s/btpngkpepebvtumxm72zbeaehg3a3j", 
                        header=TRUE, stringsAsFactors=FALSE)
dim(IDN_benthic)
names(IDN_benthic)

#Order management level
IDN_benthic$inside_or_outside_NTZ <- factor(IDN_benthic$inside_or_outside_NTZ, 
                                            levels = c( "outside","TURF","inside"))
IDN_benthic$inside_or_outside_NTZ <- recode_factor(IDN_benthic$inside_or_outside_NTZ,
                                                   'outside' = "Fished",
                                                   'TURF' = "Managed Access",
                                                   'inside' = "Reserve")

### Load Benthic data from Philippines
PHL_benthic <- read.csv("https://query.data.world/s/bnbbk5fsfl2biqza2y6ibbr2vixksx",
                        header=TRUE, stringsAsFactors=FALSE)
dim(PHL_benthic)
names(PHL_benthic)

#Order management level
PHL_benthic$inside_or_outside_NTZ <- factor(PHL_benthic$inside_or_outside_NTZ, 
                                            levels = c( "outside","TURF","inside"))
PHL_benthic$inside_or_outside_NTZ <- recode_factor(PHL_benthic$inside_or_outside_NTZ,
                                                   'outside' = "Fished",
                                                   'TURF' = "Managed Access",
                                                   'inside' = "Reserve")
levels(PHL_benthic$inside_or_outside_NTZ)

### Load fishe Data from Philippines
PHL_fish <- read.csv("https://query.data.world/s/3452agl3owus3u3zv7zej55cckqtjc", 
                     header=TRUE, stringsAsFactors=FALSE)

PHL_fish$inside_or_outside_NTZ <- factor(PHL_fish$inside_or_outside_NTZ, 
                                         levels = c( "Outside","TURF","Inside"))

PHL_fish$inside_or_outside_NTZ <- recode_factor(PHL_fish$inside_or_outside_NTZ,
                                                'Outside' = "Fished",
                                                'TURF' = "Managed Access",
                                                'Inside' = "Reserve")

## Calculate lenght count based on transect
PHL_fish_sum <- aggregate(count_ha ~ Year + site_code + inside_or_outside_NTZ + 
                            transect_id + species_scientific_name, 
                          FUN = length, data = PHL_fish, na.action= na.pass)

PHL_fish_count <- aggregate(count_ha ~ Year + site_code + inside_or_outside_NTZ,
                            FUN = length, data = PHL_fish_sum, na.action= na.pass)


## Strip Lmax from Fish Base ###
#Species Code
#library(str_match)
#for (i in c(3502:3505)) {
# pg <- read_html(paste0("https://www.fishbase.de/summary/", i))

# print(unique(str_match_all(html_text(html_nodes(pg, "body")),
#                      "(Max length : [[:alnum:]]+)")[[1]][,2])) }