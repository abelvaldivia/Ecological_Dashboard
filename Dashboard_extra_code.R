## MAR and OLD IND Data


#mar_master <- read.csv("https://query.data.world/s/fizkuqu5f6wmsz3thakhf6mqjff246", 
#                    header=TRUE, stringsAsFactors=FALSE)
#  mar1 <- droplevels(subset(mar_master, Zone != "SPAG"))
#    colnames(mar1)[colnames(mar1) == "T"] <- "TF"
#    mar1 <- droplevels(subset(mar1, Collector == "HRI"))
#    mar1 <- droplevels(subset(mar1, Reef.Zone =="Crest" | 
#                            Reef.Zone  == "Fore reef" |
#                            Reef.Zone == "Patch"))

#mar1$Zone <- recode_factor(mar1$Zone, 'Fished' = "Fished",
#                           'General Use Zone' = "Managed Access",
#                           'No Fishing Zone' = "Reserve")

### INDONESIA FISH DATA FROM DATA WORLD OLD DATA
#  IDN_fish <- read.csv("https://query.data.world/s/c5o766egxnxsac4j3yzyjqhtjvvgqf", 
#                    header=TRUE, stringsAsFactors=FALSE)

#  head(IDN_fish)

#Recode trophic groups
#    IDN_fish$trophic_group <- recode_factor(IDN_fish$trophic_group,
#                                        'benthic invertivore'= "Benthic invertivore",
#                                        'Benthic Invertivore' = "Benthic invertivore",
#                                        'carnivore'= "Carnivore",
#                                        'planktivore' = "Planktivore",
#                                        'omnivore' = "Omnivore",
#                                        'herbivore' = "Herbivore",
#                                        'coralivore' = "Coralivore",
#                                        'detritivore' = "Detritivore")
#Order Management zones 
#  IDN_fish$inside_or_outside_NTZ <- factor(IDN_fish$inside_or_outside_NTZ, 
#                                      levels = c( "Outside","TURF","Inside"))

#  IDN_fish$count_ha <- as.numeric (IDN_fish$count_ha)

#Recode Management zones
#  IDN_fish$inside_or_outside_NTZ <- recode_factor(IDN_fish$inside_or_outside_NTZ,
#                                              'Outside' = "Outside Reserve",
#                                               'TURF' = "Outside Reserve",
#                                               'Inside' = "Inside Reserve")

#Exclude years with low survey effort
#  IDN_fish2 <- subset(IDN_fish, Year == 2015 | Year == 2016 | Year == 2017)
#    dim(IDN_fish2)
#    head(IDN_fish2)

## Calculate lenght count based on transect
#  IDN_fish_sum <- aggregate(count_ha ~ Year + site_code + inside_or_outside_NTZ + 
#                           transect_id + species_scientific_name, 
#FUN = sum, data = IDN_fish2, na.rm= TRUE)
dim(IDN_fish_sum)

IDN_fish_count <- aggregate(count_ha ~ Year + site_code + inside_or_outside_NTZ,
                            FUN = length, data = IDN_fish_sum, na.action= na.pass)

### Calculate biomass per Site and year

IDN_fish_biomass_species <- aggregate(biomass_kg_ha ~ country + Year + site_code + inside_or_outside_NTZ +
                                        transect_id + species_scientific_name, 
                                      FUN = sum, data = IDN_fish2, na.action= na.pass)
dim(IDN_fish_biomass_species)

IDN_fish_biomass_total <- aggregate(biomass_kg_ha ~ country + Year + site_code + inside_or_outside_NTZ +
                                      transect_id , 
                                    FUN = sum, data = IDN_fish_biomass_species, na.action= na.pass)


### INDOPACIFC BENTHIC OLD DATA FROM DATAWORLD
IDN_benthic <- read.csv("https://query.data.world/s/btpngkpepebvtumxm72zbeaehg3a3j", 
                        header=TRUE, stringsAsFactors=TRUE)
dim(IDN_benthic)
names(IDN_benthic)

#Order management level
IDN_benthic$inside_or_outside_NTZ <- factor(IDN_benthic$inside_or_outside_NTZ, 
                                            levels = c( "outside","TURF","inside"))
IDN_benthic$inside_or_outside_NTZ <- recode_factor(IDN_benthic$inside_or_outside_NTZ,
                                                   'outside' = "Outside Reserve",
                                                   'TURF' = "Outside Reserve",
                                                   'inside' = "Inside Reserve")

### Load Benthic data from Philippines
PHL_benthic <- read.csv("https://query.data.world/s/bnbbk5fsfl2biqza2y6ibbr2vixksx",
                        header=TRUE, stringsAsFactors=TRUE)
dim(PHL_benthic)
names(PHL_benthic)

#Order management level
PHL_benthic$inside_or_outside_NTZ <- factor(PHL_benthic$inside_or_outside_NTZ, 
                                            levels = c( "outside","TURF","inside"))
PHL_benthic$inside_or_outside_NTZ <- recode_factor(PHL_benthic$inside_or_outside_NTZ,
                                                   'outside' = "Outside Reserve",
                                                   'TURF' = "Managed Access",
                                                   'inside' = "Inside Reserve")
levels(factor(PHL_benthic$inside_or_outside_NTZ))

### Load fishe Data from Philippines
#PHL_fish <- read.csv("https://query.data.world/s/3452agl3owus3u3zv7zej55cckqtjc", 
#                  header=TRUE, stringsAsFactors=FALSE)

#PHL_fish$inside_or_outside_NTZ <- factor(PHL_fish$inside_or_outside_NTZ, 
#                                       levels = c( "Outside","TURF","Inside"))

#PHL_fish$inside_or_outside_NTZ <- recode_factor(PHL_fish$inside_or_outside_NTZ,
#                                              'Outside' = "Outside Reserve",
#                                             'TURF' = "Managed Access",
#                                             'Inside' = "Inside Reserve")
#dim(PHL_fish)
# names(PHL_fish)
#levels(as.factor(PHL_fish$Year))

## Calculate lenght count based on transect
# PHL_fish_sum <- aggregate(count_ha ~ Year + site_code + inside_or_outside_NTZ + 
#                         transect_id + species_scientific_name, 
#                       FUN = length, data = PHL_fish, na.action= na.pass)
#dim(PHL_fish_sum)

#PHL_fish_count <- aggregate(count_ha ~ Year + site_code + inside_or_outside_NTZ,
#                          FUN = length, data = PHL_fish_sum, na.action= na.pass)





## Creaate plots panel for Welcome main page base on 6 Ecological Metrics
## Fish Biomass

#png("./Eco_MainPanel.png", width = 1800, height = 1800,   units="px", pointsize=10, res=200)    
p1 <- ggplot(mar1, aes(Year, TF/10)) + theme_bw()+ facet_wrap(~Zone, ncol=1) +
  geom_smooth(aes(fill=Zone), color="grey20", lwd= 0.6, method="glm", formula = y ~ poly(x, 3), alpha=0.4)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
        axis.title.y = element_text(size=12),
        axis.text  = element_text(size=12),
        legend.position = "none", #c(0.55,0.15), legend.direction = "vertical",
        legend.title = element_blank())+
  scale_fill_discrete(labels = c(" Fished   ", " Managed Access   ", " Reserve"))+
  ggtitle("Fish Biomass Trends") + xlab("Years") + ylab("biomass (kg/ha)")

#Fish Size Structure
p2 <- ggplot(subset(IDN_fish,  species_scientific_name == "Caesio cuning"), 
             aes(length_cm)) + theme_bw()+
  geom_density(aes(fill=inside_or_outside_NTZ), alpha=0.4, adjust =2)+
  #geom_histogram(aes(fill=inside_or_outside_NTZ), alpha=0.4, stat ="bin", binwidth = 2)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
        axis.title.y = element_text(size=12),
        axis.text  = element_text(size=12),
        legend.position = c(0.65,0.85), legend.direction = "vertical",
        legend.title = element_blank())+ 
  scale_fill_discrete(labels = c("  Fished", "  Managed Access","  Reserve"))+
  ggtitle("Fish Size Structure") + xlab("Length (cm)")+  ylab("Density") + #scale_x_log10()+
  geom_vline(xintercept = c(10,28,40), col=c("red", "darkgreen", "blue"), lwd=0.2)+
  geom_text(aes(x=17, y=-0.002), label="L-mat", col="grey40", size =3)+
  geom_text(aes(x=35, y=-0.002), label="L-opt", col="grey40", size =3)+
  geom_text(aes(x=45, y=-0.002), label="L-inf", col="grey40", size=3)
p2

## Fish Diversity
p3 <- ggplot(subset(PHL_fish_count, inside_or_outside_NTZ != "Managed Access"), 
             aes(Year, count_ha)) + theme_bw()+
  geom_smooth(aes(col=inside_or_outside_NTZ), lwd= 0.6, alpha=0.4, method="loess", span=0.9, se=F)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
        axis.title.y = element_text(size=12),
        axis.text  = element_text(size=12),
        legend.position = c(0.25,0.9), legend.direction = "vertical",
        legend.title = element_blank())+
  scale_fill_discrete(labels = c(" Fished   ", " Managed Access   ", " Reserve"))+
  # scale_y_continuous(breaks= c(0,100,200,300,400,500))+
  ggtitle("Fish Diversity") + xlab("Years") + ylab("Number of Species")

### Coral Cover

p4 <- ggplot(subset(IDN_benthic, Year == 2012 | Year == 2014 | Year == 2015 | Year == 2017),
             aes(as.factor(Year), percent_live_coral_cover)) + theme_bw()+ 
  facet_wrap(~inside_or_outside_NTZ, ncol=1)+
  geom_boxplot(aes(fill=inside_or_outside_NTZ), alpha = 0.6)+
  #geom_smooth(aes(fill=inside_or_outside_NTZ), color="grey20", lwd= 0.6, method="loess", alpha=0.4)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
        axis.title.y = element_text(size=12),
        axis.text  = element_text(size=12),
        legend.position = "none",
        legend.title = element_blank())+
  #scale_fill_brewer(labels = c("Fished", "Managed Access", " Reserve"), palette=1 )+
  #scale_y_continuous(breaks = c(0,20,40,60), limits=c(0,60))+
  ggtitle("Coral Cover") + xlab("Years") + ylab("Coral Cover (%)")

### Macroalgae cover
p5 <-  ggplot(subset(IDN_benthic, Year == 2012 | Year == 2014 | Year == 2015 | Year == 2017),
              aes(as.factor(Year), percent_algae_cover)) + theme_bw()+ 
  facet_wrap(~inside_or_outside_NTZ, ncol=1)+
  geom_boxplot(aes(fill =inside_or_outside_NTZ), alpha = 0.6)+
  #geom_jitter(aes(y=percent_algae_cover), alpha=0.01, width=0.1, height = 0.1)+
  #geom_smooth(aes(fill=inside_or_outside_NTZ), color="grey20", lwd= 0.6, method="loess", alpha=0.4)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
        axis.title.y = element_text(size=12),
        axis.text  = element_text(size=12),
        legend.position = "none",
        legend.title = element_blank())+
  #scale_fill_discrete(labels = c(" Fished   ", " Managed Access   ", " Reserve"))+
  #scale_y_continuous(breaks = c(0,20,40,50), limits=c(0,50))+
  ggtitle("Macroalgae Cover") + xlab("Years") + ylab("Macroalgae Cover (%)")
p5

### Habitat Diversity
habitat <- data.frame (year = rep(c(2012,2014,2015,2017), each = 99),
                       management = rep(c("Fished", "Managed Access", "Reserve"), each = 33, times = 4),
                       richness = c(round(rnorm(132, mean=110, sd = 8)),
                                    round(rnorm(132, mean= 137, sd= 10)),
                                    round(rnorm(132, mean= 152, sd= 12))))

p6 <-  ggplot(habitat, aes(factor(year), richness)) + theme_bw()+ 
  facet_wrap(~management, ncol=1)+
  geom_violin(aes(fill=management), lwd=0.4 , alpha=0.4, trim=F, scale = "area", draw_quantiles = c(0.5))+
  geom_jitter(aes(col=management), lwd= 0.6, alpha=0.8, width = 0.1, height = 0.1) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
        axis.title.y = element_text(size=12),
        axis.text  = element_text(size=12),
        legend.position = "none",
        legend.title = element_blank())+
  scale_fill_discrete(labels = c(" Fished   ", " Managed Access   ", " Reserve"))+
  #scale_y_continuous(breaks = c(0,20,40), limits=c(0,40))+
  ggtitle("Habitat Diversity") + xlab("Years") + ylab("Number of Species")
p6

grid.arrange(p1, p3, p4, p5, p6, nrow = 2, ncol=3) #p2, p3, p4, p5, p6,

#dev.off()


## Strip Lmax for fish species from Fish Base ###
#Species Code
library(rvest);library(readr); library(magicfor)

poplw <- read.csv("C:/Users/avaldivia/Downloads/species_lw_final.csv")
names(poplw)
dim(poplw)

lmax_values <- data.frame(speccode = 0, lmax= 0) 

for(i in poplw$speccode) {
  pg <- xml2::read_html(paste0("https://www.fishbase.de/summary/", i))
  lmax_values <- rbind(lmax_values,
                       print(c(i, parse_number((unique(str_match_all(html_text(html_nodes(pg, "body")),
                                                                     "(Max length : [[:digit:]]+\\.*[[:digit:]]*)")[[1]][,2]))))))
}

poplw_updated <- cbind(poplw, lmax_values[-1,])
head(poplw_updated)

write.csv(poplw_updated, "C:/Users/avaldivia/Dropbox/Collaborations/RARE/Fish Forever M&E/ME Framework and Metrics/Eco_DataPortal/species_lw_final.csv",
          row.names = FALSE)


