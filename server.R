
#### ~ Create Shiny Server ~ ####
  
  shinyServer(function(input, output, session) {
  
  ### Create nested selection based on inputs
    
  #output$Density_plots <- renderUI({
    
  #  if (tabPanel == "Fish size structure") {
  
 #  radioButtons(inputId = "plot_type", strong("Plot type"),
  #               choices = c("Bar plots", "Density plots"), 
  #               selected = "Bar plots") 
  #  }
  #})
  
  ## Region/Province
  
  output$Subnational_Government <- renderUI({
    selectInput(inputId = "Subnational_Government", label= strong("Subnational Government"), 
                choices = as.vector(sort(unique(droplevels(subset(fish.surveys, 
                                       country %in% input$Country))$level1_name))),
                selected = "South East Sulawesi",
                multiple = FALSE, selectize = TRUE) #, options = list(`actions-box` = TRUE))
        })

  ## District/Administration
  output$Local_Government <- renderUI({
    selectInput(inputId = 'Local_Government', label= strong("Local Government"), #selected = "No Fishing Zone",
                choices = as.vector(sort(unique(droplevels(subset(fish.surveys, 
                                country %in% input$Country & 
                                   level1_name %in% input$Subnational_Government))$level2_name))),
                selected = as.vector(sort(unique(droplevels(subset(fish.surveys, 
                                country %in% input$Country & 
                                   level1_name %in% input$Subnational_Government))$level2_name))),
                multiple = TRUE, selectize = FALSE, size = 2) #, options = list(`actions-box` = TRUE))
         })
  
  ## Municipality/Subdistrict
  output$Managed_Access <- renderUI({
    selectInput(inputId = 'Managed_Access', label = strong("Managed Access"), #selected = "No Fishing Zone",
                choices = as.vector(sort(unique(droplevels(subset(fish.surveys, 
                              country %in% input$Country & 
                                 level1_name %in% input$Subnational_Government &
                                  level2_name %in% input$Local_Government))$ma_name))),
                selected = as.vector(sort(unique(droplevels(subset(fish.surveys, 
                                country %in% input$Country & 
                                   level1_name %in% input$Subnational_Government &
                                    level2_name %in% input$Local_Government))$ma_name))),
                          multiple = TRUE, selectize = FALSE, size =2) #, options = list(`actions-box` = TRUE))
          })
  
  ## Select Fish famlies, families and trophic groups within the selected data
  output$fish_family <- renderUI({ 
    selectInput(inputId = 'fish_family', label = strong("SELECT A FISH FAMILY"), 
               choices =  c("All fish families", as.vector(sort(unique(droplevels(subset(fish.surveys, 
                              country %in% input$Country & 
                                  level1_name %in% input$Subnational_Government &
                                      level2_name %in% input$Local_Government &
                                          ma_name %in% input$Managed_Access))$family)))),
                selected =  "All fish families",
                 multiple = FALSE, selectize = TRUE)
   })
  
  # Select fish species within each family
  output$fish_species <- renderUI({ 
    selectInput(inputId = 'fish_species', label = strong("SELECT A FISH SPECIES"), 
                choices =  c("All fish species", 
                             as.vector(sort(unique(droplevels(subset(fish.surveys, 
                                country %in% input$Country & 
                                   level1_name %in% input$Subnational_Government &
                                      level2_name %in% input$Local_Government &
                                        ma_name %in% input$Managed_Access &
                                          family %in% input$fish_family))$species)))),
                selected =  c("All fish species", 
                              as.vector(sort(unique(droplevels(subset(fish.surveys, 
                                country %in% input$Country & 
                                 level1_name %in% input$Subnational_Government &
                                   level2_name %in% input$Local_Government &
                                    ma_name %in% input$Managed_Access &
                                      family %in% input$fish_family))$species)))),
                     multiple = FALSE, selectize = TRUE)
  })
  

  output$habitat_category <- renderUI({
     selectInput(inputId = 'habitat_category', label = strong("SELECT A HABITAT CATEGORY"), 
         choices =  as.vector(sort(unique(droplevels(subset(benthic.surveys, 
                        country %in% input$Country & 
                           level1_name %in% input$Subnational_Government &
                              level2_name %in% input$Local_Government &
                                 ma_name %in% input$Managed_Access))$category))),
                 selected =  "Hard coral",
                 multiple = FALSE, selectize = TRUE)
  })
  
  #Select Year range to be plotted with sliders
   #output$Year <- renderUI({
    # sliderInput(inputId = "Year", strong("Year:"), round = T, step=1,
     #         min = 2005, max = 2019, ticks = T,
      #      value = range(c(2005:2019), na.rm = T),
       #    dragRange = F)
        #   })

 
  
  #Selected Fish biomass data
   selectedData_fish.biomass <- reactive ({ 
            droplevels(subset(fish.biomass, 
                          #Year %in% input$Year & 
                            country %in% input$Country & 
                              level1_name %in% input$Subnational_Government & 
                                level2_name %in% input$Local_Government &
                                   ma_name %in% input$Managed_Access))
   })
   
   #Selected Fish biomass data
   selectedData_fish.biomass.family <- reactive ({ 
      droplevels(subset(fish.biomass.family, 
                     #Year %in% input$Year & 
                        country %in% input$Country & 
                           level1_name %in% input$Subnational_Government & 
                              level2_name %in% input$Local_Government &
                                 ma_name %in% input$Managed_Access))
   })
   
   #Selected Fish species data
   selectedData_fish.size <- reactive ({ 
         droplevels(subset(fish.density.species,
                       #Year %in% input$Year & 
                       country %in% input$Country & 
                         level1_name %in% input$Subnational_Government & 
                            level2_name %in% input$Local_Government &
                              ma_name %in% input$Managed_Access))
   })
   
   #Selected Fish density by species
   selectedData_fish.density <- reactive ({ 
          droplevels(subset(fish.density,
                       #Year %in% input$Year & 
                       country %in% input$Country & 
                         level1_name %in% input$Subnational_Government & 
                            level2_name %in% input$Local_Government &
                                ma_name %in% input$Managed_Access))
   })
   
   #Selected Fish density by all species
   selectedData_fish.density.sizeclass <- reactive ({ 
     droplevels(subset(fish.density.sizeclass,
                       #Year %in% input$Year & 
                       country %in% input$Country & 
                         level1_name %in% input$Subnational_Government & 
                            level2_name %in% input$Local_Government &
                                ma_name %in% input$Managed_Access))
   })
   
   #Selected Fish density by family
   selectedData_fish.density.family <- reactive ({ 
      droplevels(subset(fish.density.family,
                        #Year %in% input$Year & 
                        country %in% input$Country & 
                           level1_name %in% input$Subnational_Government & 
                              level2_name %in% input$Local_Government &
                                  ma_name %in% input$Managed_Access))
   })
   
   #Selected Fish diversity data
   selectedData_fish.diversity <- reactive ({ 
        droplevels(subset(fish.diversity,
                       #Year %in% input$Year & 
                       country %in% input$Country & 
                         level1_name %in% input$Subnational_Government & 
                            level2_name %in% input$Local_Government &
                                ma_name %in% input$Managed_Access))
   })
   
  
   #Selected Benthic Cover data
   selectedData_benthic.cover <- reactive ({ 
     droplevels(subset(benthic.cover,
                       #Year %in% input$Year & 
                       country %in% input$Country & 
                         level1_name %in% input$Subnational_Government & 
                            level2_name %in% input$Local_Government &
                                ma_name %in% input$Managed_Access)) })
      
   #Selected Benthic Cover data
   selectedData_benthic.diversity <- reactive ({ 
     droplevels(subset(benthic.diversity,
                       #Year %in% input$Year & 
                       country %in% input$Country & 
                         level1_name %in% input$Subnational_Government & 
                          level2_name %in% input$Local_Government &
                           ma_name %in% input$Managed_Access))
   })
   
   #Selected Data for map Cover data
   selectedData_map <- reactive ({ 
     droplevels(subset(portal_map,
                       country %in% input$Country & 
                         level1_name %in% input$Subnational_Government & 
                          level2_name %in% input$Local_Government &
                           ma_name %in% input$Managed_Access))
   })
   
   
 #### Reserve performace ####
   ### FISH BIOMASS#####
   
 output$plot_fish_biomass<- renderPlot({
          req(nrow(selectedData_fish.biomass())> 0) 
              plotFishBiomass() })
     
 #if (input$Analysis == "Reserve Performace") {
   
 plotFishBiomass <- function(){
    
   if(input$plot_type == "Mean ± 95% CI") {
       if(input$fish_family == "All fish families") {
       
   plot_fish.biomass.mci <- ggplot(selectedData_fish.biomass(),
                                       aes(location_status, biomass_kg_ha), na.rm = TRUE) +
      theme_rare+
      facet_wrap(c(input$grouping_level), ncol=4, scale = input$y_axis)+
      geom_jitter(alpha=0.1, width = 0.05, height = 0, size =2) + 
      stat_summary(aes(col=location_status), na.rm=TRUE,
                       fun.data = "mean_se", geom = "pointrange", size = .7, position=position_dodge(width=.5)) +
      ggtitle("\nTOTAL FISH BIOMASS")+
      scale_colour_manual (values = c("#F58233", "#00AFD8"))+
            #input$y_log + #scale_x_log10()
            xlab ("") + ylab ("Fish biomass (kg/ha)") #+ coord_flip(clip="on")
      plot_fish.biomass.mci
       }
      
    else if (input$fish_family != "All fish families") {
  
   plot_fish.biomass.mci <- ggplot(subset(selectedData_fish.biomass.family(), family == c(input$fish_family)),
                                         aes(location_status, biomass_kg_ha), na.rm = TRUE) +
     theme_rare+
     facet_wrap(c(input$grouping_level), ncol=4, scale = input$y_axis)+
          geom_jitter(alpha=0.1, width = 0.05, height = 0, size =2) + 
     stat_summary(aes(col=location_status), na.rm=TRUE,
                  fun.data = "mean_se", geom = "pointrange", size = .7, position=position_dodge(width=.5)) +
      ggtitle(paste("\nFAMILY",toupper(c(input$fish_family))))+
      scale_colour_manual (values = c("#F58233", "#00AFD8"))+
       #input$y_log + #scale_x_log10()
      xlab ("") + ylab ("Fish biomass (kg/ha)") #+ coord_flip(clip="on")
    plot_fish.biomass.mci
      }
    }
       
    else if (input$plot_type == "Bar plots") {
       if (input$fish_family == "All fish families") {
          
          summary_data <- summarySE(data = selectedData_fish.biomass(), 
                                    measurevar = "biomass_kg_ha",
                                    groupvars = c("country", input$grouping_level, "location_status"))
          
          plot_fish.biomass.bar <- ggplot(summary_data, aes(location_status, biomass_kg_ha), na.rm=TRUE) +
             theme_rare + 
             facet_wrap(c(input$grouping_level), ncol=4, scale = input$y_axis)+
             geom_bar(aes(fill = location_status), position=position_dodge(), stat = "identity",
                      width = .5, colour="black", size=.3)+
             geom_errorbar(aes(ymin=biomass_kg_ha - SE, ymax = biomass_kg_ha + SE),
                           #stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", 
                           size = .5, position=position_dodge(width=.2),
                           na.rm=TRUE, width=.2) +
             scale_fill_manual (values = c("#F58233", "#00AFD8"))+
             ggtitle("\nTOTAL FISH BIOMASS")+
             #scale_y_continuous(expand = c(0,0))+#, limits = c(0, max(summary_data$biomass_kg_ha)))+
             #scale_y_log10()+
             xlab ("") + ylab ("Fish biomass (kg/ha)") #+ coord_flip(clip="on")
          
          plot_fish.biomass.bar
          
       }
       
       
      else if (input$fish_family != "All fish families") {
       
      summary_data <- summarySE(data = subset(selectedData_fish.biomass.family(), family == input$fish_family), 
                                measurevar = "biomass_kg_ha",
                                groupvars = c("country", input$grouping_level, "location_status"))
                                  
      plot_fish.biomass.bar <- ggplot(summary_data, aes(location_status, biomass_kg_ha), na.rm=TRUE) +
        theme_rare + 
        facet_wrap(c(input$grouping_level), ncol=4, scale = input$y_axis)+
         geom_bar(aes(fill = location_status), position=position_dodge(), stat = "identity",
                   width = .5, colour="black", size=.3)+
          geom_errorbar(aes(ymin=biomass_kg_ha - SE, ymax = biomass_kg_ha + SE),
              size = .5, position=position_dodge(width=.2), na.rm=TRUE, width=.2) +
          scale_fill_manual (values = c("#F58233", "#00AFD8"))+
        ggtitle(paste("\nFAMILY", toupper(input$fish_family)))+
        #scale_y_continuous(expand = c(0,0))+#, limits = c(0, max(summary_data$biomass_kg_ha)))+
        #scale_y_log10()+
        xlab ("") + ylab ("Fish biomass (kg/ha)") #+ coord_flip(clip="on")
      
      plot_fish.biomass.bar
      
        }
     
    }
 }
   
   
   #Summary table for fish biomass
   summary_fish_biomass <- reactive({summarySE(selectedData_fish.biomass(), measurevar = "biomass_kg_ha",
                                            groupvars= c("country", input$grouping_level,
                                                         "location_status"))})
    
   output$summary_fish_biomas <- renderTable ({ summary_fish_biomass() })
  
   #Download Data
   output$downloadData0 <- downloadHandler(
      filename = function() {
          paste0("summary_fish_biomass_",input$Country, "_",input$grouping_level,"_level", ".csv", sep="")
              },  
          content = function(file) {
              write.csv(summary_fish_biomass(), file, row.names = FALSE)
          }
      )
  #Download Figure
  output$downloadPlot0 <- downloadHandler(
     filename = function() { 
       paste0("plot_fish_biomass_", input$Country, "_",input$grouping_level, "_level", ".png", sep="") 
           },
     content = function(file) {
        ggsave(plot = last_plot(), file, device = "png", units = "in", dpi = 600, scale=0.8)
     }
   )

   #### FISH DENSITY ####
   
   output$plot_fish.density<- renderPlot({
     req(nrow(selectedData_fish.density())> 0)
     
     plotFishDensity() })
  
  plotFishDensity <- function(){
     
     #if (input$Analysis == "Reserve Performace") {
     
     if (input$plot_type == "Mean ± 95% CI") {
        if(input$fish_family == "All fish families") {
       
       plot_fish.density.mci <- ggplot(selectedData_fish.density(), aes(location_status, density_ind_ha), na.rm = T) +
         theme_rare + 
         facet_wrap(c(input$grouping_level), ncol=4, scale = input$y_axis)+
         stat_summary(aes(col=location_status), na.rm=TRUE,
                      fun.data = "mean_se", geom = "pointrange", size = .7, position=position_dodge(width=.5)) +
         geom_jitter(alpha=0.1, width = 0.05, height = 0, size =2) + 
         #scale_y_continuous(breaks = seq(0,1000, 100))+
         ggtitle("\nTOTAL FISH DENSITY ")+
         scale_colour_manual (values = c("#F58233", "#00AFD8"))+
         #scale_y_continuous(expand = c(0.02,0))+
         #scale_y_log10()+
         xlab ("") + ylab ("Fish density (individuals / ha)") #+ coord_flip(clip="on")
       
       plot_fish.density.mci
        }
        
        else if (input$fish_family != "All fish families") {
           plot_fish.density.mci <- ggplot(subset(selectedData_fish.density.family(), family == input$fish_family), 
                                           aes(location_status, density_ind_ha), na.rm = T) +
              theme_rare + 
              facet_wrap(c(input$grouping_level), ncol=4, scale = input$y_axis)+
              stat_summary(aes(col=location_status), na.rm=TRUE,
                           fun.data = "mean_se", geom = "pointrange", size = .7, position=position_dodge(width=.5)) +
              geom_jitter(alpha=0.1, width = 0.05, height = 0, size =2) + 
              #scale_y_continuous(breaks = seq(0,1000, 100))+
              ggtitle(paste("\nFAMILY", toupper(input$fish_family)))+
              scale_colour_manual (values = c("#F58233", "#00AFD8"))+
              #scale_y_continuous(expand = c(0.02,0))+
              #scale_y_log10()+
              xlab ("") + ylab ("Fish density (individuals / ha)") #+ coord_flip(clip="on")
           plot_fish.density.mci
           }
       }
     
     else if (input$plot_type == "Bar plots") {
        if (input$fish_family == "All fish families") {
      
       summary_data <- summarySE(selectedData_fish.density(), measurevar = "density_ind_ha",
                                 groupvars= c("country", input$grouping_level,
                                              "location_status"))
       
       plot_fish.density.bar <- ggplot(summary_data, aes(location_status, density_ind_ha), na.rm=TRUE) +
         theme_rare + 
         facet_wrap(c(input$grouping_level), ncol=4, scale = input$y_axis)+
         geom_bar(aes(fill = location_status), position=position_dodge(), stat = "identity",
                  width = 0.5, colour="black", size=.3)+
         geom_errorbar(aes(ymin=density_ind_ha-SE, ymax = density_ind_ha+SE),
                       size = .5, position=position_dodge(width=.2),
                       na.rm=TRUE, width = .2) +
         scale_y_continuous(expand = c(0.02,0))+
         scale_fill_manual (values = c("#F58233", "#00AFD8"))+
         ggtitle("\nTOTAL FISH DENSITY ")+
         #scale_y_log10()+
         xlab ("") + ylab ("Fish density (individuals / ha)") #+ coord_flip(clip="on")
       
       plot_fish.density.bar
        }
        
      else if (input$fish_family != "All fish families") {
         
         summary_data <- summarySE(subset(selectedData_fish.density.family(), family == input$fish_family),
                                          measurevar = "density_ind_ha",
                                   groupvars= c("country", input$grouping_level, "location_status"))
         
         plot_fish.density.bar <- ggplot(summary_data, aes(location_status, density_ind_ha), na.rm=TRUE) +
            theme_rare + 
            facet_wrap(c(input$grouping_level), ncol=4, scale = input$y_axis)+
            geom_bar(aes(fill = location_status), position=position_dodge(), stat = "identity",
                     width = 0.5, colour="black", size=.3)+
            geom_errorbar(aes(ymin=density_ind_ha-SE, ymax = density_ind_ha+SE),
                          size = .5, position=position_dodge(width=.2),
                          na.rm=TRUE, width = .2) +
            scale_y_continuous(expand = c(0.02,0))+
            scale_fill_manual (values = c("#F58233", "#00AFD8"))+
            ggtitle(paste("\nFAMILY", toupper(input$fish_family)))+
            #scale_y_log10()+
            xlab ("") + ylab ("Fish density (individuals / ha)") #+ coord_flip(clip="on")
         
         plot_fish.density.bar  
         }
      }
   }
   
   #Summary table for fish density
   summary_fish_density <- reactive({ summarySE(selectedData_fish.density(), measurevar = "density_ind_ha",
                                                groupvars= c("country", input$grouping_level,
                                                             "location_status")) })
   output$summary_fish_density <- renderTable ({ summary_fish_density() })
   
   #Download data
   output$downloadData1 <- downloadHandler(
     filename = function() {
       paste("summary_fish_density_",input$Country, "_",input$grouping_level, ".csv", sep="")
     },  
     content = function(file) {
       write.csv(summary_fish_density(), file, row.names = FALSE)
     }
   )
   
   #Download Figure
   output$downloadPlot1 <- downloadHandler(
     filename = function() { 
       paste0("plot_fish_density_", input$Country, "_",input$grouping_level, "_level", ".png", sep="") 
     },
     content = function(file) {
       ggsave(plot = last_plot(), file, device = "png", units = "in", dpi = 600, scale=0.8)
     }
   )
   
   
   #### FISH SIZE  ####
   #Check species abundances 
   
   output$plot_fish.size<- renderPlot({
          req(nrow(selectedData_fish.size())> 0)
       plotFishSize() })
   
   plotFishSize <- function(){
     
     if (input$plot_type == "Bar plots") {
       if(input$fish_species == "All fish species") {
         plot_fish.size <- ggplot(selectedData_fish.density.sizeclass(), aes(sizeclass), na.rm=T) + 
           theme_rare_sizeclass +
           facet_wrap (c(input$grouping_level), ncol=3, scale= input$y_axis)+
           geom_bar(aes(fill=location_status), position = position_dodge(preserve = "single"),
                    width = .6, colour="black", size=.3) +
           #scale_y_continuous(breaks=pretty_breaks(u5.bias = .5)) +
           xlab("Size class (cm)")+ ylab("Counts")+
           ggtitle(paste("\n", toupper(input$fish_species)))+
           scale_fill_manual (values = c("#F58233", "#00AFD8"))
         plot_fish.size
       } 
      else if (input$fish_species != "All fish species") {
        plot_fish.size <- ggplot(subset(selectedData_fish.size(), species == input$fish_species),
                                      aes(sizeclass), na.rm=T)+ 
         theme_rare_sizeclass +
         facet_wrap (c(input$grouping_level), ncol=3, scale= input$y_axis)+
         geom_bar(aes(fill=location_status), position = position_dodge(preserve = "single"),
                  width = .6, colour="black", size=.3) +
           #scale_y_continuous(breaks=pretty_breaks(u5.bias = .5)) +
         xlab("Size class (cm)")+ ylab("Counts")+
         ggtitle(paste("\n", toupper(input$fish_species)))+
         scale_fill_manual (values = c("#F58233", "#00AFD8"))
     plot_fish.size
        }
      }
     
     else if (input$plot_type == "Mean ± 95% CI") {
       if(input$fish_species == "All fish species") {
         plot_fish.size <- ggplot(selectedData_fish.density.sizeclass(), aes(length, group=location_status), na.rm=T)+ 
           theme_rare_sizeclass+
           facet_wrap (c(input$grouping_level), ncol=3, scale= input$y_axis)+
           geom_density(aes(fill=location_status), colour="black", size=.3, alpha = 0.5)+
              geom_vline(aes(xintercept = mean(length, na.rm=T), colour=location_status),  lty=3)+
           geom_text(aes(label="L-avg", x=mean(length, na.rm=T)+2, y = 0, hjust=-0.3), 
                      angle = 90, parse = TRUE, col = "grey50")+
           #scale_y_continuous(breaks=pretty_breaks(u5.bias = .5)) +
           #scale_x_continuous(limits = c(0,50)) +
           xlab("Length (cm)")+ ylab("Density")+
           ggtitle(paste("\n", toupper(input$fish_species)))+
           scale_fill_manual (values = c("#F58233", "#00AFD8"))
         plot_fish.size
        }
       else if (input$fish_species != "All fish species") {
        plot_fish.size <- ggplot(subset(selectedData_fish.size(), species == input$fish_species), aes(length), na.rm=T)+ 
          theme_rare_sizeclass+
          facet_wrap (c(input$grouping_level), ncol=3, scale= input$y_axis)+
          geom_density(aes(fill=location_status), colour="black", size=.3, alpha = 0.5) +
            geom_vline(aes(xintercept = lmax), col = 2, lty= 3)+
            geom_vline(aes(xintercept = mean(length, na.rm=T)), col = 4, lty=3)+
         geom_text (aes(label="L-max", x=lmax+2, y = 0, hjust=-0.3), angle = 90, parse = TRUE, col = "grey50")+
         geom_text (aes(label="L-avg", x=mean(length, na.rm=T)+2, y = 0, hjust=-0.3), angle = 90, parse = TRUE, col = "grey50")+
         #scale_y_continuous(breaks=pretty_breaks(u5.bias = .5)) +
       #scale_x_continuous(limits = c(0,50)) +
       xlab("Length (cm)")+ ylab("Density")+
       ggtitle(paste("\n", toupper(input$fish_species)))+
       scale_fill_manual (values = c("#F58233", "#00AFD8"))
     plot_fish.size
       }
     }
   }
   
 #Summary table for fish density
   summary_fish_size <- reactive({ summarySE(selectedData_fish.size(), measurevar = "length",
                                                groupvars= c("country", input$grouping_level,
                                                             "location_status")) })
   output$summary_fish_size <- renderTable ({ summary_fish_size() })
   
   #Download data
   output$downloadData2 <- downloadHandler(
     filename = function() {
       paste("summary_fish_size_",input$Country, "_",input$grouping_level, ".csv", sep="")
     },  
     content = function(file) {
       write.csv(summary_fish_size(), file, row.names = FALSE)
     }
   )
   
   #Download Figure
   output$downloadPlot2 <- downloadHandler(
     filename = function() { 
       paste0("plot_fish_size_", input$Country, "_",input$grouping_level, "_level", ".png", sep="") 
     },
     content = function(file) {
       ggsave(plot = last_plot(), file, device = "png", units = "in", dpi = 600, scale=0.8)
     }
   )
   

  ### FISH DIVERSITY #####
    
   output$plot_fish.deversity <- renderPlot({
     
     req(nrow(selectedData_fish.diversity())> 0)
     
     plotFishDiversity() })
   
   plotFishDiversity <- function(){
     
     #if (input$Analysis == "Reserve Performace") {
     
     if (input$plot_type == "Mean ± 95% CI") {
       
       plot_fish.diversity <- ggplot(selectedData_fish.diversity(), aes(location_status, species), na.rm = T) +
         theme_rare + 
         facet_wrap(c(input$grouping_level), ncol=4, scale = input$y_axis)+
         stat_summary(aes(col=location_status), na.rm=TRUE,
                      fun.data = "mean_se", geom = "pointrange", size = .7, position=position_dodge(width=.5)) +
         geom_jitter(alpha=0.1, width = 0.05, height = 0, size =2) + 
         scale_y_continuous(breaks = pretty_breaks(n=5))+
         ggtitle("\nFISH DIVERSITY ")+
         scale_colour_manual (values = c("#F58233", "#00AFD8"))+
         scale_y_continuous(expand = c(0.02,0))+
         #scale_y_log10()+
         xlab ("") + ylab ("Number of fish species") #+ coord_flip(clip="on")
       
       plot_fish.diversity
       
     }
     
     else if (input$plot_type == "Bar plots") {
       
        summary_data1 <- summarySE(selectedData_fish.diversity(), measurevar = "species",
                                    groupvars= c("country", input$grouping_level,
                                                 "location_status"))
        
         plot_fish.diversity.bar <- ggplot(summary_data1, aes(location_status, species), na.rm=TRUE) +
           theme_rare + 
           facet_wrap(c(input$grouping_level), ncol=4, scale = input$y_axis)+
           geom_bar(aes(fill = location_status), position=position_dodge(), stat = "identity",
                    width = 0.5, colour="black", size=.3)+
           geom_errorbar(aes(ymin=species-SE, ymax = species+SE),
                         size = .5, position=position_dodge(width=.2),
                         na.rm=TRUE, width = .2) +
           #scale_y_continuous(breaks = seq(0,1000, 100))+
           scale_fill_manual (values = c("#F58233", "#00AFD8"))+
           ggtitle("\nFISH DIVERSITY ")+
           scale_y_continuous(expand = c(0.02,0))+
           xlab ("") + ylab ("Number of fish species") #+ coord_flip(clip="on")
         
         plot_fish.diversity.bar
       }
   }
   
   
   #Summary table for fish density
   summary_fish_diversity <- reactive({ summarySE(selectedData_fish.diversity(), measurevar = "species",
                                                  groupvars= c("country", input$grouping_level,
                                                               "location_status")) })
   
   output$summary_fish_diversity <- renderTable ({ summary_fish_diversity() })
   
   #Download Data
   output$downloadData3 <- downloadHandler(
     filename = function() {
       paste("summary_fish_diversity_",input$Country, "_",input$grouping_level, ".csv", sep="")
     },  
     content = function(file) {
       write.csv(summary_fish_diversity(), file, row.names = FALSE)
     }
   )
   
   #Download Figure
   output$downloadPlot3 <- downloadHandler(
     filename = function() { 
       paste0("plot_fish_diversity_", input$Country, "_",input$grouping_level, "_level", ".png", sep="") 
     },
     content = function(file) {
       ggsave(plot = last_plot(), file, device = "png", units = "in", dpi = 600, scale=0.8)
     }
   )
   

 ### HABITAT COVER ####
   
   output$plot_hardcoral.cover<- renderPlot({
     req(nrow(selectedData_benthic.cover())> 0) 
         plot_hardcoral.cover() })
   
    #if (input$Analysis == "Reserve Performace") {
   
   plot_hardcoral.cover <- function(){
     
     if (input$plot_type == "Mean ± 95% CI") { 
       
       plot_hardcoral.cover.mci <- ggplot(droplevels(subset(selectedData_benthic.cover(), 
                                                            category == input$habitat_category)), 
                                          aes(location_status, percentage), na.rm = T) +
         theme_rare + 
         facet_wrap(c(input$grouping_level), ncol=4, scale = input$y_axis)+
         geom_jitter(alpha=0.1, width = 0.05, height = 0, size =2) + 
         scale_y_continuous(expand = c(0, 0))+
         stat_summary(aes(col=location_status), na.rm=TRUE,
                      fun.data = "mean_se", geom = "pointrange", size = .7, position=position_dodge(width=.5)) +
         ggtitle(paste("\n",toupper(input$habitat_category)))+
         scale_colour_manual (values = c("#F58233", "#00AFD8"))+
         #input$y_log +
         #scale_x_log10()
         xlab ("") + ylab ("Substrate Cover(%)") 
       
       plot_hardcoral.cover.mci
       
     }
     
     
     else if (input$plot_type == "Bar plots") {
       
       summary_data <- summarySE(data=subset(selectedData_benthic.cover(), category == input$habitat_category),
                                 measurevar = "percentage",
                                 groupvars= c("country", input$grouping_level, "location_status"))
       
       plot_hardcoral.cover.bar <- ggplot(summary_data, aes(location_status, percentage), na.rm=TRUE) +
         theme_rare + 
         facet_wrap(c(input$grouping_level), ncol=4, scale = input$y_axis)+
         geom_bar(aes(fill = location_status), position=position_dodge(), stat = "identity",
                  width = .5, colour="black", size=.3)+
         geom_errorbar(aes(ymin=percentage - SE, ymax = percentage + SE),
                       #stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", 
                       size = .5, position=position_dodge(width=.2),
                       na.rm=TRUE, width=.2) +
         
         scale_fill_manual (values = c("#F58233", "#00AFD8"))+
         ggtitle(paste("\n",toupper(input$habitat_category)))+
         scale_y_continuous(expand = c(0.02,0))+
         #scale_y_log10()+
         xlab ("") + ylab ("Substrate Cover (%)") #+ coord_flip(clip="on")
       
       plot_hardcoral.cover.bar
       
     }
     
   } 
   
   
   #Summary table for fish biomass
   summary_hardcoral.cover <- reactive({summarySE(subset(selectedData_benthic.cover(),
                                                          category == "Hard coral"),
                                                          measurevar = "percentage",
                                               groupvars= c("country", input$grouping_level,
                                                            "location_status"))})
   
   output$summary_hardcoral.cover <- renderTable ({ summary_hardcoral.cover() })
   
   #Download Data
   output$downloadData4 <- downloadHandler(
     filename = function() {
       paste0("summary_hardcoral_cover_",input$Country, "_",input$grouping_level,"_level", ".csv", sep="")
     },  
     content = function(file) {
       write.csv(summary_hardcoral.cover(), file, row.names = FALSE)
     }
   )
   #Download Figure
   output$downloadPlot4 <- downloadHandler(
     filename = function() { 
       paste0("plot_hardcoral_cover_", input$Country, "_",input$grouping_level, "_level", ".png", sep="") 
     },
     content = function(file) {
       ggsave(plot = last_plot(), file, device = "png", units = "in", dpi = 600, scale=1)
     }
   )
   
  
   #### HABITAT DIVERSITY ####
   
   output$plot_benthic.diversity<- renderPlot({
     
     req(nrow(selectedData_benthic.diversity())> 0) 
     
     plot_benthic.diversity() })
   
   #if (input$Analysis == "Reserve Performace") {
   
   plot_benthic.diversity <- function(){
     
     if (input$plot_type == "Mean ± 95% CI") { 
       
       plot_benthic.diversity.mci <- ggplot(selectedData_benthic.diversity(), 
                                          aes(location_status, attribute), na.rm = T) +
         theme_rare + 
         facet_wrap(c(input$grouping_level), ncol=4, scale = input$y_axis)+
         geom_jitter(alpha=0.1, width = 0.05, height = 0, size =2) + 
         scale_y_continuous(expand = c(0, 0))+
         stat_summary(aes(col=location_status), na.rm=TRUE,
                      fun.data = "mean_se", geom = "pointrange", size = .7, position=position_dodge(width=.5)) +
         ggtitle("\nHABITAT DIVERSITY ")+
         scale_colour_manual (values = c("#F58233", "#00AFD8"))+
         #input$y_log +
         #scale_x_log10()
         xlab ("") + ylab ("Number of benthic groups") 
       
       plot_benthic.diversity.mci
       
     }
     
     
     else if (input$plot_type == "Bar plots") {
       
       summary_data <- summarySE(data=selectedData_benthic.diversity(),
                                 measurevar = "attribute",
                                 groupvars= c("country", input$grouping_level, "location_status"))
       
       plot_benthic.diversity.bar <- ggplot(summary_data, aes(location_status, attribute), na.rm=TRUE) +
         theme_rare + 
         facet_wrap(c(input$grouping_level), ncol=4, scale = input$y_axis)+
         geom_bar(aes(fill = location_status), position=position_dodge(), stat = "identity",
                  width = .5, colour="black", size=.3)+
         geom_errorbar(aes(ymin=attribute - SE, ymax = attribute + SE),
                       #stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", 
                       size = .5, position=position_dodge(width=.2),
                       na.rm=TRUE, width=.2) +
         scale_fill_manual (values = c("#F58233", "#00AFD8"))+
         ggtitle("\nHABITAT DIVERSITY ")+
         scale_y_continuous(expand = c(0.02,0))+
         #scale_y_log10()+
         xlab ("") + ylab ("Number of benthic groups") #+ coord_flip(clip="on")
       
       plot_benthic.diversity.bar
       
     }
     
   } 
   

   #Summary table for fish biomass
   summary_habitat.diversity <- reactive({summarySE(selectedData_benthic.diversity(),
                                                  measurevar = "attribute",
                                                  groupvars= c("country", input$grouping_level,
                                                               "location_status"))})
   
   output$summary_habitat.diversity <- renderTable ({ summary_habitat.diversity() })
   
   #Download Data
   output$downloadData5 <- downloadHandler(
     filename = function() {
       paste0("summary_habitat_diversity_",input$Country, "_",input$grouping_level,"_level", ".csv", sep="")
     },  
     content = function(file) {
       write.csv(summary_habitat.diversity(), file, row.names = FALSE)
     }
   )
   #Download Figure
   output$downloadPlot5 <- downloadHandler(
     filename = function() { 
       paste0("plot_habitat_diversity_", input$Country, "_",input$grouping_level, "_level", ".png", sep="") 
     },
     content = function(file) {
       ggsave(plot = last_plot(), file, device = "png", units = "in", dpi = 900, scale=0.5)
     }
   )
   
   
  ## Sites trends ####
   ## Fish biomass
   ### Create a boxplot  ####
   output$plot1 <- renderPlot({
     
     par(mfrow = c(2,3), mar=c(3,2,2,1), oma=c(1,2,1,1), cex=1)
     
     # IF BOXPLOT IS  CHOSEN ###
     if (input$Analysis == "Site trends") {
       
       if (input$plot_type == "Bar plots") {
         
         par(mfrow = c(2,5), mar=c(3,2,2,1), oma=c(1,2,1,1), cex=1)
         
         boxplot(TF/10~Year, selectedData(), main = "TOTAL FISH", ylab=NULL, outline=T, lty=1,
                 col=c("lightblue"), las=1)
         points(factor(selectedData()$Year), (selectedData()$TF/10), col = "darkgrey")
         mtext("biomass (kg/ha)", side = 2, line = 3, cex= 1.2)
         
         boxplot(HHRI/10~Year, selectedData(), main = "HERBIVOROUS", ylab=NULL, outline=T,
                 lty=1, col=c("lightblue"), las=1) 
         points(factor(selectedData()$Year), jitter(selectedData()$HHRI/10), col = "darkgrey")
         
         boxplot(I/10~Year, selectedData(), main = "INVERTIVOROUS", ylab=NULL, outline=T,
                 lty=1, col=c("lightblue"), las=1) 
         points(factor(selectedData()$Year), jitter(selectedData()$I/10), col = "darkgrey")
         
         boxplot(P/10~Year, selectedData(), main = "PISCIVOROUS", ylab=NULL, outline=T,
                 lty=1, col=c("lightblue"), las=1) #, names = c("current", "predicted"))
         points(factor(selectedData()$Year), jitter(selectedData()$P/10), col = "darkgrey")
         
         boxplot(CSHRI/10~Year, selectedData(), main = "COMMERCIAL", ylab=NULL, outline=T,
                 lty=1, col=c("lightblue"), las=1) #, names = c("current", "predicted"))
         points(factor(selectedData()$Year), jitter(selectedData()$CSHRI/10), col = "darkgrey")
         
         boxplot(PARR/10~Year, selectedData(), main = "PARROTFISH", ylab="biomass (kg/ha)", outline=F,
                 lty=1, col=c("lightblue"), las=1) #, names = c("current", "predicted"))
         points(factor(selectedData()$Year), jitter(selectedData()$PARR/10), col = "darkgrey")
         
         mtext("biomass (kg/ha)", side = 2, line = 3, cex= 1.2)
         boxplot(SEAB/10~Year, selectedData(), main = "GROUPERS", ylab=NULL, outline=T,
                 lty=1, col=c("lightblue"), las=1) #, names = c("current", "predicted"))
         points(factor(selectedData()$Year), jitter(selectedData()$SEAB/10), col = "darkgrey")
         
         boxplot(SNAP/10~Year, selectedData(), main = "SNAPPERS", ylab=NULL, outline=T,
                 lty=1, col=c("lightblue"), las=1) # names = c("current", "predicted"))
         points(factor(selectedData()$Year), jitter(selectedData()$SNAP/10), col = "darkgrey")
         
         boxplot(JACK/10~Year, selectedData(), main = "JACKS", ylab=NULL, outline=T,
                 lty=1, col=c("lightblue"), las=1) #, names = c("current", "predicted"))
         points(factor(selectedData()$Year), jitter(selectedData()$JACK/10), col = "darkgrey")
         
         boxplot(GRUN/10~Year, selectedData(), main = "GRUNTS", ylab=NULL, outline=T, 
                 lty=1, col=c("lightblue"), las=1) #names = c("current", "predicted"))
         points(factor(selectedData()$Year), jitter(selectedData()$GRUN/10), col = "darkgrey")
         
       } 
       
       
       ## IF MEAN and SE is selected ##
       else if (input$plot_type == "Mean ± 95% CI") {
         
         par(mfrow = c(2,5), mar=c(3,2,2,1), oma=c(1,2,1,1), cex=1)
         
         p1 <- ggplot(selectedData(), aes(factor(Year), selectedData()$TF/10) ) + theme_bw()+
           #geom_jitter(shape=1, width = .1, alpha=.2) +
           stat_summary(fun.data = "mean_se", geom = "pointrange", 
                        size = .5, position=position_dodge(width=.5)) +
           theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
                 axis.title.y = element_text(size=12),
                 axis.text  = element_text(size=12))+
           ggtitle("TOTAL FISH") + xlab("") + ylab("biomass (kg/ha)")
         
         p1a <- ggplot(selectedData_IDN(), aes(factor(Year), log(selectedData_IDN()$biomass_kg_ha+1))) + 
           #facet_wrap(~inside_or_outside_NTZ)+
           ggtitle("TOTAL FISH") + xlab("") + ylab("biomass (kg/ha)")
         geom_boxplot()+ theme_bw() +
           theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
                 axis.title.y = element_text(size=12),
                 axis.text  = element_text(size=12))+
           geom_jitter(width=0.05,height=0.05, alpha = 0.5)
         
         p2 <- ggplot(selectedData(), aes(factor(Year), selectedData()$HHRI/10) ) + theme_bw()+
           #geom_jitter(shape=1, width = .1, alpha=.2) +
           stat_summary(fun.data = "mean_se", geom = "pointrange", 
                        size = .5, position=position_dodge(width=.5)) +
           theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
                 axis.text  = element_text(size=12)) +
           ggtitle("HERBIVOROUS") + xlab("") + ylab(NULL)
         
         p3 <- ggplot(selectedData(), aes(factor(Year), selectedData()$I/10) ) + theme_bw()+
           #geom_jitter(shape=1, width = .1, alpha=.2) +
           stat_summary(fun.data = "mean_se", geom = "pointrange", 
                        size = .5, position=position_dodge(width=.5)) +
           theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
                 axis.text  = element_text(size=12)) +
           ggtitle("INVERTIVOROUS") + xlab("") + ylab(NULL)
         
         p4 <- ggplot(selectedData(), aes(factor(Year), selectedData()$P/10) ) + theme_bw()+
           #geom_jitter(shape=1, width = .1, alpha=.2) +
           stat_summary(fun.data = "mean_se", geom = "pointrange", 
                        size = .5, position=position_dodge(width=.5)) +
           theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
                 axis.text  = element_text(size=12)) +
           ggtitle("PISCIVOROUS") + xlab("") + ylab(NULL)
         
         p5 <- ggplot(selectedData(), aes(factor(Year), selectedData()$CSHRI/10) ) + theme_bw()+
           #geom_jitter(shape=1, width = .1, alpha=.2) +
           stat_summary(fun.data = "mean_se", geom = "pointrange", 
                        size = .5, position=position_dodge(width=.5)) +
           theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
                 axis.text  = element_text(size=12)) +
           ggtitle("COMMERCIAL") + xlab("") + ylab(NULL)
         
         p6 <- ggplot(selectedData(), aes(factor(Year), selectedData()$PARR/10) ) + theme_bw()+
           #geom_jitter(shape=1, width = .1, alpha=.2) +
           stat_summary(fun.data = "mean_se", geom = "pointrange", 
                        size = .5, position=position_dodge(width=.5)) +
           theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
                 axis.title.y = element_text(size=12),
                 axis.text  = element_text(size=12)) +
           ggtitle("PARROTFISH") + xlab("") + ylab("biomass (kg/ha)")
         
         p7 <- ggplot(selectedData(), aes(factor(Year), selectedData()$SEAB/10) ) + theme_bw()+
           #geom_jitter(shape=1, width = .1, alpha=.2) +
           stat_summary(fun.data = "mean_se", geom = "pointrange", 
                        size = .5, position=position_dodge(width=.5)) +
           theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size =14),
                 axis.text  = element_text(size=12)) +
           ggtitle("GROUPERS") + xlab("") + ylab(NULL)
         
         p8 <- ggplot(selectedData(), aes(factor(Year), selectedData()$SNAP/10) ) + theme_bw()+
           #geom_jitter(shape=1, width = .1, alpha=.2) +
           stat_summary(fun.data = "mean_se", geom = "pointrange", 
                        size = .5, position=position_dodge(width=.5)) +
           theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
                 axis.text  = element_text(size=12)) +
           ggtitle("SNAPPERS") + xlab("") + ylab(NULL)
         
         p9 <- ggplot(selectedData(), aes(factor(Year), selectedData()$JACK/10) ) + theme_bw()+
           #geom_jitter(shape=1, width = .1, alpha=.2) +
           stat_summary(fun.data = "mean_se", geom = "pointrange", 
                        size = .5, position=position_dodge(width=.5)) +
           theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size =14),
                 axis.text  = element_text(size=12)) +
           ggtitle("JACKS") + xlab("") + ylab(NULL)
         
         p10 <- ggplot(selectedData(), aes(factor(Year), selectedData()$GRU/10) ) + theme_bw()+
           #geom_jitter(shape=1, width = .1, alpha=.2) +
           stat_summary(fun.data = "mean_se", geom = "pointrange", 
                        size = .5, position=position_dodge(width=.5)) +
           theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size =14),
                 axis.text  = element_text(size=12)) +
           ggtitle("GRUNTS") + xlab("") + ylab(NULL)
         
         grid.arrange(p1, p1a, p3, p4, p5, p6, p7, p8, p9, p10, nrow = 2, ncol=5)
         
       }
       
       else if (input$plot_type == "Trend lines") {
         
         par(mfrow = c(2,5), mar=c(3,2,2,1), oma=c(1,2,1,1), cex=1)
         
         p1 <- ggplot(selectedData(), aes((Year), selectedData()$TF/10) ) + theme_bw()+
           #geom_jitter(shape=1, width = .1, alpha=.2) +
           geom_smooth()+
           theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
                 axis.title.y = element_text(size=12),
                 axis.text  = element_text(size=12))+
           ggtitle("TOTAL FISH") + xlab("") + ylab("biomass (kg/ha)")
         
         p2 <- ggplot(selectedData(), aes((Year), selectedData()$HHRI/10) ) + theme_bw()+
           #geom_jitter(shape=1, width = .1, alpha=.2) +
           geom_smooth()+
           theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
                 axis.text  = element_text(size=12)) +
           ggtitle("HERBIVOROUS") + xlab("") + ylab(NULL)
         
         p3 <- ggplot(selectedData(), aes( (Year), selectedData()$I/10) ) + theme_bw()+
           #geom_jitter(shape=1, width = .1, alpha=.2) +
           geom_smooth()+
           theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
                 axis.text  = element_text(size=12)) +
           ggtitle("INVERTIVOROUS") + xlab("") + ylab(NULL)
         
         p4 <- ggplot(selectedData(), aes( (Year), selectedData()$P/10) ) + theme_bw()+
           #geom_jitter(shape=1, width = .1, alpha=.2) +
           geom_smooth()+
           theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
                 axis.text  = element_text(size=12)) +
           ggtitle("PISCIVOROUS") + xlab("") + ylab(NULL)
         
         p5 <- ggplot(selectedData(), aes( (Year), selectedData()$CSHRI/10) ) + theme_bw()+
           #geom_jitter(shape=1, width = .1, alpha=.2) +
           geom_smooth()+
           theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
                 axis.text  = element_text(size=12)) +
           ggtitle("COMMERCIAL") + xlab("") + ylab(NULL)
         
         p6 <- ggplot(selectedData(), aes( (Year), selectedData()$PARR/10) ) + theme_bw()+
           #geom_jitter(shape=1, width = .1, alpha=.2) +
           geom_smooth()+
           theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
                 axis.title.y = element_text(size=12),
                 axis.text  = element_text(size=12)) +
           ggtitle("PARROTFISH") + xlab("") + ylab("biomass (kg/ha)")
         
         p7 <- ggplot(selectedData(), aes( (Year), selectedData()$SEAB/10) ) + theme_bw()+
           #geom_jitter(shape=1, width = .1, alpha=.2) +
           geom_smooth()+
           theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size =14),
                 axis.text  = element_text(size=12)) +
           ggtitle("GROUPERS") + xlab("") + ylab(NULL)
         
         p8 <- ggplot(selectedData(), aes( (Year), selectedData()$SNAP/10) ) + theme_bw()+
           #geom_jitter(shape=1, width = .1, alpha=.2) +
           geom_smooth()+
           theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
                 axis.text  = element_text(size=12)) +
           ggtitle("SNAPPERS") + xlab("") + ylab(NULL)
         
         p9 <- ggplot(selectedData(), aes( (Year), selectedData()$JACK/10) ) + theme_bw()+
           #geom_jitter(shape=1, width = .1, alpha=.2) +
           geom_smooth()+
           theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size =14),
                 axis.text  = element_text(size=12)) +
           ggtitle("JACKS") + xlab("") + ylab(NULL)
         
         p10 <- ggplot(selectedData(), aes( (Year), selectedData()$GRU/10) ) + theme_bw()+
           #geom_jitter(shape=1, width = .1, alpha=.2) +
           geom_smooth()+
           theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size =14),
                 axis.text  = element_text(size=12)) +
           ggtitle("GRUNTS") + xlab("") + ylab(NULL)
         
         grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, nrow = 2, ncol=5)
         
       }
       
     }
     
     else if (input$Analysis == "Site Trends") {
       
       if (input$plot_type == "Bar plots") {
         
         par(mfrow = c(2,5), mar=c(3,2,2,1), oma=c(1,2,1,1), cex=1)
         
         boxplot(TF/10~Zone, mar1, main = "TOTAL FISH", ylab=NULL, outline=T, lty=1,
                 col=c("darkorange","white", "lightblue"), las=1,
                 names=c("Fished", "GU", "Reserve"))
         points(factor(selectedData()$Zone), (selectedData()$TF/10), col = "darkgrey")
         mtext("biomass (kg/ha)", side = 2, line = 3, cex= 1.2)
         
         boxplot(HHRI/10~Zone, selectedData(), main = "HERBIVOROUS", ylab=NULL, outline=T,
                 lty=1, col=c("darkorange","white", "lightblue"), las=1,
                 names=c("Fished", "General Use", "Reserve"))
         points(factor(selectedData()$Zone), jitter(selectedData()$HHRI/10), col = "darkgrey")
         
         boxplot(I/10~Zone, selectedData(), main = "INVERTIVOROUS", ylab=NULL, outline=T,
                 lty=1, col=c("darkorange","white", "lightblue"), las=1,
                 names=c("Fished", "General Use", "Reserve"))
         points(factor(selectedData()$Zone), jitter(selectedData()$I/10), col = "darkgrey")
         
         boxplot(P/10~Zone, selectedData(), main = "PISCIVOROUS", ylab=NULL, outline=T,
                 lty=1, col=c("darkorange","white", "lightblue"), las=1,
                 names=c("Fished", "General Use", "Reserve"))
         points(factor(selectedData()$Zone), jitter(selectedData()$P/10), col = "darkgrey")
         
         boxplot(CSHRI/10~Zone, selectedData(), main = "COMMERCIAL", ylab=NULL, outline=T,
                 lty=1, col=c("darkorange","white", "lightblue"), las=1,
                 names=c("Fished", "General Use", "Reserve"))
         points(factor(selectedData()$Zone), jitter(selectedData()$CSHRI/10), col = "darkgrey")
         
         boxplot(PARR/10~Zone, selectedData(), main = "PARROTFISH", ylab="biomass (kg/ha)", outline=F,
                 lty=1, col=c("darkorange","white", "lightblue"), las=1,
                 names=c("Fished", "General Use", "Reserve"))
         points(factor(selectedData()$Zone), jitter(selectedData()$PARR/10), col = "darkgrey")
         
         mtext("biomass (kg/ha)", side = 2, line = 3, cex= 1.2)
         boxplot(SEAB/10~Zone, selectedData(), main = "GROUPERS", ylab=NULL, outline=T,
                 lty=1, col=c("darkorange","white", "lightblue"), 
                 las=1, names=c("Fished", "General Use", "Reserve"))
         points(factor(selectedData()$Zone), jitter(selectedData()$SEAB/10), col = "darkgrey")
         
         boxplot(SNAP/10~Zone, selectedData(), main = "SNAPPERS", ylab=NULL, outline=T,
                 lty=1, col=c("darkorange","white", "lightblue"), las=1,
                 names=c("Fished", "General Use", "Reserve"))
         points(factor(selectedData()$Zone), jitter(selectedData()$SNAP/10), col = "darkgrey")
         
         boxplot(JACK/10~Zone, selectedData(), main = "JACKS", ylab=NULL, outline=T,
                 lty=1, col=c("darkorange","white", "lightblue"), las=1,
                 names=c("Fished", "General Use", "Reserve"))
         points(factor(selectedData()$Zone), jitter(selectedData()$JACK/10), col = "darkgrey")
         
         boxplot(GRUN/10~Zone, selectedData(), main = "GRUNTS", ylab=NULL, outline=T, 
                 lty=1, col=c("darkorange","white", "lightblue"), las=1, 
                 names=c("Fished", "General Use", "Reserve"))
         points(factor(selectedData()$Zone), jitter(selectedData()$GRUN/10), col = "darkgrey")
         
       }
     }
     
   }) 
   
  ### OUTPUT Instructions ###
  output$Note <- renderText("PLEASE NOTE:") 
  output$Notetext <- renderText("This app may time-out if left idle too long, which will cause the screen to grey-out. To use the app again, refresh the page")
  output$report_instructions <- renderText("Generate a report by following these steps")
  output$create_reporttitle <- renderText("1) Select the ecological metrics that you would like to include")
  output$report_map <- renderText("2) Include a map with selected locations")
  output$generate_report <- renderText("3) Generate report with selected options")
  output$hover_text <- renderText("Hover over the graph to show specific metric values")
  output$generatingmap <- renderText("Generating map, please wait ...")
  
  ### Hover to show mean and CI for each X value 
  output$x_value <- renderText({
   if (is.null(input$plot_hover$x)) return("")
    else {
      lvls <- c("Outside Reserve", "Inside Reserve")
      name <- lvls[round(input$plot_hover$x)]
      HTML("Management level: <code>", name, "</code>"
         )
    }
  })
  
  #Create hover Info
  output$hover_info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return(" ")
      paste0(round(e$y, 1), "\n")
    }
    
    # xy_mean_str <- function(e) {
    #  if(is.null(e)) return("")
    # paste0(round(e$ymin, 1), round(e$ymax, 1))
    #}
    paste0("Value: ", xy_str(input$plot_hover))#,
    #"Biomass Mean:", xy_mean_str(input$plot_hover), "kg/ha")
    
  })
  
  output$hover_info2 <- renderUI({
    hover <- input$plot_hover
    
    xy_str <- function(e) {
      if(is.null(e)) return(" ")
      paste0(round(e$y, 1), "\n")
    }
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: #FFFFFF; ",
                    "left:", left_pct, "px ; top:", top_pct, "px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Mean: </b>", xy_str(input$plot_hover), "kg/ha", "<br/>",
                    "<b> +/-95%CI: </b>", "", "<br/>")))
    )
  })
  
  ## Add Tooltip with each Metric Info
  addTooltip(session, id="tabs_name", placement = "bottom",
             title = paste0("<p> Select an ecological metric tab",
                              " and explore data by selecting different options",
                              " from the left column </p>"),
             trigger = 'hover')
 
  ## Function for generating tooltip on top of bar graph
  info_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$grouping_level)) return(NULL)
    
    # Pick out the movie with this ID
    info <- isolate(summary_fish_biomass)
    info <- info[info$grouping_level == x$grouping_level, ]
    
    paste0("<b>", info$grouping_level, "</b><br>",
           info$locationstaus, "<br>",
           "$", format(info$biomass_kg_ha, big.mark = ",", scientific = FALSE)
    )
  }
  
  
  ### CREATE REPORT #####
  
  output$downloadReport <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = 'Ecological_monitoring_summary_report.doc',
      
    content = function(file) {
      
      # Copy the report file to a temporary directory before processing it,
      # in case we don't have write permissions to the current working dir 
      # (which can happen when deployed).
      
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
     
      library(rmarkdown)
       withProgress(message = 'Generating report, please wait, this could take up to 20 seconds', {
        render(tempReport, output_file = file, output_format = 'word_document')
         })
                        #envir = new.env(parent = globalenv())
    }
  )
  
  
  ### VIEW MAP ####
  output$map <- renderLeaflet({
    req(nrow(selectedData_map())> 0) 
       plot_map() })
    
  plot_map <- function () {
     ##Add leafleat function
    leaflet(data=ses_aoi) %>%
       ## Setup SetView
      setView(lng=mean(selectedData_map()$lon), lat=mean(selectedData_map()$lat), zoom=8) %>%
      ## Add Basemap
      addProviderTiles(input$basemap) %>%
        
      ## Subnational polygons
      #addPolygons(data=idn_adm0,
         #color = "black", weight = 1, smoothFactor = 0.8, opacity = 0.5, 
            #fillColor = "white", fillOpacity = 0.5, 
               #label = paste("District",idn_adm0$NAME_1)) %>%
        
      ## Add Local governemnt unit (district)
      #addPolygons(data=subset(idn_adm2, NAME_1 == "Sulawesi Tenggara"),
       #  color = "black", weight = 0.5, smoothFactor = 1, opacity = 0.5, 
        #    popup = paste("District", idn_adm2$NAME_2), fillColor = "white", fillOpacity = 0.01, 
         #      label = paste("District",idn_adm2$NAME_2),
          #        highlightOptions = highlightOptions(color = "darkorange", weight = 2, bringToFront = TRUE)) %>%
        
      ## Add Managed Access areas 
        addPolygons(data=ses_aoi,
               color = "red", weight = 1, smoothFactor = 0.8, opacity = 0.5, 
                  fillColor = "red", fillOpacity = 0.01, 
        label = paste("Managed Access",ses_aoi$Name),
            highlightOptions = highlightOptions(color = "red", weight = 2, bringToFront = TRUE)) %>%

      ## Add survey locations as markers
        addCircleMarkers(lng= selectedData_map()$lon, lat= selectedData_map()$lat, radius = ~runif(100, 1, 1),
            color = "red", fillOpacity = 0.5, 
               popup = paste0("<strong> MA name: </strong>",
                              selectedData_fish.biomass()$ma_name,
                              "<br><strong>Location: </strong>",  
                                 selectedData_fish.biomass()$location_name,
                              "<br>",
                              "<br><strong> Fish biomass: </strong>", 
                                 round(tapply(selectedData_fish.biomass()$biomass_kg_ha, 
                                              selectedData_fish.biomass()$location_name, mean),1)," kg/ha",
                              "<br><strong> Fish size: </strong>",
                                 round(tapply(selectedData_fish.size()$length,
                                           selectedData_fish.size()$location_name, mean),1), " cm",
                              "<br><strong> Coral cover: </strong>",
                                 round(tapply(subset(selectedData_benthic.cover(),category=="Hard coral")$percentage,
                                                    subset(selectedData_benthic.cover(),category=="Hard coral")$location_name, mean),1)," %",
                               "<br><strong> Algae cover: </strong>",
                                 round(tapply(subset(selectedData_benthic.cover(),category=="Macroalgae")$percentage,
                                           subset(selectedData_benthic.cover(),category=="Macroalgae")$location_name, mean),1)," %"),
              label =  paste("Location: ", selectedData_fish.biomass()$location_name),
              popupOptions = popupOptions(closeButton = FALSE)) %>% 
       
        ## Add legend to map
        addLegend(title = "LEGEND", colors = c("white","","red"), 
            labels = c("Districts","","Survey location"), opacity = 0.5,
               group =  selectedData_fish.biomass()$location_name) %>% 
        ## Add scale ba
        addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial=FALSE)) %>%
        ## Add miniMap
        addMiniMap(tiles = input$basemap, toggleDisplay = TRUE, position = "bottomright")
     
         ## Add graticule and add measure and add mouse coordinates
        #addGraticule(interval = 1) %>%  addMeasure() %>% leafem::addMouseCoordinates() %>% 
        
     
  }
  
  #Download Figure
  output$downloadMap <- downloadHandler(
    filename = function() { 
      paste0("Map", input$Country, "_",input$grouping_level, "_level", ".png", sep="") 
    },
    content = function(file) { file }
  )
  
  ##Upload fish picture #####
  src = "https://www.fishbase.de/images/thumbnails/jpg/tn_"
  
  output$fish_image <-renderText({
      if (input$fish_species != "All fish species") {
          c('<img src="', paste0(src, substr(input$fish_species,1,2), 
                                 substr(sub("^\\S+\\s+",'',input$fish_species),1,3), "_u0.jpg"),'">')}
     else {NULL}
     
                                     
     })
  })


