 ## DEFINE UI ###
  fillPage(
    fluidPage(theme = shinytheme("lumen"),
      fluidRow (
        column(width = 3,
               #Select the type of plot to use
               selectInput(inputId = 'grouping_level', 
                           strong("ANALYSIS LEVEL"),
                           choices = c("Country" = 'country', 
                                       "Subnational Government" = 'level1_name',
                                       "Local Government"= "level2_name", 
                                       "Managed Access Area"='ma_name'),
                           selected = "ma_name"),
               ## help text
               bsTooltip(id="grouping_level", 
                         title ="Select a grouping level to calculate means and errors",
                         placement ="right", 
                         trigger = "hover", 
                         options = list(container = "body")
                         )
        ),
        column(width = 2,
               #select country
                 selectInput(inputId = "Country", 
                             label = strong("Country"),
                             choices = c("Honduras"= 'HND',
                                         "Indonesia" = 'IDN',
                                         "Philippines" = 'PHL',
                                         "Mozambique" = 'MOZ',
                                         "Brazil" = "BRA"),
                             selected = "IDN", 
                             multiple = FALSE, 
                             selectize = TRUE),
                 bsTooltip(id="Country", 
                           title ="Select all countries using Ctrl+A or Comm+A",
                           placement ="right", 
                           trigger = "hover", 
                           options = list(container = "body")
                           )
               ),
        column(width = 2,
               #Select subnational government
                 uiOutput("Subnational_Government"),  
                 bsTooltip(id="Subnational_Government", 
                           title ="Generally subnational goverments are provinces or states. Select all subnational governments using Ctrl+A or Comm+A",
                           placement ="right", 
                           trigger = "hover", 
                           options = list(container = "body")
                           )
               ),
        column(width = 2,
                 #Select local government
                 uiOutput(outputId = "Local_Government"),   
                 bsTooltip(id="Local_Government", 
                           title ="Select all local governments using Ctrl+A or Comm+A",
                           placement ="right", 
                           trigger = "hover", 
                           options = list(container = "body")
                           )
                ),
        column(width = 2,
                  #Select managed access 
                 uiOutput(outputId = "Managed_Access"),  
                 bsTooltip(id="Managed_Access", 
                           title ="Select all managed access areas using Ctrl+A or Comm+A",
                           placement ="right", 
                           trigger = "hover", 
                           options = list(container = "body")
                           )
            )
        ),
         
      fluidRow(
        column(width = 3,
               wellPanel(
                 
                 conditionalPanel(condition = "input.tabs == 'Coral Reefs' |
                                               input.tabs ==  'Seagrass Beds' |
                                               input.tabs == 'Mangrove Forests'",
                  
                   radioButtons(inputId = "Analysis", 
                                label = strong("TYPE OF ANALYSIS"), 
                                choices = c("Reserve Performance", 
                                            "Site Trends"),
                                selected = "Reserve Performance"),
                   ## help text
                   bsTooltip(id="Analysis", 
                             title ="Select the type of analysis you would like to perform",
                             placement ="right", 
                             trigger = "hover", 
                             options = list(container = "body")
                             )
                  ),

                 conditionalPanel(condition = "input.tabs == 'Coral Reefs' |
                                               input.tabs == 'Seagrass Beds'",
                 hr(),
                 radioButtons(inputId = "reef_seagrass_metric",
                              label = strong("SELECT METRIC"),
                              choices = c("Fish Biomass", 
                                          "Fish Density",
                                          "Fish Diversity",
                                          "Fish Size",
                                          "Habitat Cover",
                                          "Habitat Diversity"),
                              selected = "Fish Biomass"),
                 ## help text
                 bsTooltip(id="reef_seagrass_metric", 
                           title ="Select a metric you would like to explore",
                           placement ="right", 
                           trigger = "hover", 
                           options = list(container = "body", expire = 2000)
                           )
                 ),
                 
                 conditionalPanel(condition = "input.tabs == 'Mangrove Forests'",
                                  
                  hr(),
                  radioButtons(inputId = "mangrove_metric",
                               label = strong("SELECT METRIC"),
                               choices = c("Mangrove Extension", 
                                           "Above Ground Biomass",
                                           "Carbon Stock",
                                           "Canopy Cover",
                                           "Tree Height",
                                           "Tree DBH",
                                           "Tree Diversity",
                                           "Regeneration"),
                               selected = "Mangrove Extension"),
                  ## help text
                  bsTooltip(id="mangrove_metric", 
                            title ="Select a metric you would like to explore",
                            placement ="right", 
                            trigger = "hover", 
                            options = list(container = "body", expire = 2000)
                  )
                 ),
                 
                 conditionalPanel(condition = "input.tabs == 'Coral Reefs' |
                                              input.tabs == 'Seagrass Beds' |
                                              input.tabs == 'Mangrove Forests'",
                  #Select the type of plot to use
                 hr(), 
                 radioButtons(inputId = "plot_type", 
                              strong("PLOT TYPE"),
                              choices = c("Bar plots (mean ± SE)" = "Bar plots", 
                                          "Range plots (mean± SE)" = "Mean ± 95% CI"), 
                              selected = "Bar plots", 
                              inline = FALSE),
                 ## help text
                 bsTooltip(id="plot_type", 
                           title ="Select the type of plot you would like to see",
                           placement ="right", 
                           trigger = "hover", 
                           options = list(container = "body")
                           ),
                 
                 #Select the type of y axis (free or fixed)
                 hr(), 
                 radioButtons(inputId = "y_axis", 
                              strong ("Y AXIS (free or fixed)"),
                              choices = c("Free"='free_y', 
                                          "Fixed"='fixed'), 
                              selected = "free_y", 
                              inline=TRUE),
                 ## help text
                 bsTooltip(id="y_axis", 
                           title ="Change the scale of the y axis to compare between panels",
                           placement ="right", 
                           trigger = "hover", 
                           options = list(container = "body")
                           ),
               ),
                
                #Select a fish family to plot
                conditionalPanel(condition = "input.tabs == 'Coral Reefs' &
                                 input.reef_seagrass_metric == 'Fish Biomass' | 
                                 input.reef_seagrass_metric == 'Fish Density' | 
                                 input.reef_seagrass_metric == 'Fish Diversity'",
                  hr(), 
                  uiOutput(outputId = "fish_family"),  
                      bsTooltip(id = "fish_family", 
                                title ="Select all fish families using Ctrl+A or Comm+A",
                                placement ="right", 
                                trigger = "hover", 
                                options = list(container = "body")
                                )
                ),
                
                #Select a fish species to plot
                conditionalPanel(condition = "input.tabs == 'Coral Reefs' &
                                      input.reef_seagrass_metric == 'Fish Size'",
                  hr(), 
                  uiOutput(outputId = "fish_species"),
                #Include fish species picture from fishbase
                  htmlOutput("fish_image"),
                bsTooltip(id = "fish_species", 
                         title ="Select all fish species using Ctrl+A or Comm+A",
                         placement ="right", 
                         trigger = "hover", 
                         options = list(container = "body")
                )
              ),
                
             #Select a habitat categories  
                conditionalPanel(condition = "input.tabs == 'Coral Reefs' & 
                                              input.reef_sesgrass_metric == 'Habitat Cover'",
                  hr(), 
                  uiOutput(outputId = "habitat_category"),  
                    bsTooltip(id = "habitat_category", 
                              title ="Select all benthic habitat categories using Ctrl+A or Comm+A",
                              placement ="right", 
                              trigger = "hover", 
                              options = list(container = "body")
                              )
                ),
             
                              
             #Select the basemap
                conditionalPanel(condition = "input.tabs == 'View Map'",
                selectInput(inputId = "basemap", label = strong("CHOOSE A BASEMAP"),
                             choices = c("Gray Canvas basemap" = providers$Esri.WorldGrayCanvas,
                                         "National Geographic basemap" = providers$Esri.NatGeoWorldMap,
                                         "Ocean basemap"= providers$Esri.OceanBasemap,
                                         "Satellite basemap"= providers$Esri.WorldImagery,
                                          "World Topo basemap" = providers$Esri.WorldTopoMap),
                             selected = providers$Esri.OceanBasemap, 
                            selectize = TRUE),
                bsTooltip(id = "basemap", 
                          title ="Select a basemap for mapping. This will take few seconds",
                          placement ="right", 
                          trigger = "hover", 
                          options = list(container = "body")), 
                ),
                    
               ## Hover over graph to show raw values
                strong(textOutput(outputId ="hover_text")), 
            br(),
                 #strong(textOutput(outputId = "Legend")),br(),
                 htmlOutput("x_value"),
                 #textOutput(outputId ="Legendtext"),
                 verbatimTextOutput("hover_info"),
            br(),
             #Note about page reloading
              strong(textOutput(outputId = "Note")),
                 textOutput(outputId ="Notetext")
               )
        ),
                  
        column(width=9,
                   mainPanel(width =14,
                      tabsetPanel(type = "tabs", id = "tabs",
                      
                      ## Fish biomass
                  tabPanel ("Coral Reefs", icon = icon("fish"),
                    
                      conditionalPanel(condition = "input.reef_seagrass_metric == 'Fish Biomass'", 
                        downloadLink(outputId ="downloadPlot0", "| Download figure",  
                                     style="float:right"),
                        downloadLink("downloadData0", "Download data | ", 
                                       style="float:right"),
                        plotOutput(outputId = "plot_fish_biomass", 
                                   height = "700px", 
                                   width = "750px",
                                   hover = hoverOpts(id = "plot_hover")
                                   )
                     ),
                     ## Fish density
                      conditionalPanel(condition = "input.reef_seagrass_metric == 'Fish Density'", 
                        downloadLink(outputId ="downloadPlot1", "| Download figure", 
                                       style="float:right"),
                              downloadLink("downloadData1", "Download data |", 
                                           style="float:right"),
                          plotOutput(outputId = "plot_fish.density", 
                                     height = "700px", 
                                     width = "750px",
                                     hover = hoverOpts(id = "plot_hover")
                          )
                      ),
                    ## Fish size 
                     conditionalPanel(condition = "input.reef_seagrass_metric == 'Fish Size'", 
                          downloadLink(outputId ="downloadPlot2", "| Download figure",  
                                              style="float:right"),
                                 downloadLink("downloadData2", "Download data |", 
                                              style="float:right"),
                         plotOutput(outputId = "plot_fish.size", 
                                    height = "700px", 
                                    width = "750px",
                                    hover = hoverOpts(id = "plot_hover")
                                    )
                     ),
                    
                  # Fish diversity
                    conditionalPanel(condition = "input.reef_seagrass_metric == 'Fish Diversity'", 
                      downloadLink(outputId ="downloadPlot3", "| Download figure",  
                                              style="float:right"),
                                 downloadLink("downloadData3", "Download data |", 
                                              style="float:right"),
                         plotOutput(outputId = "plot_fish.deversity", 
                                    height = "700px", 
                                    width = "750px",
                                    hover = hoverOpts(id = "plot_hover")
                         )
                    ),
                # Reef Habitat cover 
                    conditionalPanel(condition = "input.reef_seagrass_metric == 'Habitat Cover'", 
                       downloadLink(outputId ="downloadPlot4", "| Download figure",  
                                              style="float:right"),
                          downloadLink("downloadData4", "Download data |", 
                                              style="float:right"),
                         plotOutput(outputId = 'plot_hardcoral.cover', 
                                    height ='700px', 
                                    width = '750px',
                                    hover = hoverOpts(id = "plot_hover")
                                    )
                    ),
                  
                  # Reef Habitat diversity
                      conditionalPanel(condition = "input.reef_seagrass_metric == 'Habitat Diversity'", 
                        downloadLink(outputId ="downloadPlot5", "| Download figure",
                                              style="float:right"),
                                 downloadLink("downloadData5", "Download data |",
                                              style="float:right"),
                          plotOutput(outputId = 'plot_benthic.diversity',
                                     height ='700px',
                                     width = '750px',
                                     hover = hoverOpts(id = "plot_hover")
                                     )
                        )
                ),
                     
                      tabPanel ("Seagrass Beds", icon = icon("water")),
                
                      tabPanel ("Mangrove Forests", icon = icon("seedling")),
                      
                      #Create Map Tab       
                      tabPanel("View Map", 
                               icon = icon("map"),
                               br(),
                               downloadLink(outputId ="downloadMap", "Download Map",  
                                            style="float:right"),
                               leafletOutput(outputId = 'map', 
                                             height='700px', 
                                             width='750px')
                               ),

                      # Create Report Tab
                        tabPanel(strong("Reporting",
                                        style='color:#005BBB'), 
                                 icon = icon("file-alt", class = "solid"),
                                 verbatimTextOutput("create_report"),
                              br(),
                              strong(h4(textOutput(outputId = "report_instructions"))), 
                              br(),
                              strong(h4(textOutput(outputId = "create_reporttitle"))), 
                              br(),
                              checkboxGroupInput("metrics", 
                                                 strong("Ecological metrics"),
                                                 choices = c("Fish Biomass", 
                                                             "Fish Density", 
                                                             "Fish Size", 
                                                             "Fish Diversity", 
                                                             "Hard Coral Cover", 
                                                             "Habitat Diversity"),
                                                 selected = NULL, 
                                                 inline = FALSE), 
                              br(),
                              strong(h4(textOutput(outputId = "report_map"))), 
                              br(),
                              checkboxGroupInput("locationmap", 
                                                 strong("Map"), 
                                                 choices = c("Location Map")), 
                              br(),
                              strong(h4(textOutput(outputId = "generate_report"))), 
                              br(),
                              downloadButton(outputId='downloadReport', 
                                             label='Generate Report', 
                                             class = 'butt'),
                              tags$head(tags$style(".butt{background-color:#F58233;} .butt{color: black;!important;}")))
                                )
                            )
                        )
                      )
                    )
                )