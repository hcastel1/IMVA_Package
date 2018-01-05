
# This is the user-interface definition of the Shiny web application.
# The heavy-lifting is left to the server file
# http://shiny.rstudio.com

library(shiny)
library(rbokeh)
library(plotly)


shinyUI(navbarPage("Interactive Exploratory Multivariate Analysis",
                   
                   tabPanel("Introduction",
                            uiOutput("ui.intro")),
                   #tabPanel("File Upload",
                   #        uiOutput("ui.loadData")), # mainPage tabPanel
                   
                   tabPanel("Principal Component Analysis", # mainPage tabPanel
                            navlistPanel(widths = c(2, 10),
                                         "PC Analysis Pages",
                                         tabPanel("File Upload",
                                                  uiOutput("ui.loadData.num")),
                                         tabPanel("Determine Number of Components to Interpret",
                                                  textOutput("textNCP"), br(),br(),          
                                                  rbokehOutput('scree_bokeh_plot', width = "100%", height = "800px")), #closes Scree Plot tabPanel
                                         tabPanel("Studying Variables",
                                                  uiOutput("ui.pcaMain")), #closes Variable Contribution Plots tabPanel
                                         tabPanel("Studying Individuals",
                                                  uiOutput("ui.pcaMain2")) #closes Interactive Data View tabPanel
                            ) #closes navlistPanel
                   ), #closes Principal Component Analysis mainPage tabPanel
                   
                   #tabPanel("Correspondence Analysis", # mainPage tabPanel
                   #         navlistPanel(widths = c(2, 10),
                   #                      "Correspondence Analysis Pages",
                   #                      tabPanel("Variables (Columns) Analysis") #closes ___ tabPanel
                   #         ) #closes navlistPanel
                   #       ), #closes Correspondence Analysis mainPage tabPanel
                   
                   tabPanel("Multiple Correspondence Analysis", # mainPage tabPanel
                            navlistPanel(widths = c(2, 10),
                                         "MC Analysis Pages",
                                         tabPanel("File Upload",
                                                  uiOutput("ui.loadData.cat")),
                                         tabPanel("Variable-Category Mapping",
                                                  verbatimTextOutput("summary")),
                                         tabPanel("Determine Number of Components to Interpret",
                                                  verbatimTextOutput("eigMCA"), br(),br(),
                                                  plotlyOutput("plot.eig.MCA", width = "100%", height = "800px")), #closes Determine tabPanel
                                         tabPanel("Studying Individuals",
                                                  uiOutput("ui.mcaIND")), #closes Studying Individuals tabPanel
                                         tabPanel("Studying Categories (Variables)",
                                                  uiOutput("ui.mcaVAR")), #closes Studying Categories tabPanel
                                         tabPanel("Simultaneous Representation ",
                                                  #uiOutput("ui.mcaSIM"),
                                                  plotOutput("plot.mca.SIM", width = "100%", height = "800px")) #closes Sim rep tabPanel
                            ) #closes navlistPanel
                   ), #closes Multiple Correspondence Analysis mainPage tabPanel
                   
                   tabPanel("Centroid-based Cluster Analysis", # mainPage tabPanel
                            navlistPanel(widths = c(2, 10),
                                         "Cluster Analysis Pages",
                                         tabPanel("Cluster Plot",
                                                  uiOutput("ui.clustMain")), #closes Cluster Plot tabPanel
                                         tabPanel("Interactive Data View",
                                                  uiOutput("ui.clustData")), #closes Interactive Data View tabPanel
                                         tabPanel("Determine Number of Clusters (k)",
                                                  rbokehOutput('a_bokeh_plot', width = "100%", height = "800px"))
                                         #plotlyOutput('gap_stat', width = "100%", height = "800px")) #closes gap_stat tabPanel
                            ) #closes navlistPanel
                   ) #closes Cluster Analysis mainPage tabPanel
                   
) #closes navbarPage
) #closes ShinyUI
