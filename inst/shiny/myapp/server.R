# This is the server logic for a Shiny web application.
#
# http://shiny.rstudio.com
#

options(shiny.maxRequestSize = 30*1024^2)

library(shiny)
library(DT) #interactive javascript data tables
library(ggfortify) #functions to develop easy ggplots
library(plotly) #interactive ggplot objects
library(ggplot2) #create grammar of graphics plots to pass to rbokeh
library(ggrepel) #for repeling labels in some plots that are too busy without 
library(cluster) #implements clustering algorithms
library(rbokeh) #another interactive plot library
library(FactoMineR) #efficient implementation of multivariate analysis
library(factoextra) #creates robust ggplot2 contribution plots
library(dplyr) #efficient data-wrangling



shinyServer(function(input, output) {
  
  ################################################################################################
  #####     Generate ui for Load Data page broken out by data type for each analyses          ####
  ################################################################################################
  
  output$ui.loadData.num = renderUI({ 
    tagList(
      sidebarPanel(
        fileInput('file1', 'Choose CSV File with Numeric Columns as Primarily Active Variables',
                  accept = c(
                    'text/csv',
                    'text/comma-separated-values',
                    'text/tab-separated-values',
                    'text/plain',
                    '.csv',
                    '.tsv'
                  )
        ),
        tags$hr(),
        checkboxInput('header', 'Header', TRUE),
        radioButtons('sep', 'Separator',
                     c(Comma=',',
                       Semicolon=';',
                       Tab='\t'),
                     ','),
        radioButtons('quote', 'Quote',
                     c(None='',
                       'Double Quote'='"',
                       'Single Quote'="'"),
                     '"'),
        tags$hr()
      ), #closes sidebarPanel
      mainPanel(
        #verbatimTextOutput("intro"),
        DT::dataTableOutput('itbl')
        #DT::dataTableOutput('itbl2')
        #tableOutput('contents')
      ) #closes mainPanel
    ) #closes tagList
  }
  ) #closes loadData renderUI
  
  
  output$ui.loadData.cat = renderUI({ 
    tagList(
      sidebarPanel(
        fileInput('file2', 'Choose CSV File with Categorical Columns as Primarily Active Variables',
                  accept = c(
                    'text/csv',
                    'text/comma-separated-values',
                    'text/tab-separated-values',
                    'text/plain',
                    '.csv',
                    '.tsv'
                  )
        ),
        tags$hr(),
        checkboxInput('header2', 'Header', TRUE),
        radioButtons('sep2', 'Separator',
                     c(Comma=',',
                       Semicolon=';',
                       Tab='\t'),
                     ','),
        radioButtons('quote2', 'Quote',
                     c(None='',
                       'Double Quote'='"',
                       'Single Quote'="'"),
                     '"'),
        tags$hr()
      ), #closes sidebarPanel
      mainPanel(
        #verbatimTextOutput("intro"),
        #DT::dataTableOutput('itbl'),
        DT::dataTableOutput('itbl2')
        #tableOutput('contents')
      ) #closes mainPanel
    ) #closes tagList
  }
  ) #closes loadData renderUI
  
  
  ###################################################
  #####   server side logic for loadData page   ####
  ###################################################
  
  #captures HTML output generated from an .Rmd file to be used as intro message for the app
  
  #NOTE: Apparently includeHTML breaks the fileupload and other ui functionality when you have a html document embedded inside a shinydashboard tabpanel. I was able to fix this issue by manually removing the head part of my html file (everything between <head> and </head> and only including the body)
  
  getPage<-function() {
    return(includeHTML("IMVA_app_intro.html")) #knitted first from IMVA_app_intro.RMD 
  }
  
  #dyanmic ui to be intro tabpanel 
  output$ui.intro<-renderUI({
    getPage()
  })
  
  
  # Creates javascript data table for Load Data tabPanel 
  output$itbl = DT::renderDataTable({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    DT::datatable(read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote), colnames = c('ID' = 1), options = list(
      scrollX=TRUE,
      scrollCollapse=TRUE)
    )
    
  })
  
  # Creates javascript data table for Load Data tabPanel 
  output$itbl2 = DT::renderDataTable({
    
    inFile2 <- input$file2
    
    if (is.null(inFile2))
      return(NULL)
    
    DT::datatable(read.csv(inFile2$datapath, header=input$header2, sep=input$sep2, quote=input$quote2), colnames = c('ID' = 1), options = list(
      scrollX=TRUE,
      scrollCollapse=TRUE)
    )
    
  })
  
  #Create a reactive data object if input datafile for reference everywhere
  Data <- reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    
  })
  
  #Create a reactive data object if input datafile for reference everywhere
  Data2 <- reactive({
    
    inFile2 <- input$file2
    
    if (is.null(inFile2))
      return(NULL)
    
    read.csv(inFile2$datapath, header=input$header2, sep=input$sep2, quote=input$quote2)
    
  })
  
  
  #Break out uploaded data into 2 separate data objects:
  
  #Reactive data object with only numeric variables for PCA and Cluster Analysis requiring only numeric variables
  Data.num <- reactive({
    datax <- Data() #set reactive data into a regular R object to lose reactive object syntax and make sub-setting easier
    is.fact <- sapply(datax, is.factor)
    datax <- datax[,-is.fact] #remove any factor variables 
    datax <- sapply(datax, as.numeric) #make sure variables left are strictly numeric
  })
  
  
  #Reactive data object with only factor variables for Correspondence and Multiple Correspondence Analysis requiring only factor variables
  Data.fact <- reactive({
    datax2 <- Data2() #set reactive data into a regular R object to lose reactive object syntax and make sub-setting easier
    is.num <- sapply(datax2, is.numeric)
    datax2 <- datax2[,-is.num] #remove any numeric variables
    datax2 <- sapply(datax2, as.factor) #make sure variables left are strictly factors
  })
  
  ################################################################################################################################################# ##################################################         BEGIN CLUSTER ANALYSIS             ################################################### 
  #################################################################################################################################################
  #################################################################################################################################################
  
  
  ##############################################################################
  # Generate entire Cluster Analysis UI dynamically, server-side using renderUI#
  ##############################################################################
  
  ##################################
  #####ui for Cluster Plots page####
  ##################################
  output$ui.clustMain = renderUI({ 
    tagList(
      sidebarPanel(
        tags$head(
          tags$style("body {background-color: #EFFBFB; }"),
          tags$style(type='text/css', "#title1 { height: 25px; }")), #closes tags
        wellPanel(
          #Begin conditionalPanels for Cluster Analysis
          selectInput("alg", label = "Select Algorithm", choices = c("Kmeans","Pam","Clara", "Fuzzy"), multiple = FALSE),
          numericInput(inputId = "num1", label = "Choose Number of Clusters (k)", value = 3, min = 2, width=400),
          selectInput("type", label = "Probability Ellipse Type", choices = c("norm","t","convex"), multiple = FALSE),
          br(),
          "Algorithm Specific Inputs",
          br(),
          br(),
          conditionalPanel(
            condition = "input.alg == 'Kmeans'",
            uiOutput("ui.kmeans")),
          conditionalPanel(
            condition = "input.alg == 'Pam'",
            uiOutput("ui.pam")),
          conditionalPanel(
            condition = "input.alg == 'Clara'",
            uiOutput("ui.clara")),
          conditionalPanel(
            condition = "input.alg == 'Fuzzy'",
            uiOutput("ui.fuzzy"))
        ) #closes wellPanel
      ), #closes sidebarPanel
      mainPanel(
        #plotlyOutput("Plot", width="100%", height = "700px"),
        rbokehOutput('clus_bokeh_plot', width = "100%", height = "800px")
      ) #closes mainPanel
    ) #closes tagList
  }
  ) #closes clustMain renderUI
  
  ###########################################
  #### ui for Interactive Data View page ####
  ###########################################
  output$ui.clustData = renderUI({ 
    tagList(
      sidebarPanel(
        tags$head(
          tags$style("body {background-color: #EFFBFB; }"),
          tags$style(type='text/css', "#title1 { height: 25px; }")), #closes tags
        wellPanel(
          selectizeInput('rows', 'Individual Row IDs to render', choices = seq_len(nrow(Data())), multiple = TRUE)
        ) #closes wellPanel
      ), #closes sidebarPanel
      mainPanel(
        DT::dataTableOutput('tbl'), br(),
        verbatimTextOutput("info"), br(), 
        plotOutput("plot.clusBrush", width="100%", height = "700px", brush = "plot_brush"), DT::dataTableOutput('x1') 
      ) #closes mainPanel
    ) #closes tagList
  }
  ) #closes clustData renderUI
  
  ###################################################################################
  #### Reactive ui components of algorithm specific inputs for Cluster Plot page ####
  ###################################################################################
  
  output$ui.kmeans = renderUI({
    tagList(
      numericInput(inputId = "num2", label = "Maximum Number of Iterations Allowed", value = 20, min = 10, width=400),
      numericInput(inputId = "num3", label = "Number of Initial Random Assignments", value = 20, min = 1, width=400) 
    ) 
  }
  )
  
  output$ui.pam = renderUI({
    tagList(
      selectInput('vect', 'Set Initial Mediods (length-k vector of row indices (Ex: 1 2 3)', choices = seq_len(nrow(Data())),
                  selected = c(1,100,1000), multiple = TRUE)
    ) 
  }
  )
  
  output$ui.clara = renderUI({
    tagList(
      numericInput(inputId = "num4", label = "Number of Samples to be drawn", value = 10, min = 5, width=400),
      numericInput(inputId = "num5", label = "Number of observations in each sample", value = 100, min = 25, width=400)
    ) 
  }
  )
  
  output$ui.fuzzy = renderUI({
    tagList(
      numericInput(inputId = "num6", label = "Membership exponent", value = 2, min = 1, width=400),
      numericInput(inputId = "num7", label = "Maximum number of iterations Allowed", value = 100, min = 50, width=400)
    ) 
  }
  )
  
  #################################################################################################################################################
  
  ##########################################################################################################
  #            Server-side logic for plots, javascript data tables, etc. of Cluster Analysis               #
  ##########################################################################################################
  
  #############################################################################################################
  #Build reactive ggplot object from autoplot function in ggfortify package; to be passed eventually to plotly#
  #############################################################################################################
  
  y.v <- reactive({
    
    #What is happening here is nuanced. I am not creating a new reactive object. I am, however, manipulating a reactive object within another reactive object and hence within a reactive context; that is why this works, otherwise i couldn't pass df.km to autoplot fn this way
    
    df.km <- Data.num()
    
    if (input$alg == "Clara") {
      # ggfortify to create underlying ggplot graphics with geometries, etc. 
      autoplot(clara(df.km, input$num1, samples = input$num4, sampsize = input$num5), data=df.km, label=TRUE, frame=TRUE, 
               frame.type=input$type) + ggtitle('Clusters in Principal Component Space')  
      
    } else if (input$alg == "Pam") {
      # ggfortify to create underlying ggplot graphics with geometries, etc. 
      autoplot(pam(df.km, input$num1, medoids = input$vect, do.swap = FALSE), data=df.km, label=TRUE, frame=TRUE, frame.type=input$type) +
        ggtitle('Clusters in Principal Component Space')  
      
    } else if (input$alg == "Fuzzy") {
      # ggfortify to create underlying ggplot graphics with geometries, etc. 
      autoplot(fanny(df.km, input$num1, memb.exp = input$num6, maxit = input$num7), data=df.km, label=TRUE, frame=TRUE, 
               frame.type=input$type) + ggtitle('Clusters in Principal Component Space')  
      
    } else 
      # ggfortify to create underlying ggplot graphics with geometries, etc. 
      autoplot(kmeans(df.km, input$num1, iter.max = input$num2, nstart = input$num3), data= df.km, label=TRUE, frame=TRUE, 
               frame.type = input$type) + ggtitle('Clusters in Principal Component Space')  
  })
  
  #convert plotly cluster plot to rbokeh plot due to problems with ggplotly
  
  #output$Plot <- renderPlotly({    
  #  ggplotly(y.v())
  #})
  
  output$clus_bokeh_plot <- renderRbokeh({
    
    #basic ggplot2 plot of cluster analysis projected onto PC space that can be queried to get underlying probability ellipse coordinates for use    in robkeh
    p.1 <- ggplot(y.v()$data, aes(PC1, PC2, color = cluster)) +
      geom_point() +
      stat_ellipse(type = "norm", linetype = 2) +
      stat_ellipse(type = "t") #+
    
    # Get ellipse coordinates from plot
    pb = ggplot_build(p.1)
    el.norm = pb$data[[2]][c("x","y","group")] #1 = point layer, 2 = stat_ellipse(type = "norm") layer, 3 = stat_ellipse(type = "t") layer
    el.t = pb$data[[3]][c("x","y","group")] #1 = point layer, 2 = stat_ellipse(type = "norm") layer, 3 = stat_ellipse(type = "t") layer
    
    #robkeh plot of clustering with probability ellipse as calculated using autoplots fortify function
    
    p.2 <- figure(title = 'Clustering of Data') %>% 
      ly_points(PC1, PC2, data = y.v()$data, color = cluster, hover = c(PC1, PC2, cluster)) %>%
      ly_lines(x, y, data = el.norm, color = "black", type = 1, width = 1, alpha = 1, 
               legend = "Multivariate Normal-dist", group = el.norm$group) %>%
      ly_lines(x, y, data = el.t, color = "blue", type = 2, width = 1, alpha = 1, legend = "Multivariate t-dist", group = el.t$group)
    p.2
    
  })
  
  
  
  output$plot.clusBrush <- renderPlot({
    y.v()
  })
  
  output$info <- renderPrint({
    cat('Brush across the plot to render data points below dynamically. Plot defaults to inputs for Cluster Plot.')
  })
  
  ##############################################################
  # Creates javascript data table for brushed points rendering #
  ##############################################################
  
  output$x1 = DT::renderDataTable(brushedPoints(y.v()$data, input$plot_brush, allRows = FALSE), server = FALSE, options = list(
    scrollX=TRUE,
    scrollCollapse=TRUE)) 
  
  #####################################
  # Create WSS plot for determining K #  
  #####################################
  
  output$a_bokeh_plot <- renderRbokeh({
    
    df.km <- Data.num()
    
    # Determine number of clusters
    wss <- (nrow(df.km)-1)*sum(apply(df.km,2,var))
    for (i in 2:15) 
      wss[i] <- sum(kmeans(df.km,centers=i)$withinss)
    
    x <- data.frame(cbind(c(1:15),wss)) #underlying dataframe for rbokeh plot
    
    figure(title="Determining # of Clusters") %>%
      ly_points(V1, wss, data = x, hover = list(wss)) %>%
      ly_lines(V1, wss, data = x, type=2) %>%
      y_axis("Within Groups Sum of Squares (WSS)") %>%
      x_axis("Number of Clusters") 
    
  })
  
  ##########################################
  # Create gap statistic for determining K #  
  ##########################################
  
  #output$gap_stat <- renderPlotly({
  
  #df.km <- Data.num()
  
  #gap.stat <- clusGap(df.km, FUNcluster = kmeans, nstart=20, K.max = 8, B = 25)
  
  #P <- fviz_gap_stat(gap.stat)
  #ggplotly(P)
  
  #})
  
  #########################################################################################
  # Creates javascript data table for selectizeInput for Data View (Interactive) tabPanel #
  #########################################################################################
  
  output$tbl = DT::renderDataTable({
    DT::datatable(Data()[c(input$rows),], colnames = c('ID' = 1), options = list(
      scrollX=TRUE,
      scrollCollapse=TRUE)
    )
  })
  
  output$foo = DT::renderDataTable(
    Data(), server = FALSE, selection = list(target = 'row')
  )
  
  proxy = dataTableProxy('foo')
  
  observeEvent(input$select1, {
    selectRows(proxy, as.numeric(input$rows))
  })
  
  
  ################################################################################################################################################# ##################################################         BEGIN PC ANALYSIS                  ################################################## 
  #################################################################################################################################################
  #################################################################################################################################################  
  
  ##########################################################################################
  # Generate entire Principal Component Analysis UI dynamically, server-side using renderUI#
  ##########################################################################################
  
  ################################################
  #####ui for Variable Contribution Plots page####
  ################################################
  output$ui.pcaMain = renderUI({ 
    tagList(
      sidebarPanel(
        tags$head(
          tags$style("body {background-color: #EFFBFB; }"),
          tags$style(type='text/css', "#title1 { height: 25px; }")
        ),
        absolutePanel(
          draggable = TRUE,
          wellPanel( #since we are using renderUI, get rid of conditionalPanels and tabsetPanel and tabPanel in mainPanel as they are not needed
            #Inputs for Variable Contribution Tab ; b/c it's a combined app need to iterate inputID's for PCA section
            numericInput(inputId = "num8", label="Number of Top Correlated Variables (w/ Dim 1) to plot", value = 10, min = 1,width = 400),
            numericInput(inputId = "num9", label = "Set Dimension 1 Principal Component", value = 1, min = 1, width=400)#,
            #numericInput(inputId = "num10", label = "Set Dim 2 Principal Component", value = 2, min = 2, width=400)
          ) #closes wellPanel
        ) #closes absolutePanel
      ), #closes sidebarPanel
      mainPanel(
        rbokehOutput('a_bokeh_plot_pca', width = "100%", height = "800px"),br(),plotlyOutput("plot.var4", width = "100%", height = "400px"),
        br(),br(),
        plotlyOutput("plot.var2", width = "100%", height = "400px"), br(), br(), plotlyOutput("plot.var3",width = "100%", height = "400px")
      ) #closes mainPanel
    ) #closes tagList
  }
  ) #closes pcaMain renderUI
  
  ###################################################
  #####ui for Observation Contribution Plots page####
  ###################################################
  output$ui.pcaMain2 = renderUI({
    tagList(
      sidebarPanel(
        tags$head(
          tags$style("body {background-color: #EFFBFB; }"),
          tags$style(type='text/css', "#title1 { height: 25px; }")
        ),
        absolutePanel(
          draggable = TRUE,
          wellPanel(
            #since we are using renderUI, get rid of conditionalPanels and tabsetPanel and tabPanel in mainPanel as they are not needed
            #Inputs for Variable Contribution Tab ; b/c it's a combined app need to iterate inputID's for PCA section
            selectizeInput('rows2', 'Individual Row IDs to Render', choices = seq_len(nrow(Data())), multiple = TRUE),
            numericInput(inputId = "num11", label= "Num Top Contributive Observations to Component Variance to graph", value = 10, min = 1,                    width = 400),
            numericInput(inputId = "num12", label = "Component for Individual Contributions Plot", value = 1, min = 1, width=400)
          ) #closes wellPanel
        ) #closes absolutePanel  
      ), #closes sidebarPanel
      mainPanel(
        verbatimTextOutput("info3"), br(), 
        DT::dataTableOutput('tbl2'),
        verbatimTextOutput("info2"), br(), 
        plotOutput("plot.score", width="100%", height = "700px", brush = "plot_brush2"), DT::dataTableOutput('x2'),br(),
        plotlyOutput("plot.ind", width = "100%", height = "400px"),br()
      ) #closes mainPanel
    ) #closes tagList
  }) #closes pcaMain renderUI
  
  
  ##########################################################################################
  #                  Server side logic for PCA and related plots                           #
  ##########################################################################################  
  
  
  df.v <- reactive({
    
    PCA.df <- Data.num()
    
    #Create PCA constructs using FactoMineR methods
    R1<-PCA(PCA.df,ncp=10,graph=FALSE)
    
    cor.pca <- dimdesc(R1, axes = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
    
    if ((input$num9 == 3)){ #& (input$num10 == 4)){
      PC1.cor <- data.frame(cor.pca$Dim.3)
      PC1.cor <- data.frame(PC1.cor,.names=row.names(PC1.cor)) # create column with row names
      names(PC1.cor) <- c("Component_Dim1_Correlation", "PC3 p-values", ".names")
      PC1.cor$.names <- as.character(PC1.cor$.names)
      PC1.cor$.names <- gsub("[[:punct:]]", "", PC1.cor$.names) #remove punctuation from variable names
      #lapply(PC1.cor,class) #check class of row names
      #PC1.cor <- na.omit(PC1.cor)
      
      PC2.cor <- data.frame(cor.pca$Dim.4) # Note: you can have variables correlation with one component and not another
      PC2.cor <- data.frame(PC2.cor,.names=row.names(PC2.cor))
      names(PC2.cor) <- c("Component_Dim2_Correlation", "PC4 p-values", ".names")
      PC2.cor$.names <- as.character(PC2.cor$.names)
      PC2.cor$.names <- gsub("[[:punct:]]", "", PC2.cor$.names) 
      #lapply(PC2.cor,class)
      #PC2.cor <- na.omit(PC2.cor) KEEP COMMENTED OUT
      df <- PC1.cor %>% left_join(PC2.cor,by=".names")
      
    } else if ((input$num9 == 2)){ #& (input$num10 == 3)){
      PC1.cor <- data.frame(cor.pca$Dim.2)
      PC1.cor <- data.frame(PC1.cor,.names=row.names(PC1.cor)) # create column with row names
      names(PC1.cor) <- c("Component_Dim1_Correlation", "PC2 p-values", ".names")
      PC1.cor$.names <- as.character(PC1.cor$.names)
      PC1.cor$.names <- gsub("[[:punct:]]", "", PC1.cor$.names) #remove punctuation from variable names
      #lapply(PC1.cor,class) #check class of row names
      #PC1.cor <- na.omit(PC1.cor)
      
      PC2.cor <- data.frame(cor.pca$Dim.3) # Note: you can have variables correlation with one component and not another
      PC2.cor <- data.frame(PC2.cor,.names=row.names(PC2.cor))
      names(PC2.cor) <- c("Component_Dim2_Correlation", "PC3 p-values", ".names")
      PC2.cor$.names <- as.character(PC2.cor$.names)
      PC2.cor$.names <- gsub("[[:punct:]]", "", PC2.cor$.names) 
      #lapply(PC2.cor,class)
      #PC2.cor <- na.omit(PC2.cor) KEEP COMMENTED OUT
      df <- PC1.cor %>% left_join(PC2.cor,by=".names")
      
    } else if ((input$num9 == 5)){ #& (input$num10 == 6)){
      PC1.cor <- data.frame(cor.pca$Dim.5)
      PC1.cor <- data.frame(PC1.cor,.names=row.names(PC1.cor)) # create column with row names
      names(PC1.cor) <- c("Component_Dim1_Correlation", "PC5 p-values", ".names")
      PC1.cor$.names <- as.character(PC1.cor$.names)
      PC1.cor$.names <- gsub("[[:punct:]]", "", PC1.cor$.names) #remove punctuation from variable names
      #lapply(PC1.cor,class) #check class of row names
      #PC1.cor <- na.omit(PC1.cor)
      
      PC2.cor <- data.frame(cor.pca$Dim.6) # Note: you can have variables correlation with one component and not another
      PC2.cor <- data.frame(PC2.cor,.names=row.names(PC2.cor))
      names(PC2.cor) <- c("Component_Dim2_Correlation", "PC6 p-values", ".names")
      PC2.cor$.names <- as.character(PC2.cor$.names)
      PC2.cor$.names <- gsub("[[:punct:]]", "", PC2.cor$.names) 
      #lapply(PC2.cor,class)
      #PC2.cor <- na.omit(PC2.cor) KEEP COMMENTED OUT
      df <- PC1.cor %>% left_join(PC2.cor,by=".names")
      
    } else {
      PC1.cor <- data.frame(cor.pca$Dim.1)
      PC1.cor <- data.frame(PC1.cor,.names=row.names(PC1.cor)) # create column with row names
      names(PC1.cor) <- c("Component_Dim1_Correlation", "PC1 p-values", ".names")
      PC1.cor$.names <- as.character(PC1.cor$.names)
      PC1.cor$.names <- gsub("[[:punct:]]", "", PC1.cor$.names) #remove punctuation from variable names
      #lapply(PC1.cor,class) #check class of row names
      #PC1.cor <- na.omit(PC1.cor)
      
      PC2.cor <- data.frame(cor.pca$Dim.2) # Note: you can have variables correlation with one component and not another
      PC2.cor <- data.frame(PC2.cor,.names=row.names(PC2.cor))
      names(PC2.cor) <- c("Component_Dim2_Correlation", "PC2 p-values", ".names")
      PC2.cor$.names <- as.character(PC2.cor$.names)
      PC2.cor$.names <- gsub("[[:punct:]]", "", PC2.cor$.names) 
      #lapply(PC2.cor,class)
      #PC2.cor <- na.omit(PC2.cor) KEEP COMMENTED OUT
      df <- PC1.cor %>% left_join(PC2.cor,by=".names")
    }
    df$names <- as.character(df$.names)
    df[1:input$num8,]
  })
  
  #####################################
  #Generate rbokeh variable Factor Map#
  #####################################
  
  #Create the underlying unit circle for rbokeh plot
  theta <- seq(0,2*pi,length.out = 100)
  circle <- data.frame(x = cos(theta), y = sin(theta))
  
  output$a_bokeh_plot_pca <- renderRbokeh({     
    figure(title = "rbokeh Variable Factor Map", xlab="Component Dimension 1", ylab="Component Dimension 2")%>% 
      ly_lines(circle)%>% #create unit circle
      ly_segments(-1,0,1,0,type=2)%>% #x-axis
      ly_segments(0,-1,0,1,type=2)%>% #y-axis  
      ly_segments(0,0, df.v()$Component_Dim1_Correlation, df.v()$Component_Dim2_Correlation,data= df.v(),color =names,legend=FALSE)%>%
      #adds hovering to data points and arrow heads to line vectors  
      ly_points(df.v()$Component_Dim1_Correlation, df.v()$Component_Dim2_Correlation, data= df.v(), glyph = "circle", 
                hover = c(Component_Dim1_Correlation, Component_Dim2_Correlation))%>%
      ly_text(x=df.v()$Component_Dim1_Correlation,y=df.v()$Component_Dim2_Correlation,text=df.v()$.names,data=df.v()) # labels vectors
  })
  
  
  #################################################################
  #Factoextra Variable Contribution Histogram for Dim 1 AND Dim 2 #
  #################################################################
  
  output$plot.var2 <- renderPlotly({
    
    PCA.df <- Data.num()
    
    #Create PCA constructs using FactoMineR methods
    R1<-PCA(PCA.df,ncp=10,graph=FALSE)
    
    #Use factoextra fviz_contrib to visualize individual obs contributions to component construction instead of the dimension plot     
    r1 <- fviz_contrib(R1, choice = "var", axes = input$num9, top = input$num8)
    ggplotly(r1)
    
  })
  
  output$plot.var3 <- renderPlotly({
    
    PCA.df <- Data.num()
    
    #Create PCA constructs using FactoMineR methods
    R1<-PCA(PCA.df,ncp=10,graph=FALSE)
    
    #Use factoextra fviz_contrib to visualize individual obs contributions to component construction instead of the dimension plot     
    r2 <- fviz_contrib(R1, choice = "var", axes = input$num9+1, top = input$num8)
    ggplotly(r2)
    
  })
  
  #######################################################
  #   Factoextra cos2 Histogram FOR Dim 1 AND Dim 2    ##
  #######################################################
  
  output$plot.var4 <- renderPlotly({
    
    PCA.df <- Data.num()
    
    #Create PCA constructs using FactoMineR methods
    R1<-PCA(PCA.df,ncp=10,graph=FALSE)
    
    input.num10 <- input$num9+1
    #Use factoextra fviz_contrib to visualize individual obs contributions to component construction instead of the dimension plot     
    r2 <- fviz_cos2(R1, choice = "var", axes = input$num9:input.num10, top = input$num8)
    ggplotly(r2)
    
  })
  
  ###################################################
  #            Develop Data View Plots              #
  ###################################################
  
  
  ########################
  #  Brushed Points Plot #
  ########################
  
  output$plot.score <- renderPlot({
    
    datax <- Data() #set reactive data into a regular R object to lose reactive object syntax and make sub-setting easier
    is.fact <- sapply(datax, is.factor)
    #dataxx <- datax[,-is.fact] #remove any factor variables 
    PCA.df <- sapply(datax[,-is.fact], as.numeric) #need to make sure variables are numeric
    clr <- data.frame(datax[ ,is.fact, drop=FALSE])
    
    autoplot(prcomp(PCA.df, center=TRUE, scale=TRUE), data = PCA.df, colour = colnames(clr)) + 
      ggtitle('Data Projected onto Principal Component Space')
    
  })
  
  output$info2 <- renderPrint({
    cat('Brush across the plot to render data points in the data table below dynamically')
  })
  
  #This reactive object is only created so we can reference the correct data for the brushed points plot otherwise it doesn't work
  z <- reactive({
    
    datax <- Data() #set reactive data into a regular R object to lose reactive object syntax and make sub-setting easier
    is.fact <- sapply(datax, is.factor)
    #dataxx <- datax[,-is.fact] #remove any factor variables 
    PCA.df <- sapply(datax[,-is.fact], as.numeric) #need to make sure variables are numeric
    clr <- data.frame(datax[ ,is.fact, drop=FALSE])
    
    autoplot(prcomp(PCA.df, center=TRUE, scale=TRUE), data = PCA.df, colour = colnames(clr)) + 
      ggtitle('Data Projected onto Principal Component Space')
    
  })
  
  ############################################################# 
  #creates javascript data table for brushed points rendering##
  #############################################################
  
  output$x2 = DT::renderDataTable(brushedPoints(z()$data, input$plot_brush2, allRows = FALSE), server = FALSE, options = list(
    scrollX=TRUE,
    scrollCollapse=TRUE))
  
  ################################################
  #Factoextra Individual Contribution Histogram ##
  ################################################
  
  output$plot.ind <- renderPlotly({
    
    PCA.df <- Data.num()
    
    #Create PCA constructs using FactoMineR methods
    R1<-PCA(PCA.df,ncp=10,graph=FALSE)
    
    #Use factoextra fviz_contrib to visualize individual obs contributions to component construction instead of the dimension plot     
    r3 <- fviz_contrib(R1, choice = "ind", axes = input$num12, top = input$num11)
    ggplotly(r3)
  })
  
  ##################################################################################################################
  #Insert javascript data table for viewing individual observations on demand; naming conventions are iterated + 1 #
  ##################################################################################################################
  
  output$info3 <- renderPrint({
    cat('The below table renders individual observations by row ID on demand; Use to inspect raw data')
  })
  
  output$tbl2 = DT::renderDataTable({
    DT::datatable(Data()[c(input$rows2),], colnames = c('ID' = 1), options = list(
      scrollX=TRUE,
      scrollCollapse=TRUE)
    )
  })
  
  output$foo2 = DT::renderDataTable(
    Data(), server = FALSE, selection = list(target = 'row')
  )
  
  proxy2 = dataTableProxy('foo2')
  
  observeEvent(input$select1, {
    selectRows(proxy2, as.numeric(input$rows2))
  })
  
  #########################################
  # Develop Scree Plot for Scree Plot tab #
  #########################################
  
  #Use cross-validation to estimate number of components in PCA
  
  output$textNCP <- renderText({ 
    
    PCA.df <- Data.num()
    
    ncp <- estim_ncp(PCA.df,scale=TRUE)
    paste("Cross-validation estimate of number of principal components in PCA:"," ", ncp$ncp)
  })
  
  
  output$scree_bokeh_plot <- renderRbokeh({
    
    PCA.df <- Data.num()
    
    #Create PCA constructs using FactoMineR methods
    R1<-PCA(PCA.df,ncp=10,graph=FALSE)
    
    var.exp <- data.frame(R1$eig)
    
    p <- figure(title="Interactive Scree Plot", toolbar_location="above") %>%
      ly_points(var.exp$percentage.of.variance, data = var.exp,hover = c(percentage.of.variance, cumulative.percentage.of.variance)) %>%
      y_axis(label="% Variance Explained", number_formatter = "basic") %>%
      x_axis(label = "Principal Component")
  })
  
  
  
  ################################################################################################################################################# ##################################################         BEGIN MC ANALYSIS                 #################################################### 
  #################################################################################################################################################
  #################################################################################################################################################  
  
  ##############################################################################################
  # Generate entire Multiple Correspondence Analysis UI dynamically, server-side using renderUI#
  ##############################################################################################
  
  #############################################################
  #####     ui for Studying Variables & Categories page    ####
  #############################################################
  
  output$ui.mcaVAR = renderUI({ 
    tagList(
      sidebarPanel(
        tags$head(
          tags$style("body {background-color: #EFFBFB; }"),
          tags$style(type='text/css', "#title1 { height: 25px; }")
        ),
        absolutePanel(
          draggable = TRUE,
          wellPanel( #since we are using renderUI, get rid of conditionalPanels and tabsetPanel and tabPanel in mainPanel as they are not needed
            #Inputs for Studying Variable Categories page; all input ID's are iterated or follow a convention that makes sense for that page
            numericInput('cat', 'Number of Categories to project by Contribution ', value = 10, min = 1, width = 400), 
            numericInput(inputId = "MCA2", label = "Set Dimension 1 Principal Component", value = 1, min = 1, width=400)
            #numericInput(inputId = "MCA1", label = "Number of Categories to plot (for cos2 & contrib)", value = 10, min = 2, width=400)
          ) #closes wellPanel
        )
      ), #closes sidebarPanel
      mainPanel(
        #verbatimTextOutput("summary"),
        br(),
        plotlyOutput("plot.var.mca.CAT", width = "100%", height = "400px"),
        br(),
        plotlyOutput("plot.var.mca.CONTRIB", width = "100%", height = "400px"),
        #br(),
        #plotlyOutput("plot.var.mca.COS2", width = "100%", height = "400px"),
        br(),
        plotlyOutput("plot.varMCA", width = "100%", height = "400px")
      ) #closes mainPanel
    ) #closes tagList
  }
  ) #closes mcaVAR renderUI
  
  output$ui.mcaIND = renderUI({ 
    tagList(
      sidebarPanel(
        tags$head(
          tags$style("body {background-color: #EFFBFB; }"),
          tags$style(type='text/css', "#title1 { height: 25px; }")
        ),
        absolutePanel(
          draggable = TRUE,
          wellPanel( #since we are using renderUI, get rid of conditionalPanels and tabsetPanel and tabPanel in mainPanel as they are not needed
            #Inputs for Studying Variable Categories page; all input ID's are iterated or follow a convention that makes sense for that page
            textAreaInput("clr", "Color Observations by <Enter Variable>" , value = Data.fact()[,1], width = 300, height = NULL, cols = NULL, rows           =  NULL,placeholder = NULL, resize = NULL),
            numericInput(inputId = "MCA1", label = "Set Dimension 1 Principal Component", value = 1, min = 1, width=400),
            numericInput(inputId = "ind", label = "Number of Individuals to project by Contribution ", value = as.integer(nrow(Data.fact())),                width=400)
          ) #closes wellPanel
        )
      ), #closes sidebarPanel
      mainPanel(
        #verbatimTextOutput("summary2"),
        br(),
        plotlyOutput("plot.mca.IND", width = "100%", height = "400px"),
        br(),
        plotlyOutput("plot.mca.ind.CONTRIB", width = "100%", height = "400px")
      ) #closes mainPanel
    ) #closes tagList
  }
  ) #closes mcaIND renderUI
  
  ######################################
  #This entire rendered UI below is only necessary when we are using factoextra to create the simultaneous rep plot; when we use ggplot2 directly, we 
  #can just render that directly in the tabPanel in ui.R
  ######################################
  
  #output$ui.mcaSIM = renderUI({ 
  #  tagList(
  #    sidebarPanel(
  #      tags$head(
  #        tags$style("body {background-color: #EFFBFB; }"),
  #        tags$style(type='text/css', "#title1 { height: 25px; }")
  #      ),
  #absolutePanel(
  #  draggable = TRUE,
  #  wellPanel( #since we are using renderUI, get rid of conditionalPanels and tabsetPanel and tabPanel in mainPanel as they are not needed
  #Inputs for Studying Variable Categories page; all input ID's are iterated or follow a convention that makes sense for that page
  #numericInput(inputId = "MCA3", label = "Set Dimension 1 Principal Component", value = 1, min = 1, width=400),
  #numericInput(inputId = "ind2", label = "Number of Individuals to project by Contribution ", value = as.integer(nrow(Data.fact())), 
  #width=400),
  #numericInput('catsim', 'Number of Categories to project by Contribution', value = 10, min = 1, width = 400),
  #selectizeInput('rows3', 'Individual Row IDs to Render', choices = seq_len(nrow(Data.fact())), multiple = TRUE)
  #  ) #closes wellPanel
  #)
  #), #closes sidebarPanel
  #mainPanel(
  #verbatimTextOutput("summary3"),
  #br(),
  #plotOutput("plot.mca.SIM", width = "100%", height = "800px")#,br(),
  #DT::dataTableOutput('tbl3')
  #verbatimTextOutput("info3")
  #) #closes mainPanel
  #) #closes tagList
  #}
  #) #closes mcaIND renderUI
  
  
  ##########################################################################################
  #                  Server side logic for MCA and related plots                           #
  ##########################################################################################  
  
  #eigenvalue summary + scree plot rendered directly on tabPanel in ui.R
  output$eigMCA <- renderPrint({ 
    
    MCA.df <- Data.fact()
    
    #Create MCA constructs using FactoMineR methods
    R1 <- MCA(MCA.df, ncp=10, graph=FALSE)
    
    get_eig(R1)
    
  })
  
  output$plot.eig.MCA <- renderPlotly({
    
    MCA.df <- Data.fact()
    
    #Create MCA constructs using FactoMineR methods
    R1 <- MCA(MCA.df, ncp=10, graph=FALSE)
    
    r1<- fviz_eig(R1, linecolor = "#FC4E07",barcolor = "#2E9FDF", barfill = "#2E9FDF")
    ggplotly(r1)
    
  })
  
  ##########################################################################################
  #         Plots and Output for Studying Variables and Categories                          #
  ##########################################################################################  
  
  #Create a summary of variables so you know which categories map to which variables
  # create a separate tabPanel page with this info for reference instead of putting on each individual analysis page mainPanel
  output$summary <- renderPrint({
    dataset <- Data.fact()
    summary(dataset)
  })
  
  #output$summary2 <- renderPrint({
  #  dataset <- Data.fact()
  #  summary(dataset)
  #})
  
  #output$summary3 <- renderPrint({
  #  dataset <- Data.fact()
  #  summary(dataset)
  #})
  
  
  ###############################################################
  #Create variables correlation ratio plot (cloud of variables)##
  ###############################################################
  
  output$plot.varMCA <- renderPlotly({
    
    MCA.df <- Data.fact()
    
    #Create MCA constructs using FactoMineR methods
    R1 <- MCA(MCA.df, ncp=10, graph=FALSE)
    
    axis1 <- input$MCA2
    input.MCaxis <- input$MCA2 + 1
    
    #Use factoextra fviz_contrib to visualize individual obs contributions to component construction instead of the dimension plot     
    r1 <- fviz_mca_var(R1, choice = "mca.cor", axes = c(input$MCA2:input.MCaxis))
    ggplotly(r1)
    
  })
  
  #####################################################################
  #Create variables categories cloud to project onto component space##
  #####################################################################
  
  output$plot.var.mca.CAT <- renderPlotly({
    
    MCA.df <- Data.fact()
    
    #Create MCA constructs using FactoMineR methods
    R1 <- MCA(MCA.df, ncp=10, graph=FALSE)
    
    axis1 <- input$MCA2
    input.MCaxis <- input$MCA2 + 1
    
    #Use factoextra fviz_contrib to visualize individual obs contributions to component construction instead of the dimension plot     
    r1 <- fviz_mca_var(R1, choice = "var.cat", axes = c(input$MCA2:input.MCaxis), select.var = list(contrib=input$cat))
    ggplotly(r1)
    
  })
  
  
  ##########################################################################
  #   Factoextra cos2 Histogram FOR Dim 1 AND Dim 2 variable categories   ##  #removed 1/4/2018
  ##########################################################################
  
  #Not as relevant in MCA, removing to declutter the mainPanel 12/1/18
  
  #output$plot.var.mca.COS2 <- renderPlotly({
  
  #MCA.df <- Data.fact()
  
  #Create MCA constructs using FactoMineR methods
  #R1<-MCA(MCA.df,ncp=10,graph=FALSE)
  
  #input.MCaxis <- input$MCA2 + 1
  
  #Use factoextra fviz_contrib to visualize individual obs contributions to component construction instead of the dimension plot     
  #r2 <- fviz_cos2(R1, choice = "var", axes = c(input$MCA2:input.MCaxis), top = input$cat)
  #ggplotly(r2)
  
  #})
  
  
  ##################################################################################
  #   Factoextra contribution Histogram FOR Dim 1 and Dim 2 variable categories   ##
  ##################################################################################
  
  output$plot.var.mca.CONTRIB <- renderPlotly({
    
    MCA.df <- Data.fact()
    
    #Create MCA constructs using FactoMineR methods
    R1<-MCA(MCA.df,ncp=10,graph=FALSE)
    
    #Use factoextra fviz_contrib to visualize individual var contributions to component construction instead of the dimension plot     
    r1 <- fviz_contrib(R1, choice = "var", axes = c(input$MCA2), top = input$cat)
    ggplotly(r1)
    
  })
  
  ###############################################################
  #         Plots and Output for Studying Individuals           #
  ############################################################### 
  
  output$plot.mca.IND <- renderPlotly({
    
    MCA.df <- Data.fact()
    
    #Create MCA constructs using FactoMineR methods
    R1 <- MCA(MCA.df, ncp=10, graph=FALSE)
    
    axis1 <- input$MCA1
    input.MCaxis <- input$MCA1 + 1
    
    #Use factoextra fviz_contrib to visualize individual obs contributions to component construction instead of the dimension plot     
    r1 <- fviz_mca_ind(R1, axes = c(input$MCA1:input.MCaxis), habillage = input$clr, select.ind = list(contrib=input$ind))
    ggplotly(r1)
    
  })
  
  output$plot.mca.ind.CONTRIB <- renderPlotly({
    
    MCA.df <- Data.fact()
    
    #Create MCA constructs using FactoMineR methods
    R1 <- MCA(MCA.df, ncp=10, graph=FALSE)
    
    axis1 <- input$MCA1
    input.MCaxis <- input$MCA1 + 1
    
    #Use factoextra fviz_contrib to visualize individual obs contributions to component construction instead of the dimension plot     
    r1 <- fviz_contrib(R1, choice="ind", axes=input$MCA1:input.MCaxis, top=input$ind, linecolor = "#FC4E07",barcolor = "#2E9FDF", barfill ="#2E9FDF") 
    ggplotly(r1)
    
  })
  
  
  ###############################################################
  #           Plot for Simultaneous Representation              #
  ############################################################### 
  
  output$plot.mca.SIM <- renderPlot({
    
    MCA.df <- Data.fact()
    
    #Create MCA constructs using FactoMineR methods
    R1 <- MCA(MCA.df, ncp=10, graph=FALSE)
    
    #axis1 <- input$MCA3
    #input.MCaxis <- input$MCA3 + 1
    
    #Use factoextra fviz_contrib to visualize individual obs contributions to component construction instead of the dimension plot     
    #r1 <- fviz_mca_biplot(R1, choice = "var.cat", geom = c("point", "text"), axes=input$MCA3:input.MCaxis, 
    #select.ind = list(contrib = input$ind2), select.var= list(contrib = input$catsim), repel = FALSE)
    
    #r1
    #ggplotly(r1)
    
    #######################################################################################################
    #create a better simultaneous representation plot using ggplot2 directly, instead of using factoextra##
    #######################################################################################################
    
    # number of categories per variable
    cats = apply(MCA.df, 2, function(x) nlevels(as.factor(x)))
    #cats
    
    # data frames for ggplot
    mca1_vars_df = data.frame(R1$var$coord, Variable = rep(names(cats), 
                                                           cats))
    mca1_obs_df = data.frame(R1$ind$coord)
    
    # plot of variable categories
    #ggplot(data = mca1_vars_df, aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df))) + 
    #geom_hline(yintercept = 0, colour = "gray70") + geom_vline(xintercept = 0, colour = "gray70") + 
    #geom_text_repel(aes(colour = Variable)) + ggtitle("MCA plot of variables")
    
    # MCA simulteanous representation plot of observations and categories
    ggplot(data = mca1_obs_df, aes(x = Dim.1, y = Dim.2)) + 
      geom_hline(yintercept = 0, colour = "gray70") + 
      geom_vline(xintercept = 0, colour = "gray70") + 
      geom_point(colour = "gray50", alpha = 0.7) + 
      geom_density2d(colour = "gray80") + 
      geom_text_repel(data = mca1_vars_df, aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df), colour = Variable)) + 
      #geom_text(data = mca1_obs_df, aes(x = Dim.1, y = Dim.2, label = rownames(mca1_obs_df))) +  
      ggtitle("MCA Simultaneous Representation Plot")       + scale_colour_discrete(name = "Variable")
    
  })
  
  
  #############################################################################################
  #creates javascript data table for when we have factoextra simultaenous representation plot
  #allows individual observations in the underlying data to be returned as an interactive table
  #############################################################################################
  
  #output$tbl3 = DT::renderDataTable({
  #  DT::datatable(Data2()[c(input$rows3),], colnames = c('ID' = 1), options = list(
  #    scrollX=TRUE,
  #   scrollCollapse=TRUE)
  #  )
  #})
  
  #output$foo3 = DT::renderDataTable(
  #  Data2, server = FALSE, selection = list(target = 'row')
  #)
  
  #proxy3 = dataTableProxy('foo3')
  
  #observeEvent(input$select1, {
  #  selectRows(proxy3, as.numeric(input$rows3))
  #})
  
  
}) #end server function