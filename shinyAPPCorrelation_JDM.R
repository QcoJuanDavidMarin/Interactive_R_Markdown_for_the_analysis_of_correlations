library(shiny)
library(shinydashboard)
library(tibble)
library(tidyr)
library(dplyr)
library(tidyselect)
library(DT)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(psych)
library(rstatix)
library(plotly)
library(ggplot2)
library(ggpubr)
library(ggstatsplot)
library(patchwork) # Divide graph
library(fpc)
library(cluster)


ui <- dashboardPage(skin = 'red',
  dashboardHeader(title = 'Correlation Analysis in Physico-Chemical Experiments', titleWidth = 650),
  
  dashboardSidebar(h6('By QCO_DS Juan David Marin'),
                   img(src ='jdm_logo_2.png', height = 100, width = 230),
                   ##SubMenus 
                   sidebarMenu(
                     menuItem(text = 'Load Data', tabName = 'LoadData', icon = icon('file-csv', class = 'fa-flip')),
                     menuItem(text = 'Testing', tabName = 'test', icon = icon("arrow-up-right-dots"),
                              menuSubItem(text = 'basic stats', tabName = 'stats'),
                              menuSubItem(text = 'correlation', tabName = 'corr')),
                     menuItem(text = 'PCA and Clus Analysis',tabName = 'pcaclus',icon = icon('shapes'),
                              menuSubItem(text = 'PCA CLUSTER', tabName = 'acp'),
                              menuSubItem(text = 'PCA Analysis', tabName = 'pcaanalysis'),
                              menuSubItem(text = 'Cluster Analysis', tabName = 'clusana'),
                              sliderInput(inputId = 'nclus',label = 'Num. Clus', min = 1,
                                          max = 12,value = 3,step = 1)),
                     menuItem(text = 'predictions', tabName = 'lm', icon = icon('square-up-right'),
                              menuSubItem(text = 'lineal Models', tabName = 'lm'),
                              menuSubItem(text = 'Anova 2ways', tabName = 'a2w'),
                              numericInput(inputId = 'var_A5', label = 'var_A5', min = 0,step = 0.1, value = 0),
                              numericInput(inputId = 'var_B5', label = 'var_B5', min = 0,step = 0.1, value = 0),
                              menuSubItem(text = 'Prediction', tabName = 'pred'))
                     
                   )
                   ),
  
  dashboardBody(

    tabItems(tabItem(tabName = 'LoadData', box(radioButtons(inputId = 'headers', label = 'Header',
                                                            choices = c('Columns have headers'='Yes',
                                                                        'Columns do not have headers'='No'),
                                                            selected = 'Yes'), status = 'danger', width = 2, height = 130), #primary, success, info, warning, danger
                     
                                            box(radioButtons(inputId = 'sepa', label = 'Separator',
                                                             choices = c('Coma' = ',',
                                                                       'Semicolon' = ';',
                                                                       'Tab' = '\t'),
                                                             selected = ';'),status = 'danger', width = 2, height = 130),
                     
                                            box(radioButtons(inputId = 'quot','Quote',
                                                             choices = c(None = '',
                                                                         'Double Quote'='"',
                                                                         'Single Quote'="'"),
                                                             selected = '"'),status = 'danger', width = 2, height = 130),
                     
                                            box(radioButtons(inputId = 'decimal',label =  'Decimal',
                                                             choices = c('Comma'=',',
                                                                         'Point'= '.'),
                                                             selected = '.'),status = 'danger', width = 2, height = 130),
                     
                                           box(radioButtons(inputId = 'strasfac',label =  'stringsAsFactors',
                                                            choices = c('True'='Yes',
                                                                        'False'= 'No'),
                                                            selected = 'Yes'),status = 'danger', width = 2, height = 130),
                                            
                                            box(fileInput(inputId = 'file1', label = 'Select CSV File',
                                                          accept = c('text/csv',
                                                                     'text/comma-separated-values',
                                                                     'text/tab-separated-values',
                                                                     'text/plain',
                                                                     '.csv', 
                                                                     '.tsv')), background = 'black'),
                     
  
                     
                                     DT::dataTableOutput('dataset'),

                                     uiOutput(outputId = "selectColumn"),
                                     actionButton(inputId = "Remove", label = "Remove")),# End file tabitem LoadData
             
          ### sheet STATISTICS 
          
          tabItem(tabName = 'stats',
                  box(uiOutput(outputId = 'variables'), width = 6),
                  box(uiOutput(outputId = 'select_sup_var1'), width = 6),
                  box(plotOutput(outputId = 'StatsBoxPlot'), collapsible = T, title = 'Box Plot',
                      solidHeader = T,status = 'info', width = 12),
                  checkboxInput(inputId = 'wrap', label = 'Facet Wrap'),
                  box(DT::dataTableOutput(outputId = 'average'),collapsible = T,title = 'Summary',
                      solidHeader = T,status = 'info',collapsed = T),
                  box(DT::dataTableOutput(outputId = 'outliers'), collapsible = T, title = 'Outliers',
                      solidHeader = T,status = 'info', collapsed = T)
                  ),
          
          tabItem(tabName = 'corr',
                  box(plotOutput('correlation', height = 405),title = "Correlation Matrix", status = "warning", solidHeader = T,
                      collapsible = T),
                  box(DT::dataTableOutput(outputId = 'KMO'), title = 'Kaiser-Meyer-Olkin',status = "warning", solidHeader = T,
                      collapsible = T),
                  box(DT::dataTableOutput(outputId = 'bartlett'),title = "Bartlett's test", status = "warning", solidHeader = T,
                      collapsible = T),
                  box(tableOutput(outputId = 'det'), title = 'DET',status = "warning", solidHeader = T,
                      collapsible = T)
                  ),

             ## sheet ACP
          tabItem(tabName = 'acp',
                  box(uiOutput(outputId = 'contriVar'),width = 3),
                  box(uiOutput(outputId = 'contriInd'),width = 3),
                  box(uiOutput(outputId = 'select_sup_var'), width = 3),
                  checkboxInput(inputId = 'ellipse', label = 'show ellipse', value = F),
                  plotOutput(outputId = 'principal_ca')
                  ),
              
                ## Sheet pcaanalysis
            tabItem(tabName = 'pcaanalysis',
                    box(uiOutput(outputId = 'summarypca'),width = 2),
                    box(verbatimTextOutput(outputId = 'summary_pca'),title = 'Summary PCA', solidHeader = T,
                        collapsible = T, width = 10, status = "success",collapsed = T),
                    box(plotOutput(outputId = 'qualitiescontrib'), width = 12, title = 'Cualidades y contribución de la representación de las variables',
                        solidHeader = T, collapsible = T,status = "success", collapsed = T),
                    box(verbatimTextOutput(outputId = 'LBVCV'),title = 'Link between the variable and the continuous variables (R-square)',
                        solidHeader = T,status = "success", collapsible = T, collapsed = T),
                    box(plotOutput(outputId = 'qualiticontribindi'), width = 12, title = 'Quality and contribution of individual representation',
                        solidHeader = T, collapsible = T,status = "success", collapsed = T)
                    ),
          
              ## sheet clusana
          tabItem(tabName = 'clusana',
                  box(uiOutput(outputId = 'categoricalclus'), width = 6, height = 160, 
                      collapsible = T, solidHeader = T),
                  box(tableOutput(outputId = 'valkmeans'), collapsible = T, width = 6),
                  box(plotlyOutput(outputId = 'kmean_grup'),title = 'Principal variables by categorical variable',
                      solidHeader = T, collapsible = T, status = "success",width = 6),
                  box(plotOutput(outputId = 'partitionkmean'),collapsible = T, width = 6)
                  ),
              ## sheet lineal models
          tabItem(tabName = 'lm',
                  box(uiOutput(outputId = 'numvar1lm'),title = 'Select a X numeric variables',
                      solidHeader = T, collapsible = T,status = "success", width = 5),
                  box(uiOutput(outputId = 'numvar2lm'),title = 'Select a Y numeric variables',
                      solidHeader = T, collapsible = T,status = "success", width = 5),
                  box(checkboxInput(inputId = 'tf',label = 'T/F', value = F),title = 'Ellipse',
                      solidHeader = T, collapsible = T,status = "success", width = 2),
                  box(plotOutput(outputId = 'lmplots'), title = 'Lineal models plot',
                          solidHeader = T, collapsible = T,status = "success", width = 12)
                  ),
          ## sheet anova 2 ways
          tabItem(tabName = 'a2w',
                  box(uiOutput(outputId = 'a2wA'), title = 'Select a categorical variable 1',
                      solidHeader = T, collapsible = T,status = "warning", width = 4),
                  box(uiOutput(outputId = 'a2wB'),  title = 'Select a categorical variable 2',
                      solidHeader = T, collapsible = T,status = "warning", width = 4),
                  box(uiOutput(outputId = 'a2wC'),  title = 'Select a numerical variable',
                      solidHeader = T, collapsible = T,status = "warning", width = 4),
                  box(plotOutput(outputId = 'anovaplot',width = "100%",height = "500px"), title = 'Anova 2 Ways plot',
                      solidHeader = T, collapsible = T, status = 'warning', width = 12)
                  ),
          ## sheet pred
          tabItem(tabName = 'pred',
                  box(selectInput(inputId = 'label',label = 'Labels', multiple = T,
                                  choices = c("ind", "ind.sup", "quali", "var", "quanti.sup"), 
                                  selected = c("ind", "ind.sup", "quali", "var", "quanti.sup")),
                      solidHeader = T, collapsible = T,status = "warning", width = 6),
                  box(selectInput(inputId = 'invisible',label = 'Invisible', multiple = T,
                                  choices = c("ind", "ind.sup", "quali", "var", "quanti.sup"), selected = "ind"),
                      solidHeader = T, collapsible = T,status = "warning", width = 6),
                  box(tableOutput(outputId = 'kmeans'), collapsible = T, width = 12),
                  box(plotOutput(outputId = 'plotpred'), title = 'Predictions',
                      solidHeader = T, collapsible = T,status = "warning", width = 6),
                  box(plotlyOutput(outputId = 'plotpredrad'), title = 'Profile Radar',
                      solidHeader = T, collapsible = T,status = "warning", width =6 ),
                  box(DT::dataTableOutput(outputId = 'tableclus'), title = 'Data Table',
                      collapsible = T, status = 'warning', width = 12)
                  )
  )
    ) 
    
  )


#######################################__________________________######################################
server <- function(input, output, session){
  
  ####________PAG LOAD DATA_______####________PAG LOAD DATA_______####________PAG LOAD DATA_______####________PAG LOAD DATA_______
  
  df <- reactiveValues(data = NULL)
  removecolumn <- function(df, nameofthecolumn){dplyr::select(df, -(all_of(nameofthecolumn)))}
  
  ###DataFrame
  inputData <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    
    phys_chem <- read.csv(inFile$datapath, sep = input$sepa, header = (input$headers =='Yes'),
                          dec = input$decimal, quote = input$quote, stringsAsFactors = (input$strasfac =='Yes'))
    
    phys_chem <- column_to_rownames(phys_chem, var = colnames(phys_chem[1]))
    df$phys_chem <- phys_chem
    return(phys_chem)
    
  
  })
  
  # update column names
  output$selectColumn <- renderUI({
    shiny::req(inputData())
  
    selectInput(inputId = 'selectColumn',
                label = "Remove selected sample(s)",
                multiple = T, choices = names(inputData()))
  })
  
  observeEvent(input$Remove,{
    df$phys_chem <- removecolumn(df$phys_chem, input$selectColumn)
  })
  
  ###Show DF

  output$dataset <- DT::renderDataTable({
    DT::datatable(df$phys_chem,
                  options = list(scrollY=400,
                                 scrollX=300,
                                 scroller = TRUE,
                                 pageLength = 10))

  })
  
  
  ## Selectect vars
  output$variables <- renderUI({
    shiny::req(inputData())
    phys_chem <- df$phys_chem
    phys_chem1 <- phys_chem %>% 
      select_if(is.numeric)
    selectInput(inputId = 'variables',
                label = 'Select Variables',
                multiple = T, choices = names(inputData()[names(phys_chem1)]), selected =  names(inputData()[names(phys_chem1)[1]]))
    
  })
  

  # Select suplemntary vars FOR SUMMARY PCA
  output$summarypca <- renderUI({
      shiny::req(inputData())
      phys_chem <- df$phys_chem
      phys_chem1 <- phys_chem %>% 
        select_if(is.factor)
      selectInput(inputId = 'summarypca',
                  label = 'Categorical Var.',
                  multiple = T, choices = names(inputData()[names(phys_chem1)]), selected =  names(inputData()[names(phys_chem1)[1:length(phys_chem1)]]))
  })
  
  # Select suplemntary vars
  output$rownames <- renderUI({
    shiny::req(inputData())
    phys_chem <- df$phys_chem
    phys_chem1 <- phys_chem %>% 
      select_if(is.factor)
    
    selectInput(inputId = 'rownames',
                label = 'Samples',
                multiple = T, choices = rownames(inputData()), selected =  rownames(inputData())[1])
  })
  
  # Select suplemntary vars
  output$select_sup_var1 <- renderUI({
    shiny::req(inputData())
    phys_chem <- df$phys_chem
    
    phys_chem1 <- phys_chem %>% 
      select_if(is.factor)
    selectInput(inputId = 'select_sup_var1',
                label = 'Select Cat. Var',
                multiple = F, choices = names(inputData()[names(phys_chem1)]), selected =  names(inputData()[names(phys_chem1)[1]]))
  })

  # Select suplemntary vars para el biplot con KMEANS
  output$select_sup_var <- renderUI({
    shiny::req(inputData())
    phys_chem <- df$phys_chem
    
    
    phys_chem_std <- phys_chem %>%
      dplyr::select_if(is.numeric) %>%
      scale() %>% 
      as.data.frame()
    set.seed(123)
    clus_kmeans <- kmeans(phys_chem_std, centers = input$nclus)
    phys_chem_clus <- phys_chem %>% 
      cbind(clus = as.factor(clus_kmeans$cluster)) %>% 
      dplyr::select_if(is.factor)
    
    
    selectInput(inputId = 'select_sup_var',
                label = 'Select Sup. Var biplot',
                multiple = F, choices = names(phys_chem_clus), selected = 'clus')
  })
    
  # Select suplemntary vars CLUSTERS
  output$categoricalclus <- renderUI({
    shiny::req(inputData())
    phys_chem <- df$phys_chem
    phys_chem_std <- phys_chem %>%
      select_if(is.numeric) %>%
      scale() %>% 
      as.data.frame()
    set.seed(123)
    clus_kmeans <- kmeans(phys_chem_std, centers = input$nclus)
    phys_chem_clus <- phys_chem %>% 
      cbind(clus = as.factor(clus_kmeans$cluster)) %>% 
      dplyr::select_if(is.factor) 

    selectInput(inputId = 'categoricalclus',
                label = 'Categorical Var for Clus.',
                multiple = F, choices = names(phys_chem_clus), selected = 'clus')
  })

  # X vars
  output$numvar1lm <- renderUI({
    shiny::req(inputData())
    phys_chem <- df$phys_chem 
    varsX <- phys_chem %>% 
      dplyr::select_if(is.numeric) %>% 
      as.data.frame()
  selectInput(inputId = 'numvar1lm',label = 'Select a X var', multiple = F, choices = names(varsX), selected = names(varsX)[1])
  })
  # Y vars
  output$numvar2lm <- renderUI({
    shiny::req(inputData())
    phys_chem <- df$phys_chem
    varsY <- phys_chem %>% 
      dplyr::select_if(is.numeric) %>% 
      as.data.frame()
  selectInput(inputId = 'numvar2lm',label = 'Select a Y var', multiple = F, choices = names(varsY), selected = names(varsY)[2])
  })
  
  output$a2wA <- renderUI({
    shiny::req(inputData())
    phys_chem <- df$phys_chem
    phys_chem_std <- phys_chem %>%
      select_if(is.numeric) %>%
      scale() %>% 
      as.data.frame()
    set.seed(123)
    clus_kmeans <- kmeans(phys_chem_std, centers = input$nclus)
    phys_chem_clus <- phys_chem %>% 
      cbind(clus = as.factor(clus_kmeans$cluster)) %>% 
      dplyr::select_if(is.factor) 
    selectInput(inputId = 'a2wA', label = 'Select Cat. Var 1', multiple = F, choices = names(phys_chem_clus), selected = names(phys_chem_clus)[3])
    })
  output$a2wB <- renderUI({
    shiny::req(inputData())
    phys_chem <- df$phys_chem
    phys_chem_std <- phys_chem %>%
      select_if(is.numeric) %>%
      scale() %>% 
      as.data.frame()
    set.seed(123)
    clus_kmeans <- kmeans(phys_chem_std, centers = input$nclus)
    phys_chem_clus <- phys_chem %>% 
      cbind(clus = as.factor(clus_kmeans$cluster)) %>% 
      dplyr::select_if(is.factor) 
    selectInput(inputId = 'a2wB', label = 'Select Cat. group Var 2', multiple = F, choices = names(phys_chem_clus), selected = names(phys_chem_clus)[1])
  })
  output$a2wC <- renderUI({
    shiny::req(inputData())
    phys_chem <- df$phys_chem
    phys_chem_num <- phys_chem %>%
      dplyr::select_if(is.numeric) 
    selectInput(inputId = 'a2wC', label = 'Select Num. Var', multiple = F, choices = names(phys_chem_num), selected = names(phys_chem_num)[1])
  })
  

  ####________STATISTICS______####________STATISTICS______####________STATISTICS______####________STATISTICS______

    ## Basics statics
  output$average <- DT::renderDataTable({
    phys_chem1 <- df$phys_chem
    phys_chem1<-phys_chem1 %>% 
      select_if(is.numeric) 
    
    phys_chem1 <- phys_chem1 %>% 
      gather(key = 'variables', value = 'values', 1:length(phys_chem1)) %>% 
      group_by(variables) %>% 
      get_summary_stats(values, type = 'mean_sd') %>% 
      select(-variable) %>% 
      DT::datatable()

  })
  ## Outliers
  output$outliers <- DT::renderDataTable({
    phys_chem1 <- df$phys_chem
    phys_chem1<-phys_chem1 %>% 
      select_if(is.numeric)
    
    phys_chem1 <- phys_chem1 %>% 
      gather(key = 'variables', value = 'values', 1:length(phys_chem1)) %>% 
      group_by(variables) %>% 
      identify_outliers(values) %>% 
      DT::datatable()
  })
  
  # Boxplot
  output$StatsBoxPlot<- renderPlot({
    phys_chem <- df$phys_chem
    
    dada <- phys_chem %>% 
      select(c(input$variables,input$select_sup_var1))

      
    boxplots1 <- dada %>% 
      gather(key = 'variables', value = 'values', 1:length(dada)-1) 
    
    boxplots11<- ggboxplot(boxplots1, x = 'variables', y = 'values', color = input$select_sup_var1, fill = input$select_sup_var1,
                alpha = 0.2, add = c("mean","jitter"), outlier.shape = 10
                ) +
      stat_summary(fun = mean, geom = "point", shape = 18, size = 3,
                   color = "darkred",
                   position = position_dodge(width = 1)) +
      stat_summary(fun = mean, colour = "red",
                    geom = "text", vjust = -0.7,  size = 5,
                    aes(label = round(..y.., digits = 2))) 
    
    
  
      boxplots22 <- ggboxplot(boxplots1, x = 'variables', y = 'values', color = input$select_sup_var1, fill = input$select_sup_var1,
                    alpha = 0.2, add = c("mean","jitter"), outlier.shape = 10
   
          ) +
          stat_summary(fun = mean, geom = "point", shape = 18, size = 3,
                       color = "darkred",
                       position = position_dodge(width = 1)) +
          stat_summary(fun = mean, colour = "red",
                       geom = "text", vjust = -0.7,  size = 5,
                       aes(label = round(..y.., digits = 2)))  +
        facet_wrap(~class, ncol=3, scales="free")
      
      if (input$wrap)
        boxplots22
      else boxplots11
  })
  
    ### Correlation matrix
  output$correlation <- renderPlot({
    phys_chem <- df$phys_chem
    corr_plot <- phys_chem %>% 
      select_if(is.numeric) %>% 
      cor()
    pvalues <- phys_chem %>% 
      select_if(is.numeric) %>% 
      cor.mtest(conf.level = 0.95)
    
    corr_plot %>%  corrplot(method = 'square',type = "lower", tl.cex = 1,
                            order = "hclust", p.mat =pvalues$p, insig = "pch", tl.col='black',
                            cl.ratio = 0.2, tl.srt = 45, col = COL2('PiYG'), 
                            addCoef.col = 'black', number.cex =0.9, number.font = 5)
  })
  ### "Bartlett's test
  output$bartlett <- DT::renderDataTable({
    phys_chem <- df$phys_chem
    bartlett <- phys_chem %>% 
      select_if(is.numeric) %>% 
      cortest.bartlett(n=length(phys_chem[c(1:dim(phys_chem)[2])])) %>% 
      data.frame()
    DT::datatable(bartlett, options = list(dom = 't',className = 'dt-center'),rownames = F)
  })
  
  ##### Kaiser, Meyer, Olkin
  output$KMO <- DT::renderDataTable({
    phys_chem <- df$phys_chem
    bartlett <- phys_chem %>% 
      select_if(is.numeric) %>%
      cor() %>%  KMO()
    bartlett <- data.frame(bartlett[1:2])
    DT::datatable(bartlett, options = list(dom = 't',className = 'dt-center'),rownames = T)
      
  })
  ### det
  output$det <- renderValueBox({
    phys_chem <- df$phys_chem
    bartlett <- phys_chem %>% 
      select_if(is.numeric) %>%
      cor() %>%
      det() %>% 
      valueBox(subtitle = 'Determinant of a Matrix',
               icon=icon('fire'),
               color = 'orange') 
  })
  
  ## Summary PCA
  output$summary_pca <- renderPrint({
    phys_chem <- df$phys_chem
    res_pca <- phys_chem %>%
      PCA(scale.unit = T, graph = F, quali.sup = c(input$summarypca)) %>% 
      summary() 
  })
  
  ####________PAG PCA_______####________PAG PCA_______####________PAG PCA_______####________PAG PCA_______####________PAG PCA_______
  
  output$contriVar <- renderUI({
    shiny::req(inputData())
    sliderInput(inputId = 'contriVar', label = 'Contribution Var', min = 1,
                max = dim(inputData())[2], value = dim(inputData())[2], 
                step = 1)
  })
  
  output$contriInd <- renderUI({
    shiny::req(inputData())
    sliderInput(inputId = 'contriInd', label = 'Contribution Ind', min = 1,
                max = dim(inputData())[1], value = dim(inputData())[1],
                step = 1)
  })
  
  output$principal_ca <- renderPlot({
    phys_chem <- df$phys_chem
    phys_chem_std <- phys_chem %>% 
      dplyr::select(is.numeric) %>% 
      scale() %>% data.frame()
    set.seed(123)
    phys_chem_clus <- kmeans(phys_chem_std, centers = input$nclus)
      
    
    phys_chem <- cbind(phys_chem, clus = as.factor(phys_chem_clus$cluster))
    
    res_pca <- phys_chem %>% 
      dplyr::select_if(is.numeric) %>% 
      cbind(phys_chem[input$select_sup_var]) %>% 
      PCA(scale.unit = T, graph = F, quali.sup = input$select_sup_var)
    
    fviz_pca_biplot(res_pca, repel = T, habillage = c(input$select_sup_var),
                                    fill.ind = c(input$select_sup_var),
                                    pointshape = 21, pointsize = 2,
                                    addEllipses =input$ellipse,
                                    ellipse.alpha = 0.05,
                                    ellipse.type = "confidence",
                                    geom.ind =c('text',"point"),
                                    geom.var = c("text", "arrow"),
                                    alpha.var ="contrib",
                                    col.var = 'gray20',
                                    select.var = list(contrib = input$contriVar),
                                    select.ind = list(contrib = input$contriInd),
                                    legend.title = list(fill = "Class", color = "variables"),title = 'PCA BIPLOT with more important samples and variables')

    
  })


####### pag PCA analysis

  output$qualitiescontrib <- renderPlot({
    
    phys_chem <- df$phys_chem
    res_pca <- phys_chem %>%
      select_if(is.numeric) %>% 
      PCA(scale.unit = T, graph = F)
    
    fviz_cos2(res_pca, choice = 'var', axes = 1, fill = 'yellow')/fviz_contrib(res_pca, choice = 'var', axes = 1, fill = 'yellow')|
    fviz_cos2(res_pca, choice = 'var', axes = 2,fill = 'blue')/fviz_contrib(res_pca, choice = 'var', axes = 2, fill = 'blue')|
    fviz_pca_var(res_pca, repel = T, legend.title = list(fill = "Col", color = "variables"), geom = c("arrow","text"))
                                                                                       
  })

  ### pag LBVCV  
  output$LBVCV <- renderPrint({
    phys_chem <- df$phys_chem
    res_pca <- phys_chem %>%
      select_if(is.numeric) %>% 
      PCA(scale.unit = T, graph = F)
    res_pca %>% 
      dimdesc(axes = c(1,2,3), proba = 0.05)
  })
  
  output$qualiticontribindi <- renderPlot({
    phys_chem <- df$phys_chem
    res_pca <- phys_chem %>%
      select_if(is.numeric) %>% 
      PCA(scale.unit = T, graph = F)
    
    fviz_cos2(res_pca, choice = 'ind', axes = 1, fill = 'yellow',top = 15)/fviz_contrib(res_pca, choice = 'ind', axes = 1, fill = 'yellow',top = 15)|
      fviz_cos2(res_pca, choice = 'ind', axes = 2, fill = 'blue',top = 15)/fviz_contrib(res_pca, choice = 'ind', axes = 2, fill = 'blue',top = 15)|
      fviz_pca_ind(res_pca, repel = T, col.ind = phys_chem$class)
  })
  
  output$kmean_grup <- renderPlotly({
    phys_chem <- df$phys_chem
    phys_chem_std <- phys_chem %>% 
      select_if(is.numeric) %>% 
      scale() %>% 
      as.data.frame()
    set.seed(123)
    clus_kmeans <- kmeans(phys_chem_std, centers = input$nclus)
    
    phys_chem_clus <- phys_chem %>% 
      cbind(clus = as.factor(clus_kmeans$cluster)) 
    
    phys_chem_clus_num <- phys_chem %>% 
      cbind(clus = as.factor(clus_kmeans$cluster)) %>% 
      dplyr::select_if(is.numeric)
    
    phys_chem_clus_factor <- phys_chem %>% 
      cbind(clus = as.factor(clus_kmeans$cluster)) %>% 
      dplyr::select_if(is.factor)
      
    phys_chem_clus <- cbind(phys_chem_clus_num, Var = phys_chem_clus_factor[,c(input$categoricalclus)])
    
    phys_chem_clus_long <- phys_chem_clus %>% 
      tibble::rownames_to_column(var = 'names') 
    phys_chem_clus_long <- phys_chem_clus_long %>% 
      gather(key = 'var', value = 'values',3:length(phys_chem_clus_long)-1, factor_key = T)
    
    p<-ggplot(phys_chem_clus_long, aes(x=var, y = values, 
                                       group = Var, color =Var, label = names )) +
      stat_summary(geom = 'line',fun = "mean") +
      geom_point(size = 0.5)+
      theme_classic() +
      theme(text = element_text(size = 14), legend.position = 'bottom', 
            axis.text.x = element_text(angle = 90, hjust = 1,size = 8))+
      labs(title = 'Representación de los grupos por KMeans',
           subtitle = 'Caracteristicas de cada clusters',
           x = '', y = 'valor') 
    ggplotly(p)
    
  })
  
  ### Output Lineal models graphs
  output$lmplots <- renderPlot({
    phys_chem <- df$phys_chem
    phys_chem_std <- phys_chem %>% 
      select_if(is.numeric) %>% 
      scale() %>% 
      as.data.frame()
    set.seed(123)
    clus_kmeans <- kmeans(phys_chem_std, centers = input$nclus)
    phys_chem_clus <- phys_chem %>% 
      cbind(clus = as.factor(clus_kmeans$cluster)) 
    
    phys_chem_clus %>% 
      ggscatter(x = input$numvar1lm, y = input$numvar2lm, add = 'reg.line', color = 'clus',
                palette = "jco", ellipse = input$tf, ellipse.level = 0.95,ellipse.alpha = 0.1,
                ellipse.border.remove = T,mean.point = input$tf,ellipse.type = "norm",
                add.params = list(color = "red")) +
        stat_cor(label.x.npc = "left", label.y.npc = "top", 
                 aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
        stat_regline_equation(label.x.npc = 'center', label.y.npc = 'top')
  })
  
  output$anovaplot <- renderPlot({
    phys_chem <- df$phys_chem
    phys_chem_std <- phys_chem %>% 
      select_if(is.numeric) %>% 
      scale() %>% 
      as.data.frame()
    set.seed(123)
    clus_kmeans <- kmeans(phys_chem_std, centers = input$nclus)
    phys_chem_clus <- phys_chem %>% 
      cbind(clus = as.factor(clus_kmeans$cluster)) 
    
    grouped_ggbetweenstats(data = phys_chem_clus,x = !!rlang::sym(input$a2wA), y = !!rlang::sym(input$a2wC), grouping.var = !!rlang::sym(input$a2wB),
                           results.subtitle = T, messages = T, var.equal = T, p.adjust.method = 'holm'
                           )
                          
    
  })
  
  output$valkmeans <- renderValueBox({
    phys_chem <- df$phys_chem
    set.seed(123)
    phys_chem_std <- phys_chem %>% 
      select_if(is.numeric) %>% 
      scale() %>% 
      as.data.frame()
    set.seed(123)
    clus_kmeans <- kmeans(phys_chem_std, centers = input$nclus)
    dd <- phys_chem %>% 
      dplyr::select_if(is.numeric) %>% 
      dist(method = 'euclidean')
    km_stats <- cluster.stats(dd, clus_kmeans$cluster)
    #km_stats$dunn
    valueBox(round(km_stats$dunn,4),
             subtitle = 'Indice de dunn K-means',
             icon=icon('fire'),
             color = 'orange')
  
  })  
  
  output$partitionkmean <- renderPlot({
    phys_chem <- df$phys_chem
    set.seed(123)
    phys_chem_std <- phys_chem %>% 
      select_if(is.numeric) %>% 
      scale() %>% 
      as.data.frame
    
    dd <- phys_chem %>% 
      dplyr::select_if(is.numeric)   
    set.seed(123)
    clus_kmeans <- kmeans(phys_chem_std, centers = input$nclus)
    sil.km <- silhouette(clus_kmeans$cluster, dist(dd))
    fviz_silhouette(sil.km)
    
  })
  
  output$plotpred <- renderPlot({
    phys_chem <- df$phys_chem
    
    phys_chem_std <- phys_chem %>%
      dplyr::select_if(is.numeric) %>%
      scale()
    
    var_A5 <- data.frame(var_A5 = input$var_A5)
    model_A1 <- lm(var_A1 ~ var_A5, data = phys_chem)
    model_A2 <- lm(var_A2 ~ var_A5, data = phys_chem)
    model_A3 <- lm(var_A3 ~ var_A5, data = phys_chem)
    model_A4 <- lm(var_A4 ~ var_A5, data = phys_chem)
    pred_A1 <- predict(model_A1, var_A5)
    pred_A2 <- predict(model_A2, var_A5)
    pred_A3 <- predict(model_A3, var_A5)
    pred_A4 <- predict(model_A4, var_A5)
    # 
    var_B5 <- data.frame(var_B5 = input$var_B5)
    model_B1 <- lm(var_B1 ~ var_B5, data = phys_chem)
    model_B2 <- lm(var_B2 ~ var_B5, data = phys_chem)
    model_B3 <- lm(var_B3 ~ var_B5, data = phys_chem)
    model_B4 <- lm(var_B4 ~ var_B5, data = phys_chem)
    pred_B1 <- predict(model_B1, var_B5)
    pred_B2 <- predict(model_B2, var_B5)
    pred_B3 <- predict(model_B3, var_B5)
    pred_B4 <- predict(model_B4, var_B5)
    # 
    # 
    prediction = cbind(var_A1 = pred_A1,var_A2 = pred_A2, var_A3 = pred_A3,var_A4 = pred_A4,var_A5 = var_A5,
                        var_B1 = pred_B1,var_B2 = pred_B2,var_B3 = pred_B3,var_B4 = pred_B4,var_B5 = var_B5) %>% as.data.frame()
    row.names(prediction) <- 'pred'
    # 
    original_mean <- attr(phys_chem_std, "scaled:center")
    original_sd <- attr(phys_chem_std, "scaled:scale")
    # 
    prediction_sc <- scale(prediction, center = original_mean, scale = original_sd) %>% 
       as.data.frame()
    ### kmeans
    set.seed(123)
    clus_kmeans <- kmeans(phys_chem_std, centers = input$nclus)
    # ### fin kmeans
    # 
    clus_pred <- predict(clus_kmeans, newdata =prediction_sc)
    prediction <- cbind(prediction, clus = as.factor(clus_pred))
    # 
    
    new_data_clus_pred <- phys_chem %>%
      cbind(clus = as.factor(clus_kmeans$cluster)) %>% 
       dplyr::select(-class, -brand) %>%
       rbind(prediction)
  
    res_pca3 <- PCA(new_data_clus_pred, graph = F,
                    quali.sup = dim(new_data_clus_pred)[2],
                    ind.sup =dim(new_data_clus_pred)[1]
    )
    fviz_pca_biplot(res_pca3,pointshape = 21, pointsize = 2,
                    habillage = dim(new_data_clus_pred)[2],
                    label = c(input$label),
                    invisible = c(input$invisible),
                    addEllipses =TRUE,
                    ellipse.alpha = 0.1,
                    ellipse.type = "norm",
                    alpha.var ="contrib",
                    #col.var = 'black',
                    col.ind.sup = "red",
    )

  })
  
  output$kmeans <- renderValueBox({
    phys_chem <- df$phys_chem
    
    phys_chem_std <- phys_chem %>%
      dplyr::select_if(is.numeric) %>%
      scale()
    
    var_A5 <- data.frame(var_A5 = input$var_A5)
    model_A1 <- lm(var_A1 ~ var_A5, data = phys_chem)
    model_A2 <- lm(var_A2 ~ var_A5, data = phys_chem)
    model_A3 <- lm(var_A3 ~ var_A5, data = phys_chem)
    model_A4 <- lm(var_A4 ~ var_A5, data = phys_chem)
    pred_A1 <- predict(model_A1, var_A5)
    pred_A2 <- predict(model_A2, var_A5)
    pred_A3 <- predict(model_A3, var_A5)
    pred_A4 <- predict(model_A4, var_A5)
    # 
    var_B5 <- data.frame(var_B5 = input$var_B5)
    model_B1 <- lm(var_B1 ~ var_B5, data = phys_chem)
    model_B2 <- lm(var_B2 ~ var_B5, data = phys_chem)
    model_B3 <- lm(var_B3 ~ var_B5, data = phys_chem)
    model_B4 <- lm(var_B4 ~ var_B5, data = phys_chem)
    pred_B1 <- predict(model_B1, var_B5)
    pred_B2 <- predict(model_B2, var_B5)
    pred_B3 <- predict(model_B3, var_B5)
    pred_B4 <- predict(model_B4, var_B5)
    # 
    # 
    prediction = cbind(var_A1 = pred_A1,var_A2 = pred_A2, var_A3 = pred_A3,var_A4 = pred_A4,var_A5 = var_A5,
                       var_B1 = pred_B1,var_B2 = pred_B2,var_B3 = pred_B3,var_B4 = pred_B4,var_B5 = var_B5) %>% as.data.frame()
    row.names(prediction) <- 'pred'
    original_mean <- attr(phys_chem_std, "scaled:center")
    original_sd <- attr(phys_chem_std, "scaled:scale")
    prediction_sc <- scale(prediction, center = original_mean, scale = original_sd) %>% 
      as.data.frame()
    ### kmeans
    set.seed(123)
    clus_kmeans <- kmeans(phys_chem_std, centers = input$nclus)
    clus_pred <- predict(clus_kmeans, newdata =prediction_sc)

  valueBox(clus_pred,
           subtitle = 'Cluster prediction',
           icon=icon('fire'),
           color = 'orange')
  
  })
  
  output$plotpredrad <- renderPlotly({
    phys_chem <- df$phys_chem
    phys_chem_std <- phys_chem %>%
      dplyr::select_if(is.numeric) %>%
      scale()
    var_A5 <- data.frame(var_A5 = input$var_A5)
    model_A1 <- lm(var_A1 ~ var_A5, data = phys_chem)
    model_A2 <- lm(var_A2 ~ var_A5, data = phys_chem)
    model_A3 <- lm(var_A3 ~ var_A5, data = phys_chem)
    model_A4 <- lm(var_A4 ~ var_A5, data = phys_chem)
    pred_A1 <- predict(model_A1, var_A5)
    pred_A2 <- predict(model_A2, var_A5)
    pred_A3 <- predict(model_A3, var_A5)
    pred_A4 <- predict(model_A4, var_A5)
    var_B5 <- data.frame(var_B5 = input$var_B5)
    model_B1 <- lm(var_B1 ~ var_B5, data = phys_chem)
    model_B2 <- lm(var_B2 ~ var_B5, data = phys_chem)
    model_B3 <- lm(var_B3 ~ var_B5, data = phys_chem)
    model_B4 <- lm(var_B4 ~ var_B5, data = phys_chem)
    pred_B1 <- predict(model_B1, var_B5)
    pred_B2 <- predict(model_B2, var_B5)
    pred_B3 <- predict(model_B3, var_B5)
    pred_B4 <- predict(model_B4, var_B5)
    prediction = cbind(var_A1 = pred_A1,var_A2 = pred_A2, var_A3 = pred_A3,var_A4 = pred_A4,var_A5 = var_A5,
                       var_B1 = pred_B1,var_B2 = pred_B2,var_B3 = pred_B3,var_B4 = pred_B4,var_B5 = var_B5) %>% as.data.frame()
    row.names(prediction) <- 'pred'
    original_mean <- attr(phys_chem_std, "scaled:center")
    original_sd <- attr(phys_chem_std, "scaled:scale")
    prediction_sc <- scale(prediction, center = original_mean, scale = original_sd) %>% 
      as.data.frame()

    set.seed(123)
    clus_kmeans <- kmeans(phys_chem_std, centers = input$nclus)
    clus_pred <- predict(clus_kmeans, newdata =prediction_sc)
    prediction <- cbind(prediction, clus = as.factor(clus_pred))
    # 
    new_data_clus_pred <- phys_chem %>%
      cbind(clus = as.factor(clus_kmeans$cluster)) %>% 
      dplyr::select(-class, -brand) %>%
      rbind(prediction)    

    data_long_by_clus_scale <- new_data_clus_pred %>% 
      select_if(is.numeric) %>%
      scale() %>%
      cbind(clus = new_data_clus_pred$clus) %>% data.frame() %>% 
      tibble::rownames_to_column(var = 'names') %>% 
      gather(key = 'var', value = 'values',2:11, factor_key = T) %>% 
      group_by(clus, var) %>% 
      get_summary_stats(values, type ="mean_sd")

    prediction_scale_long <- prediction_sc %>% 
      cbind(clus = as.factor(clus_pred)) %>% 
      tibble::rownames_to_column(var = 'names') %>% 
      gather(key = 'var', value = 'values', 2:11) %>% 
      group_by(clus, var) %>% 
      get_summary_stats(values, type ="mean")
    
    clus_1 <- subset(data_long_by_clus_scale, data_long_by_clus_scale$clus ==1) 
    clus_2 <- subset(data_long_by_clus_scale, data_long_by_clus_scale$clus ==2)
    clus_3 <- subset(data_long_by_clus_scale, data_long_by_clus_scale$clus ==3)
    clus_4 <- subset(data_long_by_clus_scale, data_long_by_clus_scale$clus ==4)
    clus_5 <- subset(data_long_by_clus_scale, data_long_by_clus_scale$clus ==5)
    clus_6 <- subset(data_long_by_clus_scale, data_long_by_clus_scale$clus ==6)
    
    fig <- plot_ly(
      type = 'scatterpolar',
      mode = 'lines',
      fill = 'toself'
    ) 
    
    fig <- fig %>% 
      add_trace(
        type = 'scatterpolar',
        mode = 'lines',
        r = clus_1$mean,
        theta =clus_1$var,
        mode = 'markers',
        fill = 'toself',
        name = 'Clus 1'
      )
    fig <- fig %>% 
      add_trace(
        type = 'scatterpolar',
        mode = 'lines',
        r = clus_2$mean,
        theta =clus_2$var,
        mode = 'markers',
        fill = 'toself',
        name = 'Clus 2'
      )
    fig <- fig %>% 
      add_trace(
        type = 'scatterpolar',
        mode = 'lines',
        r = clus_3$mean,
        theta =clus_3$var,
        mode = 'markers',
        fill = 'toself',
        name = 'Clus 3'
      )
    fig <- fig %>% 
      add_trace(
        type = 'scatterpolar',
        mode = 'lines',
        r = clus_4$mean,
        theta =clus_4$var,
        mode = 'markers',
        fill = 'toself',
        name = 'Clus 4'
      )
    fig <- fig %>% 
      add_trace(
        type = 'scatterpolar',
        mode = 'lines',
        r = clus_5$mean,
        theta =clus_5$var,
        mode = 'markers',
        fill = 'toself',
        name = 'Clus 5'
      )
    fig <- fig %>% 
      add_trace(
        type = 'scatterpolar',
        mode = 'lines',
        r = clus_6$mean,
        theta =clus_6$var,
        mode = 'markers',
        fill = 'toself',
        name = 'Clus 6'
      )
    fig <- fig %>% 
      add_trace(
        type = 'scatterpolar',
        mode = 'lines',
        r = prediction_scale_long$mean,
        theta =prediction_scale_long$var,
        mode = 'markers',
        fill = 'toself',
        name = 'prediction'
      )
    fig <- fig %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T
          )
        )
      )
    
      fig
  })
  
  output$tableclus <- DT::renderDataTable({
    phys_chem <- df$phys_chem
    phys_chem_std <- phys_chem %>%
      dplyr::select_if(is.numeric) %>%
      scale()
    var_A5 <- data.frame(var_A5 = input$var_A5)
    model_A1 <- lm(var_A1 ~ var_A5, data = phys_chem)
    model_A2 <- lm(var_A2 ~ var_A5, data = phys_chem)
    model_A3 <- lm(var_A3 ~ var_A5, data = phys_chem)
    model_A4 <- lm(var_A4 ~ var_A5, data = phys_chem)
    pred_A1 <- round(predict(model_A1, var_A5),2)
    pred_A2 <- round(predict(model_A2, var_A5),2)
    pred_A3 <- round(predict(model_A3, var_A5),2)
    pred_A4 <- round(predict(model_A4, var_A5),2)
    var_B5 <- data.frame(var_B5 = input$var_B5)
    model_B1 <- lm(var_B1 ~ var_B5, data = phys_chem)
    model_B2 <- lm(var_B2 ~ var_B5, data = phys_chem)
    model_B3 <- lm(var_B3 ~ var_B5, data = phys_chem)
    model_B4 <- lm(var_B4 ~ var_B5, data = phys_chem)
    pred_B1 <- round(predict(model_B1, var_B5),2)
    pred_B2 <- round(predict(model_B2, var_B5),2)
    pred_B3 <- round(predict(model_B3, var_B5),2)
    pred_B4 <- round(predict(model_B4, var_B5),2)
    prediction = cbind(var_A1 = pred_A1,var_A2 = pred_A2, var_A3 = pred_A3,var_A4 = pred_A4,var_A5 = var_A5,
                       var_B1 = pred_B1,var_B2 = pred_B2,var_B3 = pred_B3,var_B4 = pred_B4,var_B5 = var_B5) %>% as.data.frame()
    row.names(prediction) <- 'pred'
    original_mean <- attr(phys_chem_std, "scaled:center")
    original_sd <- attr(phys_chem_std, "scaled:scale")
    prediction_sc <- scale(prediction, center = original_mean, scale = original_sd) %>%
      as.data.frame()
    # ### kmeans
    set.seed(123)
    clus_kmeans <- kmeans(phys_chem_std, centers = input$nclus)
    # # ### fin kmeans
    clus_pred <- predict(clus_kmeans, newdata =prediction_sc)
    prediction <- cbind(prediction, clus = as.factor(clus_pred))
    # #
    new_data_clus_pred <- phys_chem %>%
      cbind(clus = as.factor(clus_kmeans$cluster)) %>%
      dplyr::select(-class, -brand) %>%
      rbind(prediction)

    DT::datatable(new_data_clus_pred,
                  filter = 'top',
                  extensions = c('Scroller'),
                  options = list(scrollY=600, 
                                 scrollX=300, 
                                 scroller = TRUE,
                                 deferRender = TRUE,
                                 pageLength = 250))
  })
  
  
  
  
}
shinyApp(ui, server)

