library(shiny)
library(corrplot)
library(colourpicker)
library(ggplot2)
##
library(gridExtra)

cor.mtest <- function(mat, alternative=alternative,conf.level = 0.95) {
     mat <- as.matrix(mat)
       n <- ncol(mat)
       p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
       diag(p.mat) <- 0
       diag(lowCI.mat) <- diag(uppCI.mat) <- 1
       for (i in 1:(n - 1)) {
             for (j in (i + 1):n) {
                   tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level,
                                   alternative=alternative )
                  p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
                   lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
                   uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]
               }
        }
      return(list(p.mat, lowCI.mat, uppCI.mat))
   }


ui = fluidPage(
  titlePanel("Correlation Matrix: Visualization and Independence tests"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Upload your  CSV File',
                accept=c('text/csv',
                         'text/comma-separated-values,text/plain',
                         '.csv')),
      htmlOutput("varSelect")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(h3("Visualization"),
                 tabsetPanel(
                   tabPanel(h4("Single represensation"),
                    column(4,wellPanel(h3("Customizing the graph"),
                                     selectInput(inputId = "method_cor",label = "Correlation type",
                                                 choices = c("pearson", "kendall", "spearman"),selected = "pearson",multiple = F),
                                     selectInput(inputId = "method",label = "Method",
                                                 choices =  c("circle", "square", "ellipse",
                                                              "number", "shade","color", "pie"),selected = "circle",multiple = F),
                                     selectInput(inputId = "type",label = "Type",choices = c("full", "lower", "upper"),
                                                 selected = "full",multiple = F))),
                 column(4,wellPanel("",
                                     selectInput(inputId = "order",label = "Ordering",
                                                 choices =  c("original", "AOE", "FPC", "hclust", "alphabet"),
                                                 selected = "original",multiple = F),
                                    selectInput(inputId = "tl.pos",label = "Position of text labels.",
                                                choices =  c("left and top", "left and diagonal", "top and diagonal", "diagonal only", "nothing"),
                                                selected = "original",multiple = F),
                                     htmlOutput("hclust_method"),
                                    htmlOutput("hclust_numb_rect"))),
                                     #downloadButton('downloadGraph', 'Download'),
                  plotOutput("graph", height="800px")
                 ),
                 tabPanel(h4("Mixed represensation"),
                          column(4,wellPanel(h3("Customizing the graph"),
                                             selectInput(inputId = "method_cor_m",label = "Correlation type",
                                                         choices = c("pearson", "kendall", "spearman"),selected = "pearson",multiple = F),
                                             selectInput(inputId = "lower",label = "Lower",
                                                         choices =  c("circle", "square", "ellipse",
                                                                      "number", "shade","color", "pie"),selected = "circle",multiple = F),
                                             selectInput(inputId = "upper",label = "Upper",
                                                         choices =  c("circle", "square", "ellipse",
                                                                      "number", "shade","color", "pie"),selected = "circle",multiple = F)
                                             )),
                          column(4,wellPanel("",
                                             selectInput(inputId = "order_m",label = "Ordering",
                                                         choices =  c("original", "AOE", "FPC", "hclust", "alphabet"),
                                                         selected = "original",multiple = F),
                                             selectInput(inputId = "tl.pos_m",label = "Position of text labels",
                                                         choices = c("left and top", "diagonal", "nothing"),
                                                         selected = "nothing",multiple = F),
                                             selectInput(inputId = "diag_m",label = "Diagonal",
                                                         choices =   c("nothing", "lower triangle", "upper triangle"),
                                                         selected = T,multiple = F),
                                             htmlOutput("hclust_method_m"))),
                                             #downloadButton('downloadGraph_m', 'Download'),
                          plotOutput("graph_m", height="800px")
                          )
                 )),
        tabPanel(h3("Pearson-Indepence Test"),
                 column(4,wellPanel(h3(""),
                                     selectInput(inputId = "method_p",label = "Method",
                                                choices =  c("circle", "square", "ellipse",
                                                             "number", "shade","color", "pie"),selected = "circle",multiple = F),
                                    selectInput(inputId = "type_p",label = "Type",choices = c("full", "lower", "upper"),
                                                selected = "full",multiple = F),
                                    selectInput(inputId = "order_p",label = "Ordering",
                                                choices =  c("original", "AOE", "FPC", "hclust", "alphabet"),
                                                selected = "original",multiple = F),
                                    htmlOutput("hclust_method_p"))),
                 column(4,wellPanel("",
                                       sliderInput(inputId = "sig_alpha","Siginificant level",min = 0,max = 1,value = 0.05),
                                       selectInput(inputId = "alternative","Alternative hypothesis",choices =  c("two.sided", "less", "greater"),
                                                   selected="two.sided",multiple = F),
                                       selectInput(inputId = "insig",label = "Insignificant coefficient",
                                                   choices = c("pch","p-value","blank","n"),selected="pch",multiple=F))),
                                       #downloadButton('downloadGraph_p', 'Download'),
                plotOutput("graph_p", height="800px")),
        tabPanel(h3("Active Data"),tableOutput('table'))
      )
    )
  )
)
  

server = function(input, output, session){
  
  myData <- reactive({
     inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    data <- read.csv(inFile$datapath, header = TRUE,row.names=1)
    data
  })
  

  
  
  output$varSelect <- renderUI({
    
    #if (identical(myData(), '') || identical(myData(),data.frame())) return(NULL)
    
    x=sapply(myData(),class)
    x=(x=="numeric")
    df=myData()[,x]
    
    if (identical(df, '') || identical(df,data.frame())) return(NULL)
    
    # Variable selection:    
    selectInput("varsSelect_s", "Variables to use:",
                names(df), names(df), multiple =TRUE)            
  })
  
  output$table <- renderTable({
    
    if (is.null(input$varsSelect_s) || length(input$varsSelect_s)==0) return(NULL)
    
    return(myData()[,input$varsSelect_s,drop=FALSE])
  })
  
  ### Correlation
  
  
 output$hclust_method <- renderUI({
   if( input$order=="hclust"){
     selectInput(inputId = "hclust_method_s",label = "Hclust method",
                 choices = c("complete", "ward", "ward.D", "ward.D2", "single", "average", "mcquitty", "median", "centroid"),
                 selected = "ward",multiple = F)
   }
 })
 
 output$hclust_numb_rect <- renderUI({
   if( input$order=="hclust"){
     numericInput(inputId = "rect",label = "Number of rectangles",value = 2,min = 2,max = 9)
   }
 })
 
corr_mat<-reactive({
  X=myData()[,input$varsSelect_s,drop=FALSE]
  M=cor(x = X,method = input$method_cor)
  M
})

corr_plot <- reactive({
  
 # tl.pos.para="n"
 # if(input$tl.pos=="left and top") tl.pos.para="lt"
 # if(input$tl.pos=="left and diagonal") tl.pos.para="dl"
 # if(input$tl.pos=="top and diagonal") tl.pos.para="td"
 # if(input$tl.pos=="diagonal only") tl.pos.para="d"
  
  if (is.null(myData())) return(NULL)
  
  
  M=corr_mat()
  if(input$order=="hclust"){
    corrplot(M,method = input$method,type = input$type,
             order = input$order,hclust.method = input$hclust_method_s,
             addrect = input$rect
    )
  }
  else 
    corrplot(M,method = input$method,type = input$type,
             order = input$order)
  
})


output$graph <- renderPlot({
  corr_plot()
})


output$downloadGraph <- downloadHandler(
  filename = "fig.png",
  content = function(file) {
    device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
    ggsave(file, plot = corr_plot(), device = device)
  }
)

### Mized correlation

corr_mat_m<-reactive({
  X=myData()[,input$varsSelect_s,drop=FALSE]
  M=cor(x = X,method = input$method_cor_m)
  M
})


output$hclust_method_m <- renderUI({
  if( input$order_m=="hclust"){
    selectInput(inputId = "hclust_method_s_m",label = "Hclust method",
                choices = c("complete", "ward", "ward.D", "ward.D2", "single", "average", "mcquitty", "median", "centroid"),
                selected = "ward",multiple = F)
  }
})


corr_plot_m <- reactive({
  
  diag_m.para="n"
  if(input$diag_m=="lower triangle") diag_m.para="l"
  if(input$diag_m=="upper triangle") diag_m.para="u"
  
  tl.pos_m.para="n"
  if(input$tl.pos_m=="diagonal") tl.pos_m.para="d"
  if(input$tl.pos_m=="left and diagonal") tl.pos_m.para="ld"
  
  
  M=corr_mat_m()
  
  corrplot.mixed(M,lower = input$lower, 
                 upper = input$upper, tl.pos = tl.pos_m.para,
                 order = input$order_m,hclust.method = input$hclust_method_s_m,
                 diag = diag_m.para)
})


output$graph_m <- renderPlot({
  corr_plot_m()
})


output$downloadGraph <- downloadHandler(
  filename = "fig_mixed.png",
  content = function(file) {
    device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
    ggsave(file, plot = corr_plot_m(), device = device)
  }
)


### p-value matrix

corr_mat_p<-reactive({
  X=myData()[,input$varsSelect_s,drop=FALSE]
  M=cor(x = X,method = "pearson")
  M
})

output$hclust_method_p <- renderUI({
  if( input$order_p=="hclust"){
    selectInput(inputId = "hclust_method_s_p",label = "Hclust method",
                choices = c("complete", "ward", "ward.D", "ward.D2", "single", "average", "mcquitty", "median", "centroid"),
                selected = "ward",multiple = F)
  }
})

output$hclust_numb_rect_p <- renderUI({
  if( input$order_p=="hclust"){
    numericInput(inputId = "rect_p",label = "Number of rectangles",value = 2,min = 2,max = 9)
  }
})

corr_pv<-reactive({
  X=myData()[,input$varsSelect_s,drop=FALSE]
  res1 <- cor.mtest(X, alternative=input$alternative,
                    conf.level = input$sig_alpha)
  res1[[1]]
})



corr_plot_p <- reactive({
  
  # tl.pos.para="n"
  # if(input$tl.pos=="left and top") tl.pos.para="lt"
  # if(input$tl.pos=="left and diagonal") tl.pos.para="dl"
  # if(input$tl.pos=="top and diagonal") tl.pos.para="td"
  # if(input$tl.pos=="diagonal only") tl.pos.para="d"
  
  if (is.null(myData())) return(NULL)
  
  
  M=corr_mat_p()
  if(input$order=="hclust"){
    g<-corrplot(M,method = input$method_p,type = input$type_p, p.mat = corr_pv(),insig=input$insig,sig.level = input$sig_alpha,
             order = input$order_p,hclust.method = input$hclust_method_s_p
    )
  }
  else 
    g<-corrplot(M,method = input$method_p,type = input$type_p, p.mat = corr_pv(),insig=input$insig,sig.level = input$sig_alpha,
             order = input$order_p)
 g
})


output$graph_p <- renderPlot({
  corr_plot_p()
})


output$downloadGraph_p <- downloadHandler(
  filename = "fig_pval.png",
  content = function(file) {
    device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
    ggsave(file, plot = corr_plot(), device = device)
   
  }
)



}


shinyApp(ui=ui,server=server)
