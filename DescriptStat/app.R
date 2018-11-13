library("shiny")
library(sjPlot)
ui = fluidPage(
  titlePanel("Descriptive Statistics"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose a file to upload',
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
                   '"')
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Numeric variables",
                 htmlOutput("varNumSelect"),
                 uiOutput("contVar")),
        tabPanel("Factor variables",
                 htmlOutput("varFactSelect"),
                 uiOutput("FreqVar"))
      )
    )
  ))

server<-function(input,output){
  
  myData <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    read.csv(inFile$datapath, header = input$header,
             sep = input$sep, quote = input$quote)
  })
  
  output$varNumSelect <- renderUI({
    x=sapply(myData(),class)
    i=which(x!="factor")
    varNames=colnames(myData())[i]
    if (identical(varNames, c()) ) return(NULL)
    else selectInput("varsNum", "Select variables",
                     varNames, varNames, multiple =T,width = '100%')            
  })
  
  
  output$varFactSelect <- renderUI({
    x=sapply(myData(),class)
    i=which(x=="factor")
    varNames=colnames(myData())[i]
    if (identical(varNames, c()) ) return(NULL)
    else selectInput("varsFact", "Select one variable",
                     varNames, varNames[1], multiple =F)           
  })
  output$FreqVar<-renderText({
    validate(
      need(input$file1 != "", "Please upload your csv file")
    )
    xCSS = list(css.table = "border: 2px solid;",
                css.tdata = "border: 1px solid;",
                css.firsttablecol = "color:#003399;
                font-weight:bold;")
    df=myData()
    if (identical(df, '') || identical(df,data.frame())) return(NULL)
    else{
      j=which(colnames(df)==input$varsFact)
      if(identical(j,NA)) return(NULL)
      else {
        x=df[,j]
        sjt.frq(x,ignore.strings = F,
                title =input$varsFact,CSS=xCSS)$knitr
      }
    }
  })
  output$contVar<-renderText({
    validate(
      need(input$file1 != "", "Please upload your csv file")
    )
    xCSS = list(css.table = "border: 2px solid;",
                css.tdata = "border: 1px solid;",
                css.firsttablecol = "color:#003399;
                font-weight:bold;")
    df=myData()
    if (identical(df, '') || identical(df,data.frame())) return(NULL)
    j=which(colnames(df)%in%input$varsNum)
    x=as.data.frame(df[,j])
    if (identical(x, '') || identical(x,data.frame())) return(NULL)
    else sjt.df(x,CSS=xCSS)$knitr
  })
  
}
shinyApp(ui=ui,server=server)
