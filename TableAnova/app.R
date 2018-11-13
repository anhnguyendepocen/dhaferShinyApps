library(shiny)
library(agricolae)
library(plyr)
library(DT)
library(ggplot2)
library(multcompView)


### functions

TableAnova2=function(X,alpha){
  library(agricolae)
  library(plyr)
  X=as.data.frame(X)
  X=na.omit(X)
  X[,1]=factor(X[,1])
  X[,2]=factor(X[,2])
  cond1=levels(X[,1])
  cond2=levels(X[,2])
  p=length(cond1)
  Y=X[,-c(1,2)]
  q=ncol(Y)
  res1=list()
  for(i in 1:q){
    res=list()
    for(j in 1:length(cond1)){
      res[[j]]=rep(NA,length(cond2))
      names(res[[j]])=cond2
      x=Y[X[,1]==cond1[j],i]
      a=X[X[,1]==cond1[j],2]
      a=droplevels(a)
      l=length(levels(a))
      ll=length(unique(x))
      if(ll>1){
        if(l>1){
          m1=aov(x~a) 
          z=HSD.test(m1,"a", group=TRUE,console = F,alpha = alpha)
          zsd=tapply(x,a,sd)
          xx=paste(round(z$groups$x,3),"(",round(zsd,3),")",z$groups$groups,sep="")
          names(xx)=rownames(z$groups)
          ii=match(levels(a),names(xx))
          xx=xx[ii]
          res[[j]][names(res[[j]])%in%names(xx)]=xx
        }
      }
    }
    names(res)=cond1
    res1[[i]]=ldply(res)
    colnames(res1[[i]])[1]=colnames(X)[1]
  }
  names(res1)=colnames(Y)
  res2=ldply(res1)
  colnames(res2)[1]="Variable"
  return(res2)
}


TableAnova1=function(X,alpha){
  library(agricolae)
  library(plyr)
  X=as.data.frame(X)
  X=na.omit(X)
  
  cond1=levels(X[,1])
  
  p=length(cond1)
  Y=X[,-1]
  q=ncol(Y)
  res1=list()
  for(i in 1:q){
    x=Y[,i]
    ll=length(unique(x))
    a=X[,1]
    a=droplevels(a)
    l=length(unique(levels(a)))
    if(ll>1){
      if(l>1){
        m1=aov(x~a) 
        z=HSD.test(m1,"a", group=TRUE,console = F,alpha = alpha)
        zsd=tapply(x,a,sd)
        xx=paste(round(z$groups$x,3),"(",round(zsd,3),")",z$groups$groups,sep="")
        names(xx)=rownames(z$groups)
        ii=match(levels(a),names(xx))
        xx=xx[ii]
        res1[[i]]=xx 
      }
    }
  }
  
  names(res1)=colnames(Y)
  res2=ldply(res1)
  colnames(res2)[1]="Variable"
  return(res2)
}

generate_label_df <- function(d,HSD, flev){
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- HSD[[flev]][,4]
  Tukey.labels <- multcompLetters(Tukey.levels)['Letters']
  plot.labels <- names(Tukey.labels[['Letters']])
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  # upper quantile and label placement
  boxplot.df <- ddply(d, flev, function (x) max(fivenum(x$y)) + 0.2)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  plot.levels <- data.frame(plot.labels, labels = Tukey.labels[['Letters']],
                            stringsAsFactors = FALSE)
  
  # Merge it with the labels
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  return(labels.df)
}

draw_tukey=function(d,alpha_sig=.05,alpha_tr=.5,xlab,ylab){
  a <- aov(y~lev, data=d)
  tHSD <- TukeyHSD(a, ordered = FALSE, conf.level = alpha_sig)
  xc=generate_label_df(d,tHSD, 'lev')
  z=mapvalues(d$lev,from=xc$plot.labels,to=xc$labels)
  d$z=z
  p_base <- ggplot(d, aes(x=lev, y=y),size=2) + geom_boxplot(aes(fill=z,col=z,alpha=alpha_tr)) + geom_jitter()+
    geom_text(data = xc, aes(x = plot.labels, y = V1, label = labels),size=5)+xlab(xlab)+ylab(ylab)
  p_base<-p_base+theme_bw()+theme(legend.position = "none")+coord_flip()
  
}

ui = fluidPage(
  titlePanel("Table of One-way and Two-way ANOVA with post-hoc Tukey HSD (Honestly Significant Difference)"),
   tags$p("This app can help you to perform a One-way and Two-way ANOVA with post-hoc Tukey HSD. 
         Your data has to be in a CSV file containing at least one factor variable and one Quantitative variable. 
 The result of this analysis is displayed in a table that can be downloaded in a CSV file.  
         We had used in this  app the R package agricolae. If you want to learn more about this method you can visit this website"),
  tags$a(href="http://tarwi.lamolina.edu.pe/~fmendiburu/","R-Agricolae Package"),
   tabsetPanel(
    tabPanel("One-way ANOVA",
              sidebarLayout(
                sidebarPanel(
                  fileInput('file1', 'Upload your  CSV File',
                            accept=c('text/csv',
                                     'text/comma-separated-values,text/plain',
                                     '.csv')),
                  htmlOutput("varselectOne"),
                  htmlOutput("factorVar"),
                  sliderInput(inputId = "alpha1",label = "Significant level",min = 0,max = 1,value = .05),
                  tags$div(class="header", checked=NA,
                           tags$h4("This Shiny app is powered by"),
                           tags$h4("Dhafer Malouche"),
                           tags$a(href="http://dhafermalouche.net", "http://dhafermalouche.net")
                           
                  )
                ),
                mainPanel(
                  tabsetPanel(
                    tabPanel("ANOVA Table",
                             column(6,
                                    downloadButton('downloadDataOneFactor', 'Download result (csv)')),
                             dataTableOutput("anova1")
                    ),
                    tabPanel("ANOVA graph",
                             column(6,wellPanel(h2("One-way ANOVA"),
                                                htmlOutput("quant_var"),
                                                htmlOutput("alpha_tr"))),
                             column(6,
                                    downloadButton('downloadGraphOneFactor', 'Download')),
                             plotOutput("anova_plot1", height="800px")
                             )
                  )
                )
              )
              
  ),
  tabPanel("Two-way ANOVA",
              sidebarLayout(
                sidebarPanel(
                  fileInput('file2', 'Upload your CSV File',
                            accept=c('text/csv',
                                     'text/comma-separated-values,text/plain',
                                     '.csv')),
                  htmlOutput("varselectTwo"),
                  htmlOutput("factorVar1"),
                  htmlOutput("factorVar2"),
                  sliderInput(inputId = "alpha2",label = "Significant level",min = 0,max = 1,value = .05),
                  tags$div(class="header", checked=NA,
                           tags$h4("This Shiny app is powered by"),
                           tags$h4("Dhafer Malouche"),
                           tags$a(href="http://dhafermalouche.net", "http://dhafermalouche.net")
                  )
                ),
                mainPanel(
                  column(6,
                         downloadButton('downloadDataTwoFactor', 'Download result (csv)')),
                  dataTableOutput("anova2")
                )
              )
              )
    )
)



server = function(input, output, session){
  
  myData1 <- reactive({
   # validate(
   #   need(input$myData1 != "", "")
   # )
    
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    data <- read.csv(inFile$datapath, header = TRUE,row.names=1)
    data
  })
  
  myData2 <- reactive({
    #validate(
    #  need(input$myData2 != "", "")
    #)
    inFile <- input$file2
    if (is.null(inFile)) return(NULL)
    data <- read.csv(inFile$datapath, header = TRUE,row.names=1)
    data
  })
  
## for one factor
  
  output$varselectOne <- renderUI({
    
    #if (identical(myData(), '') || identical(myData(),data.frame())) return(NULL)
    
    x=sapply(myData1(),class)
    x=(x=="numeric")
    df=myData1()[,x]
    
    if (identical(df, '') || identical(df,data.frame())) return(NULL)
    
    # Variable selection:    
    selectInput("varsOne", "Variables to use:",
                names(df), names(df), multiple =TRUE)            
  })
  
## for two factors
  
  output$varselectTwo <- renderUI({
    
    #if (identical(myData(), '') || identical(myData(),data.frame())) return(NULL)
    
    x=sapply(myData2(),class)
    x=(x=="numeric")
    df=myData2()[,x]
    
    if (identical(df, '') || identical(df,data.frame())) return(NULL)
    
    # Variable selection:    
    selectInput("varsTwo", "Quantitative variables to use:",
                names(df), names(df), multiple =TRUE)            
  })
  
## Factor Var for one factor ANOVA
  
  output$factorVar <- renderUI({
    
    
    x=sapply(myData1(),class)
    x=(x=="factor")
    df=names(myData1())[x]
    #if (identical(df, '') || identical(df,data.frame())) return(NULL)
    # Variable selection:    
    selectInput("factorVar_i", "Factor variable",
                df, df, multiple =F)            
  })
  
## Factor Var for two factor ANOVA
  ## Variable 1

  output$factorVar1 <- renderUI({
    
    
    x=sapply(myData2(),class)
    x=(x=="factor")
    df=names(myData2())[x]
    #if (identical(df, '') || identical(df,data.frame())) return(NULL)
    # Variable selection:    
    selectInput("factorVar1_i", "Factor variable 1",
                df, df, multiple =F)            
  })
  
  ## Variable 2
  
  output$factorVar2 <- renderUI({
    
    
    x=sapply(myData2(),class)
    x=(x=="factor")
    df=names(myData2())[x]
    #if (identical(df, '') || identical(df,data.frame())) return(NULL)
    # Variable selection:    
    selectInput("factorVar2_i", "Factor variable 2",
                df, df, multiple =F)            
  })
  
## Performing ANOVA one factor
  
PrefANOVA1 <- reactive({
  Z=myData1()
  variables=c(input$factorVar_i,input$varsOne)
  X=Z[,variables]
  res1=TableAnova1(X,alpha=input$alpha1)
  res1
  })

## Performing ANOVA two factors

PrefANOVA2 <- reactive({
  Z=myData2()
  variables=c(input$factorVar1_i,input$factorVar2_i,input$varsOne)
  X=Z[,variables]
  res2=TableAnova2(X,alpha=input$alpha2)
  res2
  
})

### Outputing results.


output$anova1 <-renderDataTable({
  df=PrefANOVA1()
  datatable(df,rownames = F,options = list(
    columnDefs = list(list(className = 'dt-center',width = '200px'))
  ))
})

output$anova2 <-renderDataTable({
  df=PrefANOVA2()
  datatable(df,rownames = F,options = list(
    columnDefs = list(list(className = 'dt-center',width = '200px'))
  ))
})


## Downloading results

output$downloadDataOneFactor <- downloadHandler(
  filename = function() { paste(input$file1, '_ANOVA1.csv', sep='') },
  content = function(file) {
    write.csv(PrefANOVA1(), file)
  }
)


output$downloadDataTwoFactor <- downloadHandler(
  filename = function() { paste(input$file1, '_ANOVA2.csv', sep='') },
  content = function(file) {
    write.csv(PrefANOVA2(), file)
  }
)


## Graph ANOVA  One factor



output$quant_var <- renderUI({
  selectInput("varsOne_g", "Variables to use:",choices = input$varsOne, selected=input$varsOne[1], multiple =F)            
})


output$alpha_tr <- renderUI({

  sliderInput(inputId =  "alpha_tr_g",label = "alpha (Transparency)",min = 0,max = 1,value = .6)
})


output$geom <- renderUI({
  selectInput("geom_g", "Geom:",choices = c("Box plot","Violon","Error bar"), selected="Box plot", multiple =F)  
})

graph_ANOVA1 <-reactive({
  Z=myData1()
  variables=c(input$factorVar_i,input$varsOne_g)
  d=Z[,variables]
  colnames(d)=c("lev","y")
  draw_tukey(d,alpha_sig = input$alpha1,alpha_tr = input$alpha_tr_g,xlab = input$factorVar_i,ylab=input$varsOne_g)
})

output$anova_plot1 <-renderPlot({
  print(graph_ANOVA1())
})




output$downloadGraphOneFactor <- downloadHandler(
  filename = "fig.png",
  content = function(file) {
    device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
    ggsave(file, plot = graph_ANOVA1(), device = device)
  }
)

  ### final
}


shinyApp(ui=ui,server=server)

