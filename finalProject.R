#Install packages if needed
#install.packages('quantmod')
#install.packages('googleVis')

require(quantmod)
require(shiny)
require(rvest)
require(magrittr)
require(shinyjs)
require(quantmod)
require(googleVis)
#Get name list of s&p 500 companies or tickers
page=read_html("http://www.barchart.com/stocks/sp500.php?_dtp1=0")
tmp=matrix(page %>% html_nodes("#dt1 a") %>% 
             html_text(trim=TRUE),ncol=2,byrow=TRUE)
df=data.frame(
  ticker=tmp[,1],
  company=tmp[,2],
  stringsAsFactors=FALSE
)
spnames=df$company
sptickers=df$ticker

shinyApp(
  # Shiny UI
  ui = fluidPage(
    useShinyjs(),
    titlePanel("How to get rich-Created by Isabella Yang and Jaslyn Zhang"),
    # Create title panel
    sidebarLayout(
      sidebarPanel (
        #actionButton("ticker","Search company 1 by ticker name"),
        selectInput("company1ticker","Choose a ticker",sptickers),
        
        #Allow users to choose what price they want to see
        selectInput("var","Metric",c("open","high","low","close","volume","adjusted")),
        
        # allow users to input start and end date 
        dateInput('start',
                  label = 'Start date',
                  value = Sys.Date()-365
        ),
        dateInput('end',
                  label = 'End date',
                  value = Sys.Date()-1
        ),
        
        # allow users to choose to compare with index
        
        selectInput("company2ticker","Choose a ticker",sptickers)
        
      ),
      # allow display of plot in mainPanel
      
      mainPanel(
        tabsetPanel(
          tabPanel("Plot of Company 1 compared with S&P500 baseline", htmlOutput("view"),h6(paste("Summary of Company 1 on ",Sys.Date())),dataTableOutput('table4'),h6(paste("Summary of S&P 500 on",Sys.Date())),dataTableOutput('table3')),
          tabPanel("Exponential Smoothing for Company 1 close price from 2007-1-3 to today ", plotOutput("p1")),
          tabPanel("Plot of Company 1 compared with Company 2", htmlOutput("view2"),h6(paste("Summary of Company 1 on  ",Sys.Date())),dataTableOutput('table'),h6(paste("Summary of Company 2 on ",Sys.Date())),dataTableOutput('table2'))
        
      )
    ))),
  
  
  ## Shiny Server
  server = function(input, output,session) {
    dataInput0 <- reactive({ 
      
      c1=input$company1ticker
      sp500<-getSymbols("S&P500", src = "yahoo",
                        #Seperated the data into two seperate data sets and set auto.assign=FALSE
                        from = input$start,
                        to = input$end,
                        auto.assign = FALSE)
      company1 <- getSymbols(c1, src = "yahoo",
                             #Seperated the data into two seperate data sets and set auto.assign=FALSE
                             from = input$start,
                             to = input$end,
                             auto.assign = FALSE)
      
      c1.df <- data.frame(
        index(company1),
        coredata(company1),
        coredata(sp500),
        stringsAsFactors=FALSE
      )
      colnames(c1.df)=c("date","open","high","low","close","volume","adjusted","openc2","highc2","lowc2","closec2","volumec2","adjustedc2")
      
      
      
      return (c1.df)                          #Stored the data sets in a single list 
    })
    #Get close price of company 1 ticker from 2007-1-3 to today
    dataInputP <- reactive({ 
      
      c1=input$company1ticker
      company1 <- getSymbols(c1, src = "yahoo",
                             #Seperated the data into two seperate data sets and set auto.assign=FALSE
                             #from = input$start,
                             #to = Sys.Date(),
                             to = input$end,
                             auto.assign = FALSE)
      
      c1.df <- data.frame(
        index(company1),
        coredata(company1),
        stringsAsFactors=FALSE
      )
      colnames(c1.df)=c("date","open","high","low","close","volume","adjusted")
      return (c1.df)                          #Stored the data sets in a single list 
    })
    dataInput <- reactive({ 
      
      c1=input$company1ticker
      c2=input$company2ticker
      company2 <- getSymbols(c2, src = "yahoo",
                             #Seperated the data into two seperate data sets and set auto.assign=FALSE
                             from = input$start,
                             to = input$end,
                             auto.assign = FALSE)
      
      
      company1 <- getSymbols(c1, src = "yahoo",
                             #Seperated the data into two seperate data sets and set auto.assign=FALSE
                             from = input$start,
                             to = input$end,
                             auto.assign = FALSE)
      
      c1.df <- data.frame(
        index(company1),
        coredata(company1),
        coredata(company2),
        stringsAsFactors=FALSE
      )
      colnames(c1.df)=c("date","open","high","low","close","volume","adjusted","openc2","highc2","lowc2","closec2","volumec2","adjustedc2")
      
      
      
      return (c1.df)                          #Stored the data sets in a single list 
    })
    
    #Exponential smoothing of company1 close price from 2007-1-3 to user chosen end date
    output$p1<-renderPlot({
      plot(HoltWinters(ts(dataInputP()$close,start=c(2007,1,3) ,frequency=252), gamma=FALSE))
      
    })
    
    
    #Plot of company1 ticker and S&P 500 comparison
    output$view <- renderGvis({
      gvisLineChart(data=dataInput0(), 
                    xvar = "date",
                    yvar=c(input$var,paste0(input$var,"c2")),
                    options = list(
                      title = paste(input$company1ticker,"compared with S&P baseline S&P 500 (googleVis::gvisLineChart)")
                    )
      )
      
    })
    #Summary of S&P 500 of Sys.date
    output$table3 <- renderDataTable(
      data.frame(#title="Summary of S&P 500",
                name=read_html(paste0("https://www.google.com/finance?q=INDEXSP%3A.INX&ei=7gsmV4HJO5bnerirr5gN")) %>% html_nodes(".key") %>% html_text(trim=TRUE),
                 val=read_html(paste0("https://www.google.com/finance?q=INDEXSP%3A.INX&ei=7gsmV4HJO5bnerirr5gN")) %>% html_nodes(".val") %>% html_text(trim=TRUE),
                 stringsAsFactors=FALSE)
    )
    
    #Plot of company1 and company2 ticker comparison
    output$view2 <- renderGvis({
      gvisLineChart(
        data = dataInput(),
        xvar = "date",
        yvar=c(input$var,paste0(input$var,"c2")),
        options = list(
          title = paste(input$company1ticker,"compared with",input$company2ticker,"S&P 500 (googleVis::gvisLineChart)")
        ))
    })
    
    #Summary of company2 ticker of Sys.date
    output$table2 <- renderDataTable(
      if (input$company2ticker=="A"){
        data.frame(#title=paste("Summary of ",input$company2ticker),
          name=read_html("https://www.google.com/finance?q=NYSE%3AA&sq=A&sp=3&ei=mVAmV5mwF-j_esKziaAN") %>% html_nodes(".key") %>% html_text(trim=TRUE),
                   val=read_html("https://www.google.com/finance?q=NYSE%3AA&sq=A&sp=3&ei=mVAmV5mwF-j_esKziaAN") %>% html_nodes(".val") %>% html_text(trim=TRUE),
                   stringsAsFactors=FALSE)
      }
      else{
        data.frame(#title=paste("Summary of ",input$company2ticker),
          name=read_html(paste0("https://www.google.com/finance?q=",input$company2ticker,"&ei=u_UlV-jcN8KYmAH074N4")) %>% html_nodes(".key") %>% html_text(trim=TRUE),
                   val=read_html(paste0("https://www.google.com/finance?q=",input$company2ticker,"&ei=u_UlV-jcN8KYmAH074N4")) %>% html_nodes(".val") %>% html_text(trim=TRUE),
                   stringsAsFactors=FALSE)
      }
      
    )
    
    
    
    #Summary of company1 ticker of Sys.date 
    output$table <- renderDataTable(
      if (input$company1ticker=="A"){
        data.frame(#title=paste("Summary of ",input$company1ticker),
          name=read_html("https://www.google.com/finance?q=NYSE%3AA&sq=A&sp=3&ei=mVAmV5mwF-j_esKziaAN") %>% html_nodes(".key") %>% html_text(trim=TRUE),
                   val=read_html("https://www.google.com/finance?q=NYSE%3AA&sq=A&sp=3&ei=mVAmV5mwF-j_esKziaAN") %>% html_nodes(".val") %>% html_text(trim=TRUE),
                   stringsAsFactors=FALSE)
      }
      else{
        data.frame(#title=paste("Summary of ",input$company1ticker),
          name=read_html(paste0("https://www.google.com/finance?q=",input$company1ticker,"&ei=u_UlV-jcN8KYmAH074N4")) %>% html_nodes(".key") %>% html_text(trim=TRUE),
                   val=read_html(paste0("https://www.google.com/finance?q=",input$company1ticker,"&ei=u_UlV-jcN8KYmAH074N4")) %>% html_nodes(".val") %>% html_text(trim=TRUE),
                   stringsAsFactors=FALSE)
      }
    )
    
    #Summary of company1 ticker of Sys.date
    output$table4 <- renderDataTable(
      if (input$company1ticker=="A"){
        data.frame(#title=paste("Summary of ",input$company1ticker),
          name=read_html("https://www.google.com/finance?q=NYSE%3AA&sq=A&sp=3&ei=mVAmV5mwF-j_esKziaAN") %>% html_nodes(".key") %>% html_text(trim=TRUE),
          val=read_html("https://www.google.com/finance?q=NYSE%3AA&sq=A&sp=3&ei=mVAmV5mwF-j_esKziaAN") %>% html_nodes(".val") %>% html_text(trim=TRUE),
          stringsAsFactors=FALSE)
      }
      else{
        data.frame(#title=paste("Summary of ",input$company1ticker),
          name=read_html(paste0("https://www.google.com/finance?q=",input$company1ticker,"&ei=u_UlV-jcN8KYmAH074N4")) %>% html_nodes(".key") %>% html_text(trim=TRUE),
          val=read_html(paste0("https://www.google.com/finance?q=",input$company1ticker,"&ei=u_UlV-jcN8KYmAH074N4")) %>% html_nodes(".val") %>% html_text(trim=TRUE),
          stringsAsFactors=FALSE)
      }
    )
    
    
    
  })




