library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(dashboardthemes)
library(plotly)
library(shinyalert)
require(dplyr)
require(ggplot2)
require(tibbletime)
require(tidyverse)
require(glmnet)
require(caret)
library(tidyr)
library(vioplot)
library(rsconnect)
library(DALEX)
library(DT)
library(randomForest)


svm<-readRDS('svm1.rds')
svm2<-readRDS('svm2.rds')
svm3<-readRDS('svm3.rds')


df=read.csv("airquality.csv",stringsAsFactors = TRUE)
df1=read.csv("aq.3.csv",stringsAsFactors = TRUE)
df2=read.csv("aq.8.csv",stringsAsFactors = TRUE)
df3=read.csv("aq.24.csv",stringsAsFactors = TRUE)

count_aq.3=table(df1$AQ.3)
count_aq.8=table(df2$AQ.8)
count_aq.24=table(df3$AQ.24)
piepercent1 = paste(round(100*count_aq.3/sum(count_aq.3)), "%")
piepercent2 = paste(round(100*count_aq.8/sum(count_aq.8)), "%")
piepercent3 = paste(round(100*count_aq.24/sum(count_aq.24)), "%")
names=c("Good","Satisfactory","Moderate","Poor","Very Poor","Severe")
cols = c("aliceblue","bisque","cadetblue1","chartreuse","chocolate1","cornsilk1")


ui <- shinyUI(dashboardPage( skin = 'blue',
                             
                             dashboardHeader( title = "Intraday Air Quality Prediction System", titleWidth = 800),
                             
                             dashboardSidebar(
                               sidebarMenu(
                                 id = "sidebar",
                                 
                                 #first menu item
                                 menuItem(text = "My Dashboard", tabName="A_Dashboard", icon = icon("user")),
                                 
                                 #second menu item
                                 menuItem(text = "Visualization", tabName="V_chart", icon = icon("chart-line")),
                                 
                                 #third menu item
                                 menuItem(text = "Prediction", tabName="P", icon = icon("equals")),
                                 
                                 #about data item
                                 menuItem(text = "About Us", tabName="about", icon = icon("table"))
                                 
                                 
                               )
                               
                             ),
                             dashboardBody(
                               shinyDashboardThemes(
                                 theme = "purple_gradient"
                               ),
                               
                               tabItems(
                                 #The about dashboard tab
                                 tabItem('A_Dashboard',
                                         fluidRow(
                                           mainPanel(align = 'left',
                                                     img(src = "aq11.jpg", width = 1000, height = 500, align = 'Center'),
                                                     br(),
                                                     br()
                                           ),
                                           tabBox(
                                             
                                             # The id lets us use input$tabset1 on the server to find the current tab
                                             id = "tabset1", height = "450px",
                                             tabPanel("My Dashboard", 
                                                      h1("My Dashoard"),
                                                      p("High population expansion has driven rapid industrialization, urbanization, and an increase in motor traffic, resulting in worsening air quality throughout the decades. \n
                                                         In recent years, India has ranked fifth among the world’s most polluted countries, with a pollution index of 58.1. \n
              This project will help Indian residents to better plan their daytime travel activities and reduce the impact of air pollution on the health of Indian residents. ", style =  "font-size:15px;"),
                                                      br(),
                                                     
                                             ),
                                             
                                             
                                             tabPanel("Visualization", 
                                                      h1("Visualization"),
                                                      p("This section is descriptive statistics on air quality in India, in this section, you can view: \n", style =  "font-size:15px;"),
                                                      p("- The distribution of features", style =  "font-size:15px;"),
                                                      p("- Boxplot", style =  "font-size:15px;"),
                                                      p("- Scatterplot", style =  "font-size:15px;"),
                                                      p("- Scatterplot", style =  "font-size:15px;"),
                                                      p("- Vioplot", style =  "font-size:15px;"),
                                                      p("- Hotmap", style =  "font-size:15px;")
                                                      
                                             ),
                                             
                                             
                                             tabPanel("Prediction", 
                                                      h1("Prediction"),
                                                      p("This part is predictive statistics on air quality in India, in this part you can predict:",style =  "font-size:15px;"),
                                                      p("- Air Quality after 3 hours",style =  "font-size:15px;"),
                                                      p("- Air Quality after 8 hours",style =  "font-size:15px;"),
                                                      p("- Air Quality after 24 hours",style =  "font-size:15px;")),
                                             
                                             tabPanel("About Us", 
                                                      h1("About Us"),
                                                      p("Introduction of the team.",style =  "font-size:15px;"))
                                             
                                           )
                                         )
                                 ),
                                 
                                 
                                 #the Overview tab
                                 tabItem('V_chart',
                                         fluidRow(
                                           tabBox(
                                             title="The distribution of features",
                                             id = "tabset1", height = "300px",
                                             tabPanel("PM2.5", plotOutput("plot1",height = "300px")),
                                             tabPanel("PM10", plotOutput("plot2",height = "300px")),
                                             tabPanel("NO", plotOutput("plot3",height = "300px")),
                                             tabPanel("NO2", plotOutput("plot4",height = "300px")),
                                             tabPanel("NOx", plotOutput("plot5",height = "300px")),
                                             tabPanel("NH3", plotOutput("plot6",height = "300px")),
                                             tabPanel("CO", plotOutput("plot7",height = "300px")),
                                             tabPanel("SO2", plotOutput("plot8",height = "300px")),
                                             tabPanel("O3", plotOutput("plot9",height = "300px")),
                                             tabPanel("Benzene", plotOutput("plot10",height = "300px")),
                                             tabPanel("Toluene", plotOutput("plot11",height = "300px")),
                                           ),
                                           
                                           tabBox(
                                             title="Boxplot",
                                             id = "tabset3", height = "300px",
                                             tabPanel("PM2.5", plotOutput("plot16",height = "300px")),
                                             tabPanel("PM10", plotOutput("plot17",height = "300px")),
                                             tabPanel("NO", plotOutput("plot18",height = "300px")),
                                             tabPanel("NO2", plotOutput("plot19",height = "300px")),
                                             tabPanel("NOx", plotOutput("plot20",height = "300px")),
                                             tabPanel("NH3", plotOutput("plot21",height = "300px")),
                                             tabPanel("CO", plotOutput("plot22",height = "300px")),
                                             tabPanel("SO2", plotOutput("plot23",height = "300px")),
                                             tabPanel("O3", plotOutput("plot24",height = "300px")),
                                             tabPanel("Benzene", plotOutput("plot25",height = "300px")),
                                             tabPanel("Toluene", plotOutput("plot26",height = "300px")),
                                           ),
                                           
                                           tabBox(
                                             title="Scatterplot",
                                             id = "tabset3", height = "300px",
                                             tabPanel("Tab1", plotOutput("plot28",height = "300px")),
                                             tabPanel("Tab2", plotOutput("plot29",height = "300px")),
                                             tabPanel("Tab3", plotOutput("plot30",height = "300px")),
                                             tabPanel("Tab4", plotOutput("plot31",height = "300px")),
                                             tabPanel("Tab5", plotOutput("plot32",height = "300px")),
                                             tabPanel("Tab6", plotOutput("plot33",height = "300px")),
                                           ),
                                           
                                           box(
                                             title = "Vioplot"
                                             ,status = "primary"
                                             ,solidHeader = FALSE 
                                             ,collapsible = TRUE 
                                             ,plotOutput("plot12", height = "300px")
                                           ),
                                           
                                           box(
                                             title = "Hotmap"
                                             ,status = "primary"
                                             ,solidHeader = FALSE 
                                             ,collapsible = TRUE 
                                             ,plotOutput("plot27", height = "300px")
                                           ),
                                           
                                           tabBox(
                                             title = "AQ",
                                             # The id lets us use input$tabset1 on the server to find the current tab
                                             id = "tabset2", height = "300px",
                                             tabPanel("AQ.3", plotOutput("plot13",height = "300px")),
                                             tabPanel("AQ.8", plotOutput("plot14",height = "300px")),
                                             tabPanel("AQ.24", plotOutput("plot15",height = "300px")),
                                             
                                           )
                                         )
                                 ),
                                 
                                 #the about us tab
                                 tabItem('about',
                                         mainPanel(align = 'Center',
                                                   # h1('Data', style = "font-size:80px;" ),
                                                   img(src='fsktm.png', width = 500, height = 300, align = 'Center'),
                                                   br(),
                                                   br(),
                                                   p("Hi!Dear customer:\n", style = "font-size:29px;"),
                                                   br(),
                                                   p("Welcome to our shiny app！！！ Here you will be able to learn about India's air quality and make predictions to decide whether to travel! Hope this app can help you!",style = "font-size:24px;"),
                                                   br(),
                                                   p("***********Our team members*********** \n", style = "font-size:20px;"),
                                                   br(),
                                                   p("--Junhong Su (Leader)"),
                                                   p("--Jiale Xiong"),
                                                   p("--Shuangdan NI"),
                                                   p("--Jingke Tan"),
                                                   p("--Puivee"),
                                                   br(),
                                                   tags$a(href="https://www.kaggle.com/datasets/rohanrao/air-quality-data-in-india?select=city_hour.csv", "Data Source"),
                                                   br(),
                                                   tags$a(href="https://github.com/JunhongSu/WQD7001", "GitHub"),
                                                   br(),
                                                   
                                                   
                                         )),
                                 
                                 
                                 #Prediction
                                 tabItem('P',
                                         titlePanel("Prediction"),
                                         sidebarLayout(
                                           sidebarPanel(
                                             h1(" Input the current air particle information, and then you can know the air quality after 3 hours!",style = "font-size:20px"),
                                             br(),
                                             numericInput("PM2.5","PM2.5",value=0.01,min=0,max=500,step = 0.01),
                                             numericInput("PM10","PM10",value=0.01,min=0,max=500,step = 0.01),
                                             numericInput("NO","NO",value=0.01,min=0,max=500,step = 0.01),
                                             numericInput("NO2","NO2",value=0.01,min=0,max=500,step = 0.01),
                                             numericInput("NOx","NOx",value=0.01,min=0,max=500,step = 0.01),
                                             numericInput("NH3","NH3",value=0.01,min=0,max=500,step = 0.01),
                                             numericInput("CO","CO",value=0.01,min=0,max=500,step = 0.01),
                                             numericInput("SO2","SO2",value=0.01,min=0,max=500,step = 0.01),
                                             numericInput("Toluene","Toluene",value=0.01,min=0,max=500,step = 0.01),
                                             actionButton("update","Start Predict Air Quality"),
                                             
                                             hr(),
                                           ),
                                           mainPanel(
                                             h1(" Input the current air particle information, and then you can know the air quality after 8 hours!",style = "font-size:20px"),
                                             br(),
                                             numericInput("PM2.51","PM2.5",value=0.01,min=0,max=500,step = 0.01),
                                             numericInput("PM101","PM10",value=0.01,min=0,max=500,step = 0.01),
                                             numericInput("NO1","NO",value=0.01,min=0,max=500,step = 0.01),
                                             numericInput("NO21","NO2",value=0.01,min=0,max=500,step = 0.01),
                                             numericInput("NOx1","NOx",value=0.01,min=0,max=500,step = 0.01),
                                             numericInput("NH31","NH3",value=0.01,min=0,max=500,step = 0.01),
                                             numericInput("CO1","CO",value=0.01,min=0,max=500,step = 0.01),
                                             numericInput("SO21","SO2",value=0.01,min=0,max=500,step = 0.01),
                                             numericInput("Toluene1","Toluene",value=0.01,min=0,max=500,step = 0.01),
                                             actionButton("submit","Start Predict Air Quality"),
                                             
                                             
                                             hr(),
                                             h1(" Input the current air particle information, and then you can know the air quality after 24 hours!",style = "font-size:20px"),
                                             numericInput("PM2.511","PM2.5",value=0.01,min=0,max=500,step = 0.01),
                                             numericInput("PM1011","PM10",value=0.01,min=0,max=500,step = 0.01),
                                             numericInput("NO11","NO",value=0.01,min=0,max=500,step = 0.01),
                                             numericInput("NO211","NO2",value=0.01,min=0,max=500,step = 0.01),
                                             numericInput("NOx11","NOx",value=0.01,min=0,max=500,step = 0.01),
                                             numericInput("NH311","NH3",value=0.01,min=0,max=500,step = 0.01),
                                             numericInput("CO11","CO",value=0.01,min=0,max=500,step = 0.01),
                                             numericInput("SO211","SO2",value=0.01,min=0,max=500,step = 0.01),
                                             numericInput("Toluene11","Toluene",value=0.01,min=0,max=500,step = 0.01),
                                             actionButton("submit1","Start Predict Air Quality"),
                                             
                                             
                                           )
                                           
                                         )
                                         
                                 )
                               )
                             )
))


server <- function(input,output,session){
  #overview page
  #Before vs After "Comparison of Education Performance Before and During Pandemic"
  output$plot1 <- renderPlot({
    hist(df$PM2.5,breaks = 200,angle=45,col = 'red',border='blue',main = 'PM2.5',xlab = '',ylim = c(0,500))
  })
  output$plot2 <- renderPlot({
    hist(df$PM10,breaks = 200,angle=45,col = 'red',border='blue',main = 'PM10',xlab = '',ylim = c(0,300))
  })
  output$plot3 <- renderPlot({
    hist(df$NO,breaks = 200,angle=45,col = 'red',border='blue',main = 'NO',xlab = '',ylim = c(0,800))
  })
  
  output$plot4 <- renderPlot({
    hist(df$NO2,breaks = 200,angle=45,col = 'red',border='blue',main = 'NO2',xlab = '',ylim = c(0,200))
  })
  output$plot5 <- renderPlot({
    hist(df$NOx,breaks = 200,angle=45,col = 'red',border='blue',main = 'NOX',xlab = '',ylim = c(0,400))
  })
  output$plot6 <- renderPlot({
    hist(df$NH3,breaks = 200,angle=45,col = 'red',border='blue',main = 'NH3',xlab = '',ylim = c(0,600))
  })
  output$plot7 <- renderPlot({
    hist(df$CO,breaks = 200,angle=45,col = 'red',border='blue',main = 'NH3',xlab = '',ylim = c(0,600))
  })
  output$plot8 <- renderPlot({
    hist(df$SO2,breaks = 200,angle=45,col = 'red',border='blue',main = 'CO',xlab = '',ylim = c(0,500))
  })
  output$plot9 <- renderPlot({
    hist(df$O3,breaks = 200,angle=45,col = 'red',border='blue',main = 'O3',xlab = '',ylim = c(0,300))
  })
  output$plot10 <- renderPlot({
    hist(df$Benzene,breaks = 200,angle=45,col = 'red',border='blue',main = 'Benzene',xlab = '',ylim = c(0,700))
  })
  output$plot11 <- renderPlot({
    hist(df$Toluene,breaks = 200,angle=45,col = 'red',border='blue',main = 'Toluene',xlab = '',ylim = c(0,700))
  })
  output$plot12 <- renderPlot({
    vioplot(df,border='black',pchMed=16,col='antiquewhite')
  })
  
  output$plot13 <- renderPlot({
    pie(count_aq.3, labels=piepercent1,main="AQ.3",col = cols)
    legend("topleft", names,cex=0.7,fill=cols)
    theme_dark()
  })
  
  
  output$plot14 <- renderPlot({
    pie(count_aq.8, labels=piepercent2,main="AQ.8",col = cols)
    legend("topleft", names,cex=0.7,fill=cols)
    theme_dark()
  }) 
  
  
  output$plot15 <- renderPlot({
    pie(count_aq.24, labels=piepercent3,main="AQ.24",col = cols)
    legend("topleft", names,cex=0.7,fill=cols)
    theme_dark()
  })
  
  output$plot16 <- renderPlot({
    boxplot(df$PM2.5,main="PM2.5",ylab="value")
    theme_dark()
  })
  
  output$plot17 <- renderPlot({
    boxplot(df$PM10,main="PM10",ylab="value")
    theme_dark()
  })
  
  output$plot18 <- renderPlot({
    boxplot(df$NO,main="NO",ylab="value")
    theme_dark()
  })
  
  output$plot19 <- renderPlot({
    boxplot(df$NO2,main="NO2",ylab="value")
    theme_dark()
  })
  
  output$plot20 <- renderPlot({
    boxplot(df$NOx,main="NOx",ylab="value")
    theme_dark()
  })
  
  output$plot21 <- renderPlot({
    boxplot(df$NH3,main="NH3",ylab="value")
    theme_dark()
  })
  
  output$plot22 <- renderPlot({
    boxplot(df$CO,main="CO",ylab="value")
    theme_dark()
  })
  
  output$plot23 <- renderPlot({
    boxplot(df$SO2,main="SO2",ylab="value")
    theme_dark()
  })
  
  output$plot24 <- renderPlot({
    boxplot(df$O3,main="O3",ylab="value")
    theme_dark()
  })
  
  output$plot25 <- renderPlot({
    boxplot(df$Benzene,main="Benzene",ylab="value")
    theme_dark()
  })
  
  output$plot26 <- renderPlot({
    boxplot(df$Toluene,main="Toluene",ylab="value")
    theme_dark()
  })
  
  output$plot27 <- renderPlot({
    corr1<-cor(df,method = "pearson")
    heatmap(corr1)
    theme_dark()
  })
  
  output$plot28 <- renderPlot({
    plot(df$Benzene,df$Toluene,xlab='Benzene',ylab='Toluene',type='p',main='Scatterplot of Toluene and Benzene')
    theme_dark()
  })
  
  output$plot29 <- renderPlot({
    plot(df$PM2.5,df$PM10,xlab='PM2.5',ylab='PM10',type='p',main='Scatterplot of PM2.5 and PM10')
    theme_dark()
  })
  
  output$plot30 <- renderPlot({
    plot(df$NOx,df$NH3,xlab='NOx',ylab='NH3',type='p',main='Scatterplot of NOx and NH3')
    theme_dark()
  })
  
  output$plot31 <- renderPlot({
    plot(df$NO2,df$NOx,xlab='NO2',ylab='NOx',type='p',main='Scatterplot of NO2 and NOx')
    theme_dark()
  })
  
  output$plot32 <- renderPlot({
    plot(df$NO,df$NOx,xlab='NO',ylab='NOx',type='p',main='Scatterplot of NO and NOx')
    theme_dark()
  })
  
  output$plot33 <- renderPlot({
    plot(df$NO,df$CO,xlab='NO',ylab='CO',type='p',main='Scatterplot of NO and CO')
    theme_dark()
  })
  
  
  createPersonalDataFrame <- function(input) {
    return(
      data.frame(
        PM2.5=input$PM2.5[1],
        PM10=input$PM10[1],
        NO=input$NO[1],
        NO2=input$NO2[1],
        NOx=input$NOx[1],
        NH3=input$NH3[1],
        CO=input$CO[1],
        SO2=input$SO2[1],
        Toluene=input$Toluene[1]
      )
    )
  }
  
  createPersonalDataFrame1 <- function(input) {
    return(
      data.frame(
        PM2.5=input$PM2.51[1],
        PM10=input$PM101[1],
        NO=input$NO1[1],
        NO2=input$NO21[1],
        NOx=input$NOx1[1],
        NH3=input$NH31[1],
        CO=input$CO1[1],
        SO2=input$SO21[1],
        Toluene=input$Toluene1[1]
      )
    )
  }
  
  createPersonalDataFrame2 <- function(input) {
    return(
      data.frame(
        PM2.5=input$PM2.511[1],
        PM10=input$PM1011[1],
        NO=input$NO11[1],
        NO2=input$NO211[1],
        NOx=input$NOx11[1],
        NH3=input$NH311[1],
        CO=input$CO11[1],
        SO2=input$SO211[1],
        Toluene=input$Toluene11[1]
      )
    )
  }
  
  observeEvent(input$update, {
    df01 = createPersonalDataFrame(input)
    airquality1 = predict(svm, newdata = df01)
    airquality1 = as.numeric(airquality1)
    
    
    ifelse(airquality1[1]<=3,shinyalert(title='Prediction Result',type="success",text=print(paste('The air quality is',airquality1[1],', you can travel!'))),shinyalert(title='Prediction Result',type="success",text=print(paste('The air quality is',airquality1,",don't travel!"))))
  })
  
  observeEvent(input$submit, {
    df02 = createPersonalDataFrame1(input)
    airquality2 = predict(svm2, newdata = df02)
    airquality2 = as.numeric(airquality2)
    
    
    ifelse(airquality2[1]<=3,shinyalert(title='Prediction Result',type="success",text=print(paste('The air quality is',airquality2[1],', you can travel!'))),shinyalert(title='Prediction Result',type="success",text=print(paste('The air quality is',airquality2,",don't travel!"))))
  })
  
  observeEvent(input$submit1, {
    df03 = createPersonalDataFrame2(input)
    airquality3 = predict(svm3, newdata = df03)
    airquality3 = as.numeric(airquality3)
    
    
    ifelse(airquality3[1]<=3,shinyalert(title='Prediction Result',type="success",text=print(paste('The air quality is',airquality3[1],', you can travel!'))),shinyalert(title='Prediction Result',type="success",text=print(paste('The air quality is',airquality3,",don't travel!"))))
  })
  
}


shinyApp(ui = ui, server = server)


