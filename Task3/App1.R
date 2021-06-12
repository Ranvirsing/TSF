#installing some required libraries
library(shinydashboard)
library(shiny)
library(tidyverse)
library(ggplot2)
#importing dataset from the local folder
setwd("C:/Users/Admin/Desktop/TSF/Task_3")
df=read.csv("C:/Users/Admin/Desktop/TSF/Task_3/SampleSuperstore.csv")
ui <- dashboardPage(
  
  #Dashboard title
  dashboardHeader(title = 'SampleSuperstore',
                  
                  titleWidth = 350),
  #Sidebar layout
  dashboardSidebar(width = 350,
                   sidebarMenu(menuItem("Statistics", tabName = "stat", 
                                        icon = icon('calculator')),
                               menuItem("Dashboard", tabName = "dash", 
                                        icon = icon('tachometer-alt')),
                               menuItem("Plots", tabName = "plots", 
                                        icon = icon('poll'))
                               )
                    ),
  #Tabs layout
  dashboardBody(tags$head(tags$style(HTML(
    '.myClass { 
        font-size: 20px;
        line-height: 50px;
        text-align: left;
        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        padding: 0 15px;
        overflow: hidden;
        color: white;
      }
    '))),
    tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass">Ranvirsing Sisodiya </span>\');
      })
                     ')),
                tabItems(
                  tabItem('stat',
                          # Dynamic valueBoxes
                          valueBoxOutput("progressBox"),
                          valueBoxOutput("approvalBox1"),
                          valueBoxOutput("approvalBox"),
                          valueBoxOutput("approvalBox2"),
                          valueBoxOutput("approvalBox3"),
                          valueBoxOutput("approvalBox4"),
                          box(plotOutput("reg"),width = 12)
                          
                          ),
                          
                  
                  #Plots tab content
                
                    tabItem('plots', 
                          #Histogram filter
                          box(status = 'primary', 
                              title = 'Categoricaal Variable Analysis',
                              width = 12,
                              selectInput('num', "Categorical variables:",
                                          width=250,
                                          c('Category','Sub.Category',
                                            'Ship.Mode','Segment')),
                              footer = 'Histogram plot for Categorical variables'),
                          #Frecuency plot filter
                          #Boxes to display the plots
                          box(plotOutput('freqPlot'),width = 12)),
                  #Dashboard tab content
                  tabItem('dash',
                          #Dashboard filters
                          box(title = 'City', status = 'primary', 
                              selectInput('regis','Select the City',
                                          choices = df$City,width = 250)
                              ),
                          box(title = 'Profit', status = 'primary', 
                              selectInput('regis1','Select the Amount of Loss/Profit',
                                          choices = df$Profit, width = 250)
                          ),
                          #Boxes to display the plots
                          box(valueBoxOutput('q1'),
                              valueBoxOutput('q2'),
                              valueBoxOutput('q3'),
                              width = 12
                              ),
                          box(tableOutput('more'),width = 12)
                          )))
)

server <- shinyServer(function(input, output) {
  
  
  output$reg <- renderPlot({
    ggplot(data = df, aes(x = df[['Region']]))+
      geom_bar(stat = 'count',fill='cadetblue',width = 0.3)+
      stat_count(geom = 'text', size = 4,
                 aes(label = ..count..),
                 position = position_stack(vjust = 1.03))+
      labs(title = 'Region',
           x = 'Region', y = 'Count')
  })
  output$progressBox <- renderValueBox({
    valueBox(
      paste0( round(sum(df[['Profit']]),2)), "Profit", icon = icon("dollar-sign"),
      color = "fuchsia"
    )
  })
  output$approvalBox1 <- renderValueBox({
    valueBox(
      round(sum(df[['Sales']]),2), "Sales", icon = icon("dollar-sign"),
      color = "green"
    )
  })
  output$approvalBox <- renderValueBox({
    valueBox(
      sum(df[['Quantity']]), "Quantity", icon = icon("coins"),
      color = "aqua"
    )
  })
  output$approvalBox2 <- renderValueBox({
    valueBox(
      round(mean(df[['Profit']]),2), "Average Profit", icon = icon("dollar-sign"),
      color = "blue"
    )
  })
  output$approvalBox3 <- renderValueBox({
    valueBox(
      round(mean(df[['Sales']]),2), "Average Sales", icon = icon("dollar-sign"),
      color = "red"
    )
  })
  output$approvalBox4 <- renderValueBox({
    valueBox(
      round(mean(df[['Quantity']]),0), "Average Quantity Sold", icon = icon("coins"),
      color = "maroon"
    )
  })
  output$q4 <- renderText({
    for(i in input$regis1){
      df4=filter(df,df$Profit==i)
    }
  })
  output$q5 <- renderText({
    rs=as.numeric(input$regis1)
    for(i in rs){
      df4<-filter(df,df$Profit==i)
    }
  })
  output$q6 <- renderText({
    
    
      df4<-filter(df,df[['Profit']]==input$regis1)
      paste0("Sub Category is : ",df[["Sub.Category"]])
    
  })

  output$more <- renderTable({
  prop=as.numeric(input$regis1)
  df5<- filter(df,df$Profit==prop)
  
  })
  output$freqPlot <- renderPlot({
    #Column name variable
    num_val = ifelse(input$num == 'Segment', 'Segment',
                     ifelse(input$num =='Ship.Mode', 'Ship.Mode',
                            ifelse(input$num == 'Category', 'Category',
                                   ifelse(input$num == 'Sub.Category', 'Sub.Category'))))
    
    #Frecuency plot
    ggplot(data = df, aes(x = df[[num_val]]))+
      geom_bar(stat = 'count', fill = 'turquoise3', 
               width = 0.5)+
      stat_count(geom = 'text', size = 4,
                 aes(label = ..count..),
                 position = position_stack(vjust = 1.03))+
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14),
            plot.title = element_text(size = 16, face="bold"))+
      labs(title = sprintf('Frecuency plot of the variable %s', num_val),
           x = sprintf('%s', input$cat), y = 'Count')
    
  
  })
  output$q1 <- renderValueBox({
    for(i in input$regis){
     
        df2=filter(df,df$City==i)
      
    }
    valueBox(
      round(sum(df2[['Profit']]),2), "Profit", icon = icon("dollar-sign"),
      color = "red"
    )
    
  })
  output$q2 <- renderValueBox({
    for(i in input$regis){
      
      df2=filter(df,df$City==i)
      
    }
    valueBox(
      round(sum(df2[['Sales']]),2), "Sales", icon = icon("dollar-sign"),
      color = "teal"
    )
    
  })
  output$q3 <- renderValueBox({
    for(i in input$regis){
      
      df2=filter(df,df$City==i)
      
    }
    valueBox(
      sum(df2[['Quantity']]), "Quantity", icon = icon("coins"),
      color = "blue"
    )
    
  })

})
shinyApp(ui=ui, server=server)
