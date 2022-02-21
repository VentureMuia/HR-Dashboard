#Importing the packages
library(dplyr)
library(readr)
library(shinydashboard)
library(ggplot2)
library(shiny)
library(plotly)
library(shinyWidgets)
library(dashboardthemes)

#Loading the files
file_list = list.files(pattern="*.csv")%>%
  lapply(read.csv, stringsAsFactor=T)%>%
  bind_rows()

# set the theme
theme_set(theme_test())

# Select the unique categories
regions<-unique(file_list$BU.Region)
eth<-unique(file_list$EthnicGroup)
file_list$Date<-as.Date(file_list$Date,format="%d/%m/%Y")
file_list$Year<-substr(file_list$Date,1,4)
file_list$Months<-substr(file_list$Date,6,7)
years<-unique(file_list$Year)


# Creating the dashboard
UI<-dashboardPage(skin = "red",
  dashboardHeader(title = "HR Management Dashboard",titleWidth = 800),
  dashboardSidebar(
    sidebarMenu(id="sidebarid",
                menuItem("Dashboard",tabName = "dash",icon=icon("chart-line")),
                conditionalPanel(condition = 'input.sidebarid=="dash"',
                                 pickerInput("cont","Select contract type",choices = c("PT","FT"),selected = "FT",options = list(`actions-box` = TRUE),
                                             multiple = T),
                                 pickerInput("reg","Select Region",
                                             choices =c("North","West","Northwest","Midwest","Central","East","South"),
                                             selected = "East",options = list(`actions-box` = TRUE),
                                             multiple = T),
                                 pickerInput("eth","Select Ethnic group",
                                             choices =c("Group A","Group E","Group F","Group D","Group B","Group G","Group C"),
                                             selected = "Group A",
                                             options = list(`actions-box` = TRUE),
                                             multiple = T),
                                 pickerInput("yr","Choose year",choices = years,selected = 2018,options = list(`actions-box` = TRUE),
                                             multiple = T)
                                 
                                 
                                 )
                
      
    )
  ),
  dashboardBody(
    shinyDashboardThemes(theme = "grey_dark"),
    tabItems(tabItem(tabName = "dash",
                     box(width=12,
                         valueBoxOutput("val1"),
                         valueBoxOutput("val2"),
                         valueBoxOutput("val3")
                         ),
                     
                     tabBox(width=6,
                     tabPanel("Months",plotlyOutput("bar")),
                     tabPanel("Region",plotlyOutput("bar2")),
                     tabPanel("EthnicGroup",plotlyOutput("col")),
                     tabPanel("Tenure",plotlyOutput("col2")),
                           ),
                     tabBox(width = 6,side = "right",
                            tabPanel("Termination",plotlyOutput("bar3")),
                            tabPanel("Age",plotlyOutput("pie")),
                            tabPanel("PayType",plotlyOutput("bar5")),
                            tabPanel("FP",plotlyOutput("bar6"))
                           )
                     )
             )
    
  )
)

server<-function(input,output,session){
  
  # Reactive data
  data<-reactive({df<-file_list})
  
  # count the employees
  output$bar<-renderPlotly({
    s<-data()%>%
      filter(EthnicGroup==input$eth &
            Year==input$yr&
             FP==input$cont &
             BU.Region==input$reg  
               )%>%
      group_by(Year,Months,Gender)%>%
      summarise(Number_of_Employees=n())%>%
      ggplot(aes(x=Months,y=Number_of_Employees,fill=Gender))+
      geom_col(position = "dodge")+
      labs(title=" Employees by Months",y="Number of Employees")+
      theme(plot.title = element_text(hjust=0.5,face="bold"),
            legend.position = "none")
    ggplotly(s) %>% config(displayModeBar = F)
  })
  
  # Employees by EthnicGroup
  output$col<-renderPlotly({
    s<-data()%>%
      filter( Year==input$yr&
               FP==input$cont &
               BU.Region==input$reg  
      )%>%
      group_by(EthnicGroup,Gender)%>%
      summarise(count_employees=n())%>%
      ggplot(aes(x=EthnicGroup,y=count_employees,fill=Gender))+
      geom_col(position="dodge")+
      labs(title="Employees by EthnicGroup",y="Count of Employees")+
      theme(plot.title = element_text(hjust=0.5,face="bold"),
            legend.position = "none")
    ggplotly(s) %>% config(displayModeBar = F)
  })
  
  # Employees by Tenure
  output$col2<-renderPlotly({
    s<-data()%>%
      filter( Year==input$yr&
               FP==input$cont
                )%>%
      
      ggplot(aes(x=TenureMonths,fill=Gender))+
      geom_histogram(bins = 20)+
      labs(title="Employees by Tenure",x="TenureMonths",y="Number of Employees")+
      theme(plot.title = element_text(hjust=0.5,face="bold"),
            legend.position = "none")
    ggplotly(s) %>% config(displayModeBar = F)
  })
  
  # Count Employees by Region
  output$bar2<-renderPlotly({
    s<-data()%>%
      filter( EthnicGroup==input$eth &
               Year==input$yr  
      )%>%
      group_by(BU.Region,Gender)%>%
      summarise(Number_of_Employees=n())%>%
      ggplot(aes(x=BU.Region,y=Number_of_Employees,fill=Gender))+
      geom_col(position = "dodge")+
      coord_flip()+
      labs(title="Employees by Region",y="Number of Employees",x="Region")+
      theme(plot.title = element_text(hjust=0.5,face="bold"),
            legend.position = "none")
    ggplotly(s) %>% config(displayModeBar = F)
  })
  
  # Termination Reason
  output$bar3<-renderPlotly({
    s<-data()%>%
      filter( FP==input$cont &
               Year==input$yr
             )%>%
      group_by(TermReason,Gender)%>%
      summarise(Number_of_Employees=n())%>%
      ggplot(aes(x=TermReason,y=Number_of_Employees,fill=Gender))+
      geom_col(position = "dodge")+
      labs(title="Employees by Termination",y="Number of Employees")+
      theme(plot.title = element_text(hjust=0.5,face="bold"),
            legend.position = "none")
    ggplotly(s) %>% config(displayModeBar = F)
  })
  # Employees by Age
  output$pie<-renderPlotly({
    s<-data()%>%
      filter(       Year==input$yr&
                    FP==input$cont
      )%>%
      group_by(Age,Gender)%>%
      summarise(Number_of_Employees=n())%>%
      ggplot(aes(x=Age,y=Number_of_Employees,col=Gender))+
      geom_line()+
      geom_point()+
      labs(title="Employees by Age",y="Number of Employees")+
      theme(plot.title = element_text(hjust=0.5,face="bold"),
            legend.position = "none")
    ggplotly(s) %>% config(displayModeBar = F)
  })
  
  
  # Employees by payType
  output$bar5<-renderPlotly({
    s<-data()%>%
      filter(     EthnicGroup==input$eth &
                  BU.Region==input$reg &
                  FP==input$cont
      )%>%
      group_by(PayType,Gender)%>%
      summarise(Number_of_Employees=n())%>%
      ggplot(aes(x=PayType,y=Number_of_Employees,fill=Gender))+
      geom_col()+
      geom_text(aes(label=Number_of_Employees))+
      labs(title="Employees by PayType",y="Number of Employees")+
      theme(plot.title = element_text(hjust=0.5,face="bold"),
            legend.position = "none",
            axis.text.y  =element_blank(),
            axis.ticks.y =element_blank() )
    ggplotly(s) %>% config(displayModeBar = F)
  })
  
  # Employees by FP
  output$bar6<-renderPlotly({
    s<-data()%>%
      filter(     EthnicGroup==input$eth &
                    BU.Region==input$reg 
      )%>%
      group_by(FP,Gender)%>%
      summarise(Number_of_Employees=n())%>%
      ggplot(aes(x=FP,y=Number_of_Employees,fill=Gender))+
      geom_col()+
      labs(title="Employees by FP",y="Number of Employees")+
      geom_text(aes(label=Number_of_Employees))+
      theme(plot.title = element_text(hjust=0.5,face="bold"),
            axis.text.y  =element_blank(),
            axis.ticks.y =element_blank(),
            legend.position = "none")
    ggplotly(s) %>% config(displayModeBar = F)
  })
  
  # Filter
  fil<-reactive({
    data()%>%
    filter(EthnicGroup==input$eth &
             Year==input$yr&
             FP==input$cont &
             BU.Region==input$reg  
    )
  })
  
  output$val1<-renderValueBox({
    valueBox(value=count(fil()),
             subtitle ="TOTAL EMPLOYEES" ,
             icon=icon("users"),
             color="green",
             width = 4
             )
  })
  
  output$val2<-renderValueBox({
    valueBox(value=table(fil()$Gender)[1],
             subtitle =paste0(round(prop.table(table(fil()$Gender))[1]*100,2),"%"),
             icon=icon("female"),
             color="red",
             width = 4
    )
  })
  
  output$val3<-renderValueBox({
    valueBox(value=table(fil()$Gender)[2],
             subtitle =paste0(round(prop.table(table(fil()$Gender))[2]*100,2),"%"),
             icon=icon("male"),
             color="blue",
             width = 4
    )
  })
}

shinyApp(UI,server)