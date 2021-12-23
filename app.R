#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(snakecase)
library(viridis)
library(forcats)
data <- read.csv("data.csv")

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "black",
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dash_tab", icon = icon("home")),
      menuItem("Data Table", tabName = "dt_tab", icon = icon("table")),
      menuItem("Graph Plot", tabName = "plot_tab", icon = icon("chart-bar")),
      menuItem("Analysis", tabName = "ana_tab", icon = icon("book-open"))
      
      
    )
  ),
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dt_tab",
              h3("Data Table"),
              fluidRow(
              box(
                width=3,
              selectInput("num_var_0", "Year", choices =  c("All",sort(unique(as.character(data$year))))),
              selectInput("num_var_1", "Degree Of Injury", choices =  c("All",sort(unique(as.character(data$degree_of_injury))))),
              selectInput("num_var_2", "Industry", choices =  c("All",sort(unique(as.character(data$industry))))),
              selectInput("num_var_3", "Sub-Industry", choices =  c("All",sort(unique(as.character(data$sub_industry))))),
              selectInput("num_var_4", "Incident Type", choices =  c("All",sort(unique(as.character(data$incident_type))))),
              selectInput("num_var_5", "Incident Agent", choices =  c("All",sort(unique(as.character(data$incident_agent))))),
              selectInput("num_var_6", "Incident Agent Sub Type", choices =  c("All",sort(unique(as.character(data$incident_agent_sub_type))))),
              ),
              box(
                width=9,
                DT::dataTableOutput('contents'))
              )
              
              
              
      ),
      
      # Second tab content
      tabItem(tabName = "plot_tab",
              h3("Graph"),
              fluidRow(
                box(
                  width=3,
                  selectInput("num_var_11", "Year", choices =  c("All",sort(unique(as.character(data$year))))),
                  selectInput("num_var_12", "Degree Of Injury", choices =  c("All",sort(unique(as.character(data$degree_of_injury))))),
                  selectInput("num_var_13", "Industry", choices =  c("All",sort(unique(as.character(data$industry))))),
                  selectInput("num_var_14", "Sub-Industry", choices =  c("All",sort(unique(as.character(data$sub_industry))))),
                  selectInput("num_var_15", "Incident Type", choices =  c("All",sort(unique(as.character(data$incident_type))))),
                  selectInput("num_var_16", "Incident Agent", choices =  c("All",sort(unique(as.character(data$incident_agent))))),
                  selectInput("num_var_17", "Incident Agent Sub Type", choices =  c("All",sort(unique(as.character(data$incident_agent_sub_type)))))
                  
                ),
                box(
                  width=9,
                  height=580,
                    # plotOutput("plot"),
                    
                    tags$head(tags$script('
                        var dimension = [0, 0];
                        $(document).on("shiny:connected", function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });')),
                    selectInput("num_var_8", "Table Variable X", choices = names(data)),
                    textOutput("output_msg", inline = FALSE),
                    plotlyOutput("plot2", width = "auto")
                    
                  )
                 )
                

      ),
      # Second tab content
      tabItem(tabName = "ana_tab",
              h3("Findings"),
              fluidRow(
              box(
                width=12,
                height=500,
                h4(textOutput("output_msg_3", inline = FALSE), style="text-align: center;"),
                br(),
                plotlyOutput("plot3", width = "auto")
              ),
              box(
                width=12,
                height=500,
                h4(textOutput("output_msg_4", inline = FALSE), style="text-align: center;"),
                br(),
                plotlyOutput("plot4", width = "auto")
              ),
              
              box(
                width=12,
                height=1000,
                h4(textOutput("output_msg_5", inline = FALSE), style="text-align: center;"),
                br(),
                plotlyOutput("plot5_1", width = "auto"),
                br(),
                plotlyOutput("plot5_2", width = "auto")
              ),
              )
      
      ),
      
      # 4 tab content
      tabItem(tabName ="dash_tab",
                fluidRow(
                  valueBoxOutput("info_4"),
                  valueBoxOutput("info_5"),
                  valueBoxOutput("info_6"),
                  valueBoxOutput("info_1"),
                  valueBoxOutput("info_2"),
                  valueBoxOutput("info_3"),
                  
                  br(),
                  box(
                    width=12,
                    background='black',
                    h4(textOutput("dash_variable"), color="white")
                  ),
                  box(
                    width=12,
                    plotlyOutput("plot_dash_1", width = "auto")
                  )
                  
                )
        )
      
      
      
    )
  )
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  
  
  
  data2<-data
  data3<-data
  data4<-data
  data5_1<-data
  data5_2<-data
  
  output$info_4 <- renderValueBox({
    valueBox(
      "Domain", paste0("Occupational Health and Safety"), icon = icon("hard-hat"),
      color = "black"
    )
  })
  
  
  output$info_5 <- renderValueBox({
    valueBox(
      "Title", paste0("Analysis of Workplace Risk"), icon = icon("car-crash"),
      color = "black"
    )
  })
  
  
  output$info_6 <- renderValueBox({
    valueBox(
      "Data Source", paste0("https://data.gov.sg"), icon = icon("download"),
      color = "black"
    )
  })
  
  
  
  output$info_1 <- renderValueBox({
    valueBox(
      "Observation", paste0(nrow(data)," Rows"), icon = icon("align-justify"),
      color = "black"
    )
  })
  
  
  output$info_2 <- renderValueBox({
    valueBox(
      "Injuries", paste0(sum(data$no_of_injuries), " Cases"), icon = icon("ambulance"),
      color = "black"
    )
  })
  
  
  output$info_3 <- renderValueBox({
    valueBox(
      "Data Info", "Figures are victim-based", icon = icon("calendar-alt"),
      color = "black"
    )
  })
  
  
  output$dash_variable<-renderText({paste("Data Variable :",toString(names(data)))})
  
  
  output$plot_dash_1 <- renderPlotly({
    
    df <- data
    x_axis_labels <- min(df[,'year']):max(df[,'year'])
    bar_plt <- ggplot(df, aes_string(x="year", y = df$no_of_injuries, fill=as.factor("year")))+
      theme_minimal() +
      theme(legend.text=element_text(size=5))+
      geom_path(stat="summary", fun=sum ) +
      ylab("No of Injuries") +
      xlab("Year") +
      theme(text = element_text(size=theme.size))
  
    bar_plt <- bar_plt + ggtitle(paste("The Graph of Yearly Injuries (2011-2018)"))
    bar_plt <- bar_plt + scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels)
    ggplotly(bar_plt)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$contents <- DT::renderDataTable(
    DT::datatable({
      if (input$num_var_0 != "All") {
        data <- data[data$year == input$num_var_0,]
      }
      
      if (input$num_var_1 != "All") {
        data <- data[data$degree_of_injury == input$num_var_1,]
      }
      
      if (input$num_var_2 != "All") {
        data <- data[data$industry == input$num_var_2,]
      }
      
      if (input$num_var_3 != "All") {
        data <- data[data$sub_industry == input$num_var_3,]
      }
      
      if (input$num_var_4 != "All") {
        data <- data[data$incident_type == input$num_var_4,]
      }
      
      if (input$num_var_5 != "All") {
        data <- data[data$incident_agent == input$num_var_5,]
      }
      
      if (input$num_var_6 != "All") {
        data <- data[data$incident_agent_sub_type == input$num_var_6,]
      }
      
      colnames(data) <- c("Year","Degree Of Injury","Industry","Sub-Industry","Incident Type","Incident Agent","Incident Agent Sub Type","No of Injury")
      data
    },
    extensions = 'Buttons',
    options = list(
      searching = FALSE,
      paging = TRUE,
      #fixedColumns = TRUE,
      #autoWidth = TRUE,
      #ordering = FALSE,
      dom = 'Bfrtip',
      pageLength = if(nrow(data)<30){nrow(data)} else{'30'},
      buttons = c('copy', 'csv', 'excel','pdf')
    ),
    
    class = "display"
    
    )
    
  )
  
  
  
  observe({
    colnames(data2) <- c("Year","Degree Of Injury","Industry","Sub-Industry","Incident Type","Incident Agent","Incident Agent Sub Type","No of Injuries")
    updateSelectInput(session, 'num_var_8', choices = colnames(data2)[colnames(data2) != "No of Injuries"])
    
    
      
  }) 
  
  
  geom.text.size = 3
  theme.size = (14/5) * geom.text.size
  
  toListen <- reactive({
    list(
      input$dimension,
      input$num_var_8,
      input$num_var_11,
      input$num_var_12,
      input$num_var_13,
      input$num_var_14,
      input$num_var_15,
      input$num_var_16,
      input$num_var_17
    )
  })
  
  wrapper <- function(x, ...) 
  {
    paste(strwrap(x, ...), collapse = "\n")
  }
  
  observeEvent(toListen(),{
    
    title_ext <- NULL
   
    
    if (input$num_var_11 != "All") {
      data2 <- data2[data2$year == input$num_var_11,]
      title_ext<-paste(title_ext,"for",input$num_var_11 )
      
    }
    
    if (input$num_var_12 != "All") {
      data2 <- data2[data2$degree_of_injury == input$num_var_12,]
      title_ext<-paste(title_ext,"of",input$num_var_12,"Injury Cases" )
     
    }
    
    if (input$num_var_13 != "All") {
      data2 <- data2[data2$industry  == input$num_var_13,]
      title_ext<-paste(title_ext,"in",input$num_var_13,"Industry" )
      
    }
    
    if (input$num_var_14 != "All") {
      data2 <- data2[data2$sub_industry == input$num_var_14,]
      title_ext<-paste(title_ext,"in",input$num_var_14,"Sub-Industry" )
      
    }
    
    if (input$num_var_15 != "All") {
      data2 <- data2[data2$incident_type == input$num_var_15,]
      title_ext<-paste(title_ext,"for",input$num_var_15,"Incident" )
      
    }
    
    if (input$num_var_16 != "All") {
      data2 <- data2[data2$incident_agent == input$num_var_16,]
      title_ext<-paste(title_ext,"where",input$num_var_16,"As Incident Agent " )
      
    }
    
    if (input$num_var_17 != "All") {
      data2 <- data2[data2$incident_agent_sub_type == input$num_var_17,]
      title_ext<-paste(title_ext,"in which",input$num_var_17,"As Incident Agent Sub Type" )
      
    }
    
    
    
    title_ext<-wrapper(title_ext,100)
    input_p <- to_snake_case(input$num_var_8)
    
    data2
    
    #reorder(df[[input_p]],df$no_of_injuries,FUN=sum)
    #print(nrow(data2))
    
    if(input_p!="year" && nrow(data2)>0){
      output$plot2 <- renderPlotly({
        df <- data2
        bar_plt <- ggplot(df, aes_string(x =reorder(df[[input_p]],df$no_of_injuries,FUN=sum), y = data2$no_of_injuries, fill=input_p))+
          theme_minimal() +
          theme(legend.text=element_text(size=5))+
          geom_bar(stat="summary", fun=sum,width=0.5) + 
          ylab("No of Injuries") +
          xlab(to_upper_camel_case(input_p, sep_out = " ")) +
          coord_flip()+
        theme(text = element_text(size=theme.size))
      
        bar_plt <- bar_plt + scale_x_discrete(label = " ")
        bar_plt <- bar_plt + scale_fill_viridis_d(name = NULL )
        bar_plt <- bar_plt + ggtitle(paste("The Graph of", paste(input$num_var_8 , "vs No of Injuries \n",title_ext)))
        ggplotly(bar_plt, width = (0.6*as.numeric(input$dimension[1])), height = (0.6*as.numeric(input$dimension[2])))
        
        
      })
      
      output$output_msg<-NULL
    } else if(nrow(data2)>0) {
      
      output$plot2 <- renderPlotly({
        df <- data2
        x_axis_labels <- min(df[,'year']):max(df[,'year'])
        
        bar_plt <- ggplot(df, aes_string(x = input_p, y = data2$no_of_injuries, fill=as.factor(input_p)))+
          geom_bar(stat="summary", fun=sum ) +
          ylab("No of Injuries") +
          xlab("Year") +
          coord_flip()+
          theme(text = element_text(size=theme.size))
          
        bar_plt <- bar_plt + scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels)
        bar_plt <- bar_plt + scale_fill_viridis_d(name = NULL,)
        bar_plt <- bar_plt + ggtitle(paste("The Graph of",input$num_var_8 , "vs No of Injuries \n",title_ext))
        ggplotly(bar_plt, width = (0.6*as.numeric(input$dimension[1])), height = (0.6*as.numeric(input$dimension[2])))
        
        
      })   
      output$output_msg<-NULL
      
      
    } else if(nrow(data2)<=0){
      #print("row is empty")
      output$plot2 <<- NULL
      output$output_msg <-renderText({paste("   Data don't exist!  ")})
    }
  })
  
  
  
  output$plot3 <- renderPlotly({
    
    
    
    df <- data3
    
    bar_plt <- ggplot(df, aes_string(x=reorder(df[["industry"]],df$no_of_injuries,FUN=sum), y = df$no_of_injuries, fill="industry"))+
    theme_minimal() +
      theme(legend.text=element_text(size=5))+
      geom_bar(stat="summary", fun=sum, width=0.5) + 
      ylab("No of Injuries") +
      xlab("Industry") +
      coord_flip()+
      theme(text = element_text(size=theme.size))
    
    bar_plt <- bar_plt + scale_x_discrete(label = " ")
    bar_plt <- bar_plt + scale_fill_viridis_d(name = NULL )
    bar_plt <- bar_plt + ggtitle(paste("The Graph of Industry vs No of Injuries"))
    ggplotly(bar_plt)
    
    
  })
  
  output$output_msg_3 <-renderText({paste("Which industries have the highest numbers of workplace injuries?  ")})
  
  
  
  output$plot4 <- renderPlotly({
    data_full <- reactiveValues()
    data_fatal <- data4[data4$degree_of_injury =="Fatal",]
    data_major <- data4[data4$degree_of_injury =="Major",]
    data_full  <- rbind(data_fatal, data_major)
    
    
    df <- data_full
    bar_plt <- ggplot(df, aes_string(x=reorder(df[["industry"]],df$no_of_injuries,FUN=sum), y = df$no_of_injuries, fill="degree_of_injury"))+
      theme_minimal() +
      theme(legend.text=element_text(size=5))+
      
  
      geom_bar(stat="summary", fun=sum, width=0.5) + ylab("No of Injuries") +
      xlab("Industry") +
      coord_flip()+
      theme(text = element_text(size=theme.size))
    
    bar_plt <- bar_plt + scale_x_discrete(label = " ")
    bar_plt <- bar_plt + scale_fill_viridis_d(name = NULL )
    bar_plt <- bar_plt + ggtitle(paste("The Graph of Industry vs No of Injuries for Fatal and Major Injuries Cases"))
    ggplotly(bar_plt)
    
    
  })
  
  output$output_msg_4 <-renderText({paste("Which industries have the highest number of fatal and major workplace injuries?")})
  
  
  output$plot5_1 <- renderPlotly({
 
    df <- data5_1
    bar_plt <- ggplot(df, aes_string(x=reorder(df[["incident_type"]],df$no_of_injuries,FUN=sum), y = df$no_of_injuries, fill="incident_type"))+
      theme_minimal() +
      theme(legend.text=element_text(size=5))+
      geom_bar(stat="summary", fun=sum, width=0.5) + 
      ylab("No of Injuries") +
      xlab("Incident Type") +
      coord_flip()+
      theme(text = element_text(size=theme.size))
    
    bar_plt <- bar_plt + scale_x_discrete(label = " ")
    bar_plt <- bar_plt + scale_fill_viridis_d(name = NULL )
    bar_plt <- bar_plt + ggtitle(paste("The Graph of Incident Type vs No of Injuries "))
    ggplotly(bar_plt)
    
    
  })
  
  
  output$plot5_2 <- renderPlotly({
    
    df <- data5_2
    bar_plt <- ggplot(df, aes_string(x=reorder(df[["incident_agent"]],df$no_of_injuries,FUN=sum), y = df$no_of_injuries, fill="incident_agent"))+
      theme_minimal() +
      theme(legend.text=element_text(size=5))+
      geom_bar(stat="summary", fun=sum, width=0.5) + 
      ylab("No of Injuries") +
      xlab("Incident Agent") +
      coord_flip()+
      theme(text = element_text(size=theme.size))
    
    bar_plt <- bar_plt + scale_x_discrete(label = " ")
    bar_plt <- bar_plt + scale_fill_viridis_d(name = NULL )
    bar_plt <- bar_plt + ggtitle(paste("The Graph of Incident Agent vs No of Injuries "))
    ggplotly(bar_plt)
    
    
  })
  
  output$output_msg_5 <-renderText({paste("What are the most common incident types/agents that cause injuries?")})
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
