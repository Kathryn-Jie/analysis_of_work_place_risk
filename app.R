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
library(forcats)
library(shinyjs)
library(ggcorrplot)
library(tidytext)



data <- read.csv("data.csv",stringsAsFactors = TRUE)
df <- data.frame(data)

ui <- dashboardPage(skin = "black",
                    dashboardHeader(),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Dashboard", tabName = "dash_tab", icon = icon("home")),
                        menuItem("Risk Insight", tabName = "ana_tab", icon = icon("chart-bar")),
                        menuItem("Explore Risk", tabName = "plot_tab", icon = icon("compass"))
                        
                        
                      )
                    ),
                    
                    dashboardBody(
                      tags$head(tags$style(HTML(
                                '.skin-black .main-header .navbar{
                                  background-color: #222d32;
                                }

                                .skin-black .main-header>.logo:hover{
                                  background-color: #222d32;
                                  border-right: 1px solid #222d32;
                                }
                                .skin-black .main-header>.logo{
                                  background-color: #222d32;
                                  border-right: 1px solid #222d32;
                                  color : white;

                                }
                                .small-box h3{
                                  font-size : 24px!important;
                                }
                                .small-box .icon-large {
                                  font-size : 50px!important;
                                }

                                .skin-black .main-header .navbar>.sidebar-toggle{
                                  color:#fff;
                                  border-right: 1px solid #222d32;
                                }
                                .skin-black .main-header .navbar>.sidebar-toggle:hover{
                                  background-color: #222d32;
                                  color:#fff;
                                  border-right: 1px solid #222d32;
                                }
                                /* other links in the sidebarmenu when hovered */
                                .skin-black .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #4e565a;
                                }
                                 /* active selected tab in the sidebarmenu */
                                .skin-black .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #4e565a;
                                }

                                .alert-danger, .alert-error, .bg-red, .callout.callout-danger, .label-danger, .modal-danger .modal-body
                                {
                                  background-color:#4e565a!important;
                                }

                                .nav-tabs-custom .nav-tabs li.active 
                                {
                                  border-top-color: #222d32;
                                }

                                .h1, .h2, .h3, h1, h2, h3 {
                                  margin-top: 5px!important;

                                }

                                .box{
                                  margin-bottom:5px!important;
                                }

                                .fa-check-circle{
                                  margin-top: 10px!important;
                                }

                                '
                                ))),

                      tabItems(
                        
                        # Second tab content
                        tabItem(tabName = "plot_tab",
                                # tags$style(js),
                                fluidRow(
                                  box(
                                    width=3,
                                    selectInput("input_industry", "1. Select Your Industry", choices =  c("All",sort(unique(as.character(data$industry))))),
                                    selectInput("input_activity", "2. Select Your Work Activity", choices =  c("All",sort(unique(as.character(data$sub_industry))))),
                                    selectInput("input_tool", "3. Select Your Work Tool", choices =  c("All",sort(unique(as.character(data$incident_agent_sub_type))))),
                                    actionButton("reset_button", "Reset")

                                  ),

                                  box(
                                    width=9,
                                    tabBox(
                                      width=12,
                                      side = "left", height = "auto",
                                      selected = "Risk by Year",
                                      tabPanel("Risk by Year", 
                                               plotlyOutput("plot_risk_year", width = "auto",height=800)
                                               ),
                                      
                                      tabPanel("Risk by Level of Injury", 
                                               plotlyOutput("plot_risk_injury", width = "auto")
                                               ),
                                      
                                      tabPanel("Risk by Past Incident" , 
                                               plotlyOutput("plot_past_incident", width = "auto",height=800)
                                               ),

                                      tabPanel("View the Data", 
                                        DT::dataTableOutput('data_view_dt')
                                      )
                                      
                                    
                                    ),
                                    )
                                )
                                
                                
                        ),
                        # Second tab content
                        tabItem(tabName = "ana_tab",
                                fluidRow(
                                  valueBoxOutput("info_obs",width=3),
                                  valueBoxOutput("info_injury",width=3),
                                  valueBoxOutput("info_period",width=3),
                                  valueBoxOutput("info_variable",width=3),

                                  box(width=12, 
                                  tabBox( width=12, 
                                    tabPanel("Industrial Injury", 
                                      column( width=12,
                                      plotlyOutput("plot_industry_insight")
                                      ),
                                      ),
                                    tabPanel("Degree of Injury", 
                                      column( width=12,
                                        plotlyOutput("plot_injury_insight",height =1000),
                                      ),
                                      h3(align="center" , "   "),
                                      
                                      box( width=12,
                                        selectInput("degree_industry", "Refine Degree of Injury by Industry", choices =  c("Marine")),
                                        plotlyOutput("plot_injury_insight_pie", inline=TRUE),
                                      )

                                      ),
                                    tabPanel("Agency of Injury", height="auto",
                                      br(),
                                      plotlyOutput("plot_incident_type", width = "auto",height=1000),
                                      box(width=12),
                                      column( width=4,
                                      selectInput("degree_incident", "Refine Incident Type", choices =  c("Suffocation")),
                                      ),
                                      column( width=3,
                                      selectInput("degree_incident_2", "Refine Degree of Injury", choices =  c("Fatal")),
                                      ),
                                      plotlyOutput("plot_incident_insight_pie", inline=TRUE),
                                      ),
                                    tabPanel("Correlation of Injury", height=1000,
                                      br(),br(),
                                      plotlyOutput("plot_corr_sort_insight", width = "500", inline=TRUE),
                                      plotlyOutput("plot_corr_base_insight", width = "500", inline=TRUE),
                                      br(),br(),
                                      selectInput("incident_agent", "Refine correlation", choices =  c("Physical Workplace")),
                                      plotlyOutput("plot_corr_refined_insight", width = "auto")
                                      ) 
                                  )
                                  )
                                )
                                
                        ),
                        
                        # 4 tab content
                        tabItem(tabName ="dash_tab",
                                fluidRow(
                                  column(width=12,

                                    valueBoxOutput("info_title",width=3),
                                    valueBoxOutput("info_domain",width=3),
                                    valueBoxOutput("info_dtsource",width=3),
                                    valueBoxOutput("info_dtfigure",width=3),
                                  ),
                                  
                                  column(width=6,
                                    box( width=12,height=60, align="center", h3("Objective")),
                                    box( width=12,height=67,
                                      column(width = 1,icon("check-circle")), 
                                      column(width = 11,h5("Identify workplace risk of injuries and determine whether an employee may be at risk."))
                                      ), 
                                    box( width=12,height=67,
                                      column(width = 1,icon("check-circle")), 
                                      column(width = 11,h5("Provide an early warning signal to both employer and employee especially industries with disproportionately high fatality rates.")),
                                      ),
                                    box( width=12,height=67,
                                      column(width = 1,icon("check-circle")), 
                                      column(width = 11,h5("Establish a safe workplace for long term measure.")),
                                      ),
                                    box( width=12,height=67,
                                      column(width = 1,icon("check-circle")), 
                                      column(width = 11,h5("Protect employer’s most valuable asset which is the employees.")),
                                      ),
                                  ),
                                  # ),
                                  column( width=6,
                                    box( width=12,height=60, align="center", h3("Problem Statement")),
                                    box( width=12,
                                    h5("The main reasons employees quit, is an unsafe work environment."),
                                    h5("Employers are obligated to maintain a safe workplace for their employees"),
                                    h5("Employers also need to compensate employees fairly and treat them with a sense of dignity and equality"), 
                                    h5("Thus, employee can focus on their job responsibilities and not worry about the work environment. "),
                                    h5("However, many employers fail to do so, and employees are injured every year as a result. "),
                                    h5("According to the International Labour Organisation, a worker dies every 15 seconds from a 
                                      work-related accident or disease around the world."),
                                    h5("A work environment that is unsafe not only can cause significant disruption and 
                                    cost to both employer and employee, but also put employer at risk of 
                                    workplace morale, productivity, turnover, and reputation."))
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
  
  output$info_domain <- renderValueBox({
    valueBox(
      "Domain", paste0("Occupational Health and Safety"), icon = icon("hard-hat"),
      color = "red"
    )
  })
  
  
  output$info_title <- renderValueBox({
    valueBox(
      "Title", paste0("Analysis of Workplace Risk"), icon = icon("car-crash"),
      color = "red"
    )
  })
  
  
  output$info_dtsource <- renderValueBox({
    url <- a("https://data.gov.sg", href="https://data.gov.sg/dataset/workplace-injuries-annual")
    valueBox(
      "Data Source", url, icon = icon("download"),
      color = "red"
    )
  })

  output$info_dtfigure <- renderValueBox({
    valueBox(
      "Data Info", "Figures are victim-based", icon = icon("user-check"),
      color = "red"
    )
  })

  
  
  
  output$info_obs <- renderValueBox({
    valueBox(
      "Observation", paste0(nrow(data)," Rows"), icon = icon("align-justify"),
      color = "red"
    )
  })
  
  
  output$info_injury <- renderValueBox({
    valueBox(
      "Injuries", paste0(sum(data$no_of_injuries), " Cases"), icon = icon("ambulance"),
      color = "red"
    )
  })

    output$info_period <- renderValueBox({
    valueBox(
      "Time Period", paste0("2011-2018"), icon = icon("calendar-alt"),
      color = "red"
    )
  })
  
  
  output$info_variable <- renderValueBox({
    valueBox(
      "Variable", paste0(ncol(data)-1, " columns"), icon = icon("columns"),
      color = "red"
    )
  })




  
  




  
  
  
  

  
  
  
  observe({
    colnames(data2) <- c("Year","Degree Of Injury","Industry","Sub-Industry","Incident Type","Incident Agent","Incident Agent Sub Type","No of Injuries")
  }) 
  
  
  geom.text.size = 3
  theme.size = (14/5) * geom.text.size
  
  toListen <- reactive({
    list(
      input$input_industry,
      input$input_activity,
      input$input_tool
    )
  })
  
  wrapper <- function(x, ...) 
  {
    paste(strwrap(x, ...), collapse = "\n")
  }
  

  observeEvent(input$reset_button, {
    
    updateSelectInput(session, 'input_industry',selected ="All" ,choices =  c("All",sort(unique(as.character(data$industry)))))
    updateSelectInput(session, 'input_activity',selected ="All" ,choices =  c("All",sort(unique(as.character(data$sub_industry)))))
    updateSelectInput(session, 'input_tool',selected ="All" ,choices =  c("All",sort(unique(as.character(data$incident_agent_sub_type)))) )
  })

  

  
  observeEvent(toListen(),{
    
    title_ext <- NULL
    if (input$input_industry == "All") {
      updateSelectInput(session, 'input_activity', choices = "")
      updateSelectInput(session, 'input_tool', choices = "")
    }

    if (input$input_industry != "All") {
      data2 <- data2[data2$industry  == input$input_industry,]
      title_ext<-paste(title_ext,"in",input$input_industry,"Industry" )
      updateSelectInput(session, 'input_industry', choices = input$input_industry)
      updateSelectInput(session, 'input_activity',selected = if(input$input_activity!="All"){input$input_activity} else {"All"}, choices = c("All",sort(unique(as.character(data2$sub_industry)))))
    }
    data2
    
    if (input$input_activity != "All") {
      updateSelectInput(session, 'input_activity',selected = if(input$input_activity!="All"){input$input_activity} else {"All"}, choices = c("All",sort(unique(as.character(data2$sub_industry)))))
      data2 <- data2[data2$sub_industry == input$input_activity,]
      title_ext<-paste(title_ext,"in",input$input_activity,"Sub-Industry" )
      updateSelectInput(session, 'input_activity', choices = input$input_activity)
      updateSelectInput(session, 'input_tool',selected = if(input$input_tool!="All"){input$input_tool} else {"All"}, choices = c("All",sort(unique(as.character(data2$incident_agent_sub_type)))))
      
    }
    data2
    
    if (input$input_tool != "All") {
      data2 <- data2[data2$incident_agent_sub_type == input$input_tool,]
      title_ext<-paste(title_ext,"in which",input$input_tool,"As Incident Agent Sub Type" )
     
      updateSelectInput(session, 'input_industry', choices = input$input_industry)
      updateSelectInput(session, 'input_activity', choices = input$input_activity)
      updateSelectInput(session, 'input_tool', choices = input$input_tool)
      
    }
    
    
    
    title_ext<-wrapper(title_ext,100)
    #input_p <- to_snake_case(input$num_var_8)


    output$data_view_dt <- DT::renderDataTable(
    DT::datatable({
      
      colnames(data2) <- c("Year","Degree Of Injury","Industry","Sub-Industry","Incident Type","Incident Agent","Incident Agent Sub Type","No of Injury")
      data2
    },
    extensions = 'Buttons',
    options = list(
      searching = FALSE,
      paging = TRUE,
      #fixedColumns = TRUE,
      #autoWidth = TRUE,
      #ordering = FALSE,
      dom = 'Bfrtip',
      pageLength = if(nrow(data2)<10){nrow(data2)} else{'10'},
      buttons = c('copy', 'csv', 'excel','pdf')
    ),
    
    class = "display"
    
    )
    
  )
    data2
    
    output$plot_risk_year <- renderPlotly({
      
      df <- data2
      x_axis_labels <- min(df[,'year']):max(df[,'year'])
      
      bar_plt <- ggplot(df, aes_string(x = "year", y = data2$no_of_injuries, fill=as.factor("year")))+
        geom_bar(stat="summary", fun=sum ) +
        ylab("No of Injuries") +
        xlab("Year") +
        theme(text = element_text(size=theme.size),legend.position = "none")
      
      bar_plt <- bar_plt + scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels)
      bar_plt <- bar_plt + scale_fill_grey(name = NULL)
      ggplotly(bar_plt,tooltip = c('x'))
      
    })
    
    output$plot_risk_injury <- renderPlotly({
      
      df <- data2
      df<- aggregate(no_of_injuries ~ degree_of_injury, df, sum)
      df<- df[order(df$no_of_injuries,decreasing = TRUE),]
      df$degree_of_injury<- factor(df$degree_of_injury, levels = df$degree_of_injury)
      fig_2_2 <- plot_ly(df, labels = ~degree_of_injury, values = ~no_of_injuries, 
          type = "pie",
          marker = list(colors = gray.colors(3)),
          hovertemplate = "<br>Percentage of %{label} Incidents: %{percent}</br>Total Number of %{label} Incidents: %{value}"
          )
      fig_2_2 <- fig_2_2 %>% layout(
          
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
         )

      fig_2_2
      
    })
    
    
    output$plot_past_incident <- renderPlotly({
      
      df <- data.frame(data2)
      df<- aggregate(no_of_injuries ~ incident_type, df, sum)
      df<- df[order(df$no_of_injuries,decreasing = TRUE),]
      df$incident_type<- factor(df$incident_type, levels = df$incident_type)
      
      bar_plt <- ggplot(df, aes_string(x =df$incident_type, y = df$no_of_injuries, fill="incident_type"))+
        theme_minimal() +
        theme(legend.text=element_text(size=rel(1)))+
        geom_bar(aes(text = paste("Total Number of Incidents from",incident_type,"are",no_of_injuries)),stat="identity", width=0.5) + 
        ylab("No of Injuries") +
        xlab(to_upper_camel_case("incident_type", sep_out = " ")) +
        theme(text = element_text(size=theme.size),strip.text = element_text(size=rel(1.75)))
      
      bar_plt <- bar_plt + scale_x_discrete(label = " ")
      bar_plt <- bar_plt + scale_fill_grey(name = NULL)
      ggplotly(bar_plt,tooltip = c('text'))
      
    })
  })
  
  
  

  
  


  
  output$plot_industry_insight <- renderPlotly({
    
    
    df_3 <- data.frame(data)
      df_3<- aggregate(no_of_injuries ~ industry, df_3, sum)
      df_3<- df_3[order(df_3$no_of_injuries,decreasing = TRUE),]
      df_3$industry<- factor(df_3$industry, levels = df_3$industry)


      fig_3 <- plot_ly(df_3, y = ~industry, x = ~no_of_injuries, type = 'bar', 
        marker = list( 
        color =  "#bbbbbb"  
        )
      )

      fig_3 <- fig_3 %>% layout(title = ' ', 
        xaxis = list(title = 'No of Injuries'),
        yaxis = list(title = 'Industry' ,tickwidth=5 ,tickcolor='white')
         )

      fig_3
    })

  
  
  
  output$plot_injury_insight <- renderPlotly({
   

    
    df_4 <- data4
    # df_4 <- prop.table(table(df_4[3:2]),1)*100
    df_4 <- aggregate(no_of_injuries~ industry + degree_of_injury, data=df_4, sum)
    df_4 <- transform(df_4, Freq = ave(no_of_injuries, industry, FUN = function(x) round(x/sum(x),3)*100))
    df_4 <- as.data.frame(df_4)

    reorder_industry <- reorder_within(df_4$industry,df_4$Freq,df_4$degree_of_injury)
    # print(reorder_industry)

    bar_plt <- ggplot(df_4, aes_string(x=reorder_industry, y = df_4$Freq ,fill=as.factor(df_4$degree_of_injury)))+
      theme_minimal() +
      theme(axis.text.y=element_text(size=rel(1.5)))+
      geom_bar(aes(text = paste(Freq,"% of Incidents from",industry,"Industry are",degree_of_injury,"Incidents.")),stat="identity" ,width=0.7, position=position_dodge(0.75)) + 
      xlab("") +
      ylab("Percentage of Degree of Injury By Industry") +
      theme(text = element_text(size=theme.size),strip.text = element_text(size=rel(1.75)))+
      scale_x_reordered()+
      coord_flip()
    bar_plt <- bar_plt + facet_grid(degree_of_injury~., scales = "free")
    bar_plt <- bar_plt + scale_fill_manual(values=c("#ec0d0d", "#181818","#bbbbbb"), name =NULL)
    ggplotly(bar_plt,tooltip = c('text'))
    
  })



  
  observeEvent(input$degree_industry, {

    updateSelectInput(session, 'degree_industry', selected= input$degree_industry, choices =c(sort(unique(as.character(data$industry)))))

    output$plot_injury_insight_pie <- renderPlotly({
      
      df <- data4
      df <- df[df$industry ==input$degree_industry,]
      df<- aggregate(no_of_injuries ~ degree_of_injury, df, sum)
      df<- df[order(df$no_of_injuries,decreasing = TRUE),]
      df$degree_of_injury<- factor(df$degree_of_injury, levels = df$degree_of_injury)
      fig_2_2 <- plot_ly(df, labels = ~degree_of_injury, values = ~no_of_injuries, 
          type = "pie",
          marker = list(colors = c("#bbbbbb", "#181818","#ec0d0d")),
          hovertemplate = "<br>Percentage of %{label} Incidents: %{percent}</br>Total Number of %{label} Incidents: %{value}"
          )
      fig_2_2 <- fig_2_2 %>% layout(
          
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
         )

      fig_2_2
      
    })
  })




  
  output$plot_incident_type <- renderPlotly({

    df_4 <- data4
    # df_4 <- prop.table(table(df_4[3:2]),1)*100
    df_4 <- aggregate(no_of_injuries~ incident_type+ degree_of_injury, data=df_4, sum)
    df_4 <- transform(df_4, Freq = ave(no_of_injuries, incident_type, FUN = function(x) round(x/sum(x),3)*100))
    df_4 <- as.data.frame(df_4)
    reorder_w<- reorder_within(df_4$incident_type,df_4$Freq,df_4$degree_of_injury)
    # print(reorder_industry)

    bar_plt <- ggplot(df_4, aes_string(x=reorder_w, y = df_4$Freq ,fill=as.factor(df_4$degree_of_injury)))+
      theme_minimal() +
      theme(axis.text.y=element_text(size=rel(1.5)))+
      geom_bar(aes(text = paste(Freq,"% of",incident_type,"Incidents are",degree_of_injury,"Incidents.")),stat="identity" ,width=0.7, position=position_dodge(width=0.75)) + 
      xlab("") +
      ylab("Percentage of Degree of Injury By Incident Type") +
      scale_x_reordered() +
      theme(text = element_text(size=theme.size),strip.text = element_text(size=rel(1.75)))+
      coord_flip()
    bar_plt <- bar_plt + facet_grid(degree_of_injury~., scales = "free")
    bar_plt <- bar_plt + scale_fill_manual(values=c("#ec0d0d", "#181818","#bbbbbb"), name = NULL)
    ggplotly(bar_plt,tooltip = c('text')) 
  })




  toListen_incident <- reactive({
    list(
      input$degree_incident,
      input$degree_incident_2,
    )
  })

  observeEvent(toListen_incident, {


    updateSelectInput(session, 'degree_incident', selected= input$degree_incident, choices =c(sort(unique(as.character(data$incident_type)))))
    updateSelectInput(session, 'degree_incident_2', selected= input$degree_incident_2, choices =c(sort(unique(as.character(data$degree_of_injury)))))

    output$plot_incident_insight_pie <- renderPlotly({
    df <- data4 
    df <- df[df$incident_type ==input$degree_incident,]
    df <- df[df$degree_of_injury ==input$degree_incident_2,]
    
    df<- if (nrow(df)>0) aggregate(no_of_injuries ~ industry, df, sum) else df
    validate(
      need( nrow(df) > 0, "There's no relevent data under these selections")
    )
    df<- df[order(df$no_of_injuries,decreasing = TRUE),]
    df$industry<- factor(df$industry, levels = df$industry)
    
      fig_2_2 <- plot_ly(df, labels = ~industry, values = ~no_of_injuries, 
          type = "pie",
          marker = list(colors = gray.colors(length(df$industry))),
          hovertemplate = "<br> %{percent} of Selected Incident Type is from %{label} Industry</br> Total Cases of Incidents: %{value}"
          )
      fig_2_2 <- fig_2_2 %>% layout(
          
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
         )

      fig_2_2
      
    })
  })



  




  output$plot_corr_base_insight <- renderPlotly({
    
    df_6<- df
    for(i in unique(df_6$incident_agent)){
      df_6[[to_snake_case(as.character(i))]]<-ifelse(df_6$incident_agent==as.character(i),df_6$no_of_injuries,0)  
    }
   
    df_6 <- df_6[, -c(1:7)]

    data_class <- sapply(df_6, unclass) 
    res <- cor(data_class)
    res <- as.data.frame(res)
    res <- res[,order(res$no_of_injuries)]
    names(res)<-to_upper_camel_case(names(res), sep_out = " ")
    row.names(res)<-to_upper_camel_case(row.names(res), sep_out = " ")
    res_2<-res
    res_2<-res_2[1,]
    matrix_plot<- ggcorrplot(res_2, method ="square",
                              colors = c("#181818", "white", "#ec0d0d"))   
    matrix_plot<- matrix_plot + theme(axis.text.x = element_blank(),axis.ticks.x = element_blank(),) 
    ggplotly(matrix_plot,  width=500)

   })


  output$plot_corr_sort_insight <- renderPlotly({
    
    df_6_1<- df
    for(i in unique(df_6_1$incident_agent)){
      df_6_1[[to_snake_case(as.character(i))]]<-ifelse(df_6_1$incident_agent==as.character(i),df_6_1$no_of_injuries,0)  
    }
   
    df_6_1 <- df_6_1[, -c(1:7)]

    data_class <- sapply(df_6_1, unclass) 
    res <- cor(data_class)
    res <- as.data.frame(res)
    # res <- res[,order(res$no_of_injuries)]
    names(res)<-to_upper_camel_case(names(res), sep_out = " ")
    row.names(res)<-to_upper_camel_case(row.names(res), sep_out = " ")
    matrix_plot<- ggcorrplot(res, method ="square",
                              colors = c("#181818", "white", "#ec0d0d"),title='Correlation of Injury With Incident Agent')  
    matrix_plot<- matrix_plot + theme(axis.text.x = element_blank(),axis.ticks.x = element_blank(),
                                      axis.text.y = element_blank(),axis.ticks.y = element_blank(),
                                      legend.position="none")                             
    ggplotly(matrix_plot,  width=400)

   })




  observeEvent(input$incident_agent, {
    df_6_1<- df
    for(i in unique(df_6_1$incident_agent)){
      df_6_1[[to_snake_case(as.character(i))]]<-ifelse(df_6_1$incident_agent==as.character(i),df_6_1$no_of_injuries,0)  
    }
   
    df_6_1 <- df_6_1[, -c(1:7)]

    data_class <- sapply(df_6_1, unclass) 
    res <- cor(data_class)
    res <- as.data.frame(res)
    res <- res[,order(-res$no_of_injuries)]
    names(res)<-to_upper_camel_case(names(res), sep_out = " ")
    row.names(res)<-to_upper_camel_case(row.names(res), sep_out = " ")
    res_2<-res
    res_2<-res_2[1,]
    
    updateSelectInput(session, 'incident_agent',selected =input$incident_agent ,choices =  c(colnames(res_2)[colnames(res_2) != "No Of Injuries"]))    
  

    output$plot_corr_refined_insight <- renderPlotly({
      df_6_2<- df
      df_6_2 <- df_6_2[df_6_2$incident_agent ==input$incident_agent,]

      for(i in unique(df_6_2$incident_agent_sub_type)){
        df_6_2[[to_snake_case(as.character(i))]]<-ifelse(df_6_2$incident_agent_sub_type==as.character(i),df_6_2$no_of_injuries,0)  
      }
     
      df_6_2 <- df_6_2[, -c(1:7)]

      data_class <- sapply(df_6_2, unclass)
      if(length(data_class)>1){ 
      corre <- cor(data_class)
      corre <- as.data.frame(corre)
      corre <- corre[,order(corre$no_of_injuries)]
      names(corre)<-to_upper_camel_case(names(corre), sep_out = " ")
      row.names(corre)<-to_upper_camel_case(row.names(corre), sep_out = " ")
      
        matrix_plot<- ggcorrplot(corre, method ="square",
                                  colors = c("#181818", "white", "#ec0d0d"),title='Correlation of Injury With Incident Agent Sub Type') 
        matrix_plot<- matrix_plot + theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) 
        ggplotly(matrix_plot, height=400)
      } 
   })

  })



  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
