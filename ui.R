library(shiny)
# Requirements
# position items on grids and layer them correctly so they are mobile friendly and desktop friendly (use fluidPage, etc.)
# #Calender Selector should open a calender DateInput to select the date Use:
    #For Dec 3rd, 2016 to June 3rd 2017, english- dateInput(inputId, label, value, min,
    #max, format, startview, weekstart,
    #language)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Configuration", windowTitle = "Anaheim Station Forecasts"),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(width = 3,
                 uiOutput("currentTime"),
                 uiOutput("dateChoiceType"),
                 uiOutput("dateCalendar"),
                 uiOutput("debugDate"),
                 uiOutput("dateDataDebug"),
                 h4("Plan your Visit"),
                 uiOutput("chooseDate"),
                 uiOutput("dateException"),
                 # uiOutput("ui"),
                 
                 # sliderInput('date_time','Date and Time Range',
                 #             min=min(serverData$datetimehourly),
                 #             max=max(serverData$datetimehourly),
                 #             value=c(serverData$datetimehourly[20],
                 #                     serverData$datetimehourly[300]),
                 #             timezone='+0000'),
                 # sliderInput('highlight_hist','Highlight Hour',min=0,max=24,value=set_time()),
                 selectInput('model', 'From Which Model', c("RF","NNetwork","RF and NNetwork"),selected="RF and NNetwork"),
                 actionButton(inputId = "livetrain", label = "Train Model with Live Data",class = "btn-primary"),
                 checkboxInput("save", "Update saved files", value = TRUE, width = NULL),
                 #numericInput('nahead', 'Number of Rush Hours to List', 3,min = 1, max = 5),
                 #h4("Results"),
                 
                 h4("Results"),
                 h5("Queue Line"),
                 uiOutput("queueDay"),
                 uiOutput("queueLength"),
                 textOutput(serverData$queueline[12]),
                 #h5("Next Expected H2 Delivery"),
                 #verbatimTextOutput("next_expected_h2_delivery"),
                 #h5("Today Rush Hours"),
                 #verbatimTextOutput("today_rush_hours"),
                 # Every output should have a title and then the value.
                 # numericInput('Queue Line', 'Most Likely Queue Line', 3, 
                 #              min = 0, max = 20),
                 # numericInput('Next Expected H2 Delivery', 'Next Expected H2 Delivery', 
                 #              1,min = 0, max = 20),
                 # numericInput('Today Rush Hours', 'Today Rush Hours', 3,min = 0, max = 20),
                 # numericInput('Queue Line2', 'Average Consumption Per car', 3,min = 0, max = 20)
                 # h4("Consumption Rates"),
                 # numericInput('nnsize', 'Average Consumption Per car', 8,min = 2, max = 20),
                 # numericInput('nnp', 'Number of non-seasonal lags', 3,min = 2, max = 30),
                 # numericInput('maxiter', '(kg)', 53, min = 50, max = 100)
                 h5("Out of Gas Likelihood"),
                 uiOutput("outofgaslikelihood")
    
          ),
    
    # Show a plot of the generated distribution
    mainPanel(
        (titlePanel("Anaheim Station Forecasts")),
        fluidRow(
          column(9,
                 #plotOutput("plot_main", width = 800, height = 600),
                 #plotOutput('hist_react',width=800,height=500),
                 #h2("Traffic and Weather Updates"),
                 #DT::dataTableOutput('table_data_fore'),
                 #h2("Special Notes Table"),
                 #DT::dataTableOutput('table_error'),
                 plotlyOutput("barplot", width = "100%"),
                 br(),
                 div(actionButton("prevDay", "Previous Day"), actionButton("nextDay", "Next Day"), align = "center"),
                 #actionButton(adddayvalue, "Next Day"), Should run the model for the next day and list results. 
                 DT::dataTableOutput('table_val'),
                 DT::dataTableOutput('table_data')
                 
          )
        )
      )
  )
))

