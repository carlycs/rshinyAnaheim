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
  titlePanel("Configuration"),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(width = 3,
                 h4("Plan your Visit"),
                uiOutput("ui"),
                selectInput('resi_plot', 'Date and Hour', c("Now","Calender Selector")), #Calender Selector should open a calender DateInput to select the date
                dateInput('dateinput', "label"),
                selectInput('model', 'From Which Model', c("RF","NNetwork","RF and NNetwork"),selected="RF and NNetwork"),
                actionButton(inputId = "livetrain", label = "Train Model with Live Data",class = "btn-primary"),
                checkboxInput("save", "Update saved files", value = TRUE, width = NULL),
                numericInput('nahead', 'Number of Rush Hours to List', 3,min = 1, max = 5),
                h4("Results"),
                #Every output should have a title and then the value.
                numericInput('Queue Line', 'Most Likely Queue Line', 3,min = 0, max = 20),
                numericInput('Next Expected H2 Delivery', 'Next Expected H2 Delivery', 1,min = 0, max = 20),
                numericInput('Today Rush Hours', 'Today Rush Hours', 3,min = 0, max = 20),
                #numericInput('Queue Line2', 'Average Consumption Per car', 3,min = 0, max = 20)
                
                h4("Consumption Rates"),
                #numericInput('nnsize', 'Average Consumption Per car', 8,min = 2, max = 20),
                #numericInput('nnp', 'Number of non-seasonal lags', 3,min = 2, max = 30),
                numericInput('maxiter', '(kg)', 53,min = 50, max = 100)
    
          ),
    
    # Show a plot of the generated distribution
    mainPanel(
      fluidPage(
        titlePanel("Anaheim Station Forecasts"),
        fluidRow(
          column(8,
                 plotOutput("plot_main", width = 800, height = 600),
                 
                 h2("Traffic and Weather Updates"),
                 DT::dataTableOutput('table_data_fore'),
                 h2("Special Notes Table"),
                 DT::dataTableOutput('table_error'),
                 h2("Next Day"),
                 #actionButton(adddayvalue, "Next Day"), Should run the model for the next day and list results. 
                 DT::dataTableOutput('table_val'),
         
                 
                 h2("Previous Day"),
                 DT::dataTableOutput('table_data')
                 
          )
        )
      )
      
    )
  )
))

