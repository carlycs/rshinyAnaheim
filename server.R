#Edited by Hoofar Pourzand
#Fetched Originally from Nabeel M A 

# Summary of functionality for the first step
# 0. Check slack channel for the task list update accordingly 
# 1. Read Date and Hour from data.csv file - ight now this is limited to Jan 31 and few other data 
# 2. Run randomforest model for any day in the calender - 
# 6. Changing any parameter of model in the GUI will rerun the model





dat<- Sys.Date()
model_file<-paste0("model_saved",dat,".RData")

#Start of connection to shinyServer

shinyServer(function(input, output, session) {
  # Login logic ####
  rvlogin <- reactiveValues(login = FALSE, user = NULL, role = NULL, email = NULL)
  login <- reactive({ rvlogin })
  
  # initially display the login modal
  observe({ composeLoginModal() })
  
  # Logs out user
  observeEvent(input$logout_ok, {
    shiny::removeModal()
    
    # clear the values when logout is confirmed
    rvlogin$login <- FALSE
    rvlogin$user  <- NULL
    rvlogin$role  <- NULL
    
    composeLoginModal(
      div(
        id    = "modal-logout-message"
        , style = "margin-bottom: 10px"
        , span(class = "text-muted", "Successfully Logged Out")
      ) #/ modal-logout-message
    ) #/ composeLoginModal
  })
  
  # once a login is attempted, do some checks
  observeEvent(input$login_button, {
    # remove the modal while we check
    shiny::removeModal()
    
    # try to login, will automatically handle NULL-y objects
    loginResult <- validateLogin(input$login_user, input$login_passwd)
    
    # if the login is not successful, toss up another login modal, 
    # this time with a message
    if (!loginResult$login) {
      composeLoginModal(
        div(
          id    = "modal-login-message"
          , style = "margin-bottom: 10px"
          , span(style = "color: red; font-weight:bold", "Incorrect Login/Password")
        ) #/ modal-login-message
      ) #/ composeLoginModal
    } else {
      # if the login is successful, populate the known values
      rvlogin$login <- TRUE
      rvlogin$user  <- loginResult$user
      rvlogin$role  <- loginResult$role
    }
  }) #/ login_button Observer
  
  # Display main ui iff user is logged in
  output$mainUI <- renderUI({
    req(login()$login)
    ui.main # sourced from ui.main.R
  })
  
  
  
  
  # ----------------------------------------------------------------
  # Main App logic ####
  output$selectModelTraining <- renderUI({
    if(login()$role == "Manager"){
      tagList(
        selectInput('model', 'From Which Model', c("RF","NNetwork","RF and NNetwork"),selected="RF and NNetwork"),
        actionButton(inputId = "livetrain", label = "Train Model with Live Data",class = "btn-primary"),
        checkboxInput("save", "Update saved files", value = TRUE, width = NULL)
      )
    } else if(login()$role == "User"){
      NULL
    }
  })
  
  output$appTitle <- renderUI({
    if(login()$role == "Manager"){

      
      titlePanel(paste0("Configuration ", login()$role), windowTitle = "Anaheim Station Forecasts")

    } else if(login()$role == "User"){
      titlePanel(paste0("Anaheim Station"), windowTitle = "Anaheim Station Forecasts")
    }
  })
  
  output$currentTime <- renderUI({
    invalidateLater(as.integer(500),session)
    localTime <- paste0(strong("Local Time: "), 
                        format(Sys.time(), format = "%A, %B %e, %Y | %k:%M:%S %Z"))
    HTML(paste(localTime, sep = '<br/>'))
  })
  
  dateChoiceOptions <- c("Today","Calendar")
  
  # Output for date type dropdown menu
  output$dateChoiceType <- renderUI({
    selectizeInput('dateType', 'Date', 
                   choices = dateChoiceOptions,
                   selected = "Today"
    )
  })
  
  # Output for calendar selection menu; conditional on date type dropdown menu
  output$dateCalendar <- renderUI({
    req(input$dateType)
    if((input$dateType)=="Calendar"){
      dateInput("calendarDateInput","Calendar",format = "yyyy-mm-dd")
    }
  })
  
  # dateToday used as a system time variable
  dateToday <- as.POSIXct(format(format(Sys.time(), format = "%Y-%m-%d")))
  
  # Intialize reactiveValues
  chosenDate <- reactiveValues(date=dateToday)
  stateTracker <- reactiveValues(activeDate=dateToday)
  dateData <- reactiveValues(dateFrame = subset(serverData, yearMonthDay == dateToday))
  
  # Debugging code to check active date
  # print(str(isolate(chosenDate$date)))
  # output$debugDate <- renderPrint(paste0("You have chosen: ",chosenDate$date))
  
  # Debugging code to check currently selected dataframe
  # output$dateDataDebug <- renderPrint(
  #   str(isolate(dateData$dateFrame))
  # )
  
  observeEvent(input$dateType=="Today",{
    chosenDate$date <- as.POSIXct(format(format(Sys.time(), format = "%Y-%m-%d")))
  })
  
  # Previous Day Button
  observeEvent(input$prevDay,{
    req(input$dateType)
    if (input$dateType!="Calendar"){
      updateSelectizeInput(session,"dateType",selected="Calendar")
      updateDateInput(session,"calendarDateInput",value=input$calendarDateInput-days(1))
      chosenDate$date <- chosenDate$date - days(1)
    }
    else{
      updateDateInput(session,"calendarDateInput",value=input$calendarDateInput-days(1))
      chosenDate$date <- chosenDate$date - days(1)
    }
  })
  
  # Next Day Button
  observeEvent(input$nextDay,{
    req(input$dateType)
    if (input$dateType!="Calendar"){
      updateSelectizeInput(session,"dateType",selected="Calendar")
      updateDateInput(session,"calendarDateInput",value=input$calendarDateInput + days(1))
      chosenDate$date <- chosenDate$date + days(1)
    }
    else{
      updateDateInput(session,"calendarDateInput",value=input$calendarDateInput + days(1))
      chosenDate$date <- chosenDate$date + days(1)
    }
  })
  
  observeEvent(!is.null(chosenDate$date),{
    dateData$dateFrame <- subset(serverData,yearMonthDay==chosenDate$date)
  }
  )
  
  # Check to see if date has changed
  observeEvent(input$calendarDateInput != stateTracker$activeDate,{
    chosenDate$date <- input$calendarDateInput
    stateTracker$activeDate <- chosenDate$date
    dateData$dateFrame <- subset(serverData, yearMonthDay == chosenDate$date)
    output$barplot <- renderPlotly({ makeBarplot()$Likelihood })
    output$queueplot <- renderPlotly({ makeBarplot()$Queue })
  })
  
  # Function to make barplot, and check for empty dataframe
  makeBarplot <- reactive({
    plotList <- list()
    plotDf <- dateData$dateFrame
    p <- ggplot(plotDf, aes(x=hour,fill=outofgaslikelihood)) + geom_bar(aes(weight=outofgaslikelihood))
    p <- p + labs(x = "Hour", y = "Out of Gas Likelihood", 
                  title = paste0("Out of Gas Likelihood By Hour For ",
                                 format(dateToday,format="%B %d, %Y"))
    )
    final <- p + scale_y_continuous(expand = c(0,1)) + ylim(0,1.0) + 
      scale_fill_gradient(low = "#1A9850", high = "#D73027", limits=c(0,1)) +
      theme(legend.title=element_blank(), 
            axis.text.x = element_text(angle = 45),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black")
      ) # Change label angle here
    finalPlot <- ggplotly(final) %>% config(displayModeBar = F) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
    finalPlot$x$layout$width <- NULL
    finalPlot$x$layout$height <- NULL
    finalPlot$width <- NULL
    finalPlot$height <- NULL
    plotList$Likelihood <- finalPlot
    
    p <- 
      ggplot(plotDf, aes(x=hour, y=queueline)) + 
      geom_col() + 
      labs(x = "Hour", y = "Queue Length", 
           title = paste0("Queue Length By Hour For ",
                          format(dateToday,format="%B %d, %Y"))
      )
    final <- p +
      theme(
        legend.title=element_blank(), 
        axis.text.x = element_text(angle = 45),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")
      ) # Change label angle here
    finalPlot <- ggplotly(final) %>% config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
    finalPlot$x$layout$width <- NULL
    finalPlot$x$layout$height <- NULL
    finalPlot$width <- NULL
    finalPlot$height <- NULL
    
    plotList$Queue <- finalPlot
    
    plotList
  })
  
  # Function that acutally outputs barplot plotly object
  output$barplot <- renderPlotly({
    makeBarplot()$Likelihood
  })
  
  output$queueplot <- renderPlotly({
    makeBarplot()$Queue
  })
  
  # Data outputs
  # output$queueDay <- renderPrint(print(dateData$dateFrame$yearMonthDay[1]))
  # output$queueLength <- renderPrint(print(dateData$dateFrame$queueline))
  # output$outofgaslikelihood <- renderPrint(print(dateData$dateFrame$outofgaslikelihood))
  

  
=======
  #function to plot the forecast  
  plot_hybrid_forecast<- function(){
    nahead<-input$nahead
    code<- input$incident
    if(!file.exists("log_run.csv")){
      print("First run !!! click train")
    }
    #fitted values and residues are store in log_run.csv 
    mdata<- read.csv("log_run.csv")
    #forecast values are stored in forecast.csv.
    fdata<- read.csv("forecast.csv")
    tsmdata_i<-mdata[mdata$code==input$incident,]
    tsfdata_i<-fdata[fdata$code==input$incident,]
    #recreate the time series of data,fitted value and residue for ploting
    tdata <-xts(tsmdata_i$incident_count,order.by =as.Date(tsmdata_i$date ))
    ar_fited<-xts(tsmdata_i$arima_fitted ,order.by =as.Date(tsmdata_i$date ))
    res_ts <- xts(tsmdata_i$arima_residuals,order.by =as.Date(tsmdata_i$date ))
    
    dnn_res_ts <-xts(tsmdata_i$nn_residuals,order.by =as.Date(tsmdata_i$date ))
    h_fitted_ts <- xts(tsmdata_i$hybrid_fitted,order.by =as.Date(tsmdata_i$date ))
    
    
    pr_v_a<-xts(tsfdata_i$arima_validation_results,order.by =as.Date(index(tdata[(nrow(tdata)+1-nahead):nrow(tdata)] )))
    pr_v_h<- xts(tsfdata_i$hybrid_validation_results,order.by =as.Date(index(tdata[(nrow(tdata)+1-nahead):nrow(tdata)] ) ))
    p_nn_forecast_v_ts<- xts(tsfdata_i$nn_val_results,order.by =as.Date(index(tdata[(nrow(tdata)+1-nahead):nrow(tdata)] ) ))
    
    rr_f <- xts(tsfdata_i$ARIMA_forecast ,order.by =as.Date(tsfdata_i$date ))
    pr_f<- xts(tsfdata_i$hybrid_forecast ,order.by =as.Date(tsfdata_i$date ))
    p_nn_forecast_ts<- xts(tsfdata_i$NN_forecast,order.by =as.Date(tsfdata_i$date ) )
    
    
    p_nn_fitted_ts<-xts(tsmdata_i$nn_fitted,order.by = as.Date(index(tdata) ))
    p_nn_residuals_ts<-xts(tsmdata_i$nn_residuals,order.by = as.Date(index(tdata) ))
    
    
    #make plots based on the selection of inputs   
    if(input$model=="ARIMA" & input$resi_plot=="Incident count"){
      par(mfrow=c(1,1))
      plot(tdata,xlim=as.POSIXct(c(min(index(tdata)),  max(index(tdata))+(nahead+1)*31)),
           ylim=c(min(tdata,ar_fited,rr_f,na.rm = TRUE)-2, max(tdata,rr_f,ar_fited,na.rm = TRUE)+2 ) ,type="b",main=paste("ARMIA Model Prediction ",code),ylab="Incident Count")
      lines(ar_fited,col="green",type="b")
      lines(pr_v_a,col="red",type="b")
      lines(rr_f,col="blue",type="b")
      legend("topleft", c("Actual Value","Predicted during Validation","Forecast","Fitted Value"), col=c("black","red","blue","green"), lty=1)
      
    }  
  }

  ########################################

  
  # Histogram Reaction function
  run_hist <- function(hour_range,date_range) {
    filter(serverData,datetimehourly>date_range[1] & datetimehourly<date_range[2]) %>% 
      mutate(days=day(datetimehourly),
             hours=hour(datetimehourly),
             hours_ampm=if_else(hours<13,paste0(hours,' AM'),paste0(hours-12,' PM')),
             hours_ampm=recode(hours_ampm,`0 AM`='12 AM',
                               `12 AM`='12 PM'),
             hours_ampm=factor(hours_ampm,levels=c(paste0(c(12,1:11),' AM'),
                                                   paste0(c(12,1:11),' PM'))),
             highlight_hour=if_else(hours==hour(Sys.time()),TRUE,FALSE)) %>% 
      filter(hours>hour_range[1] & hours<hour_range[2]) %>% 
      ggplot(aes(x=hours_ampm)) + geom_bar(aes(fill=highlight_hour)) +
      theme_minimal() + theme(panel.grid=element_blank()) + 
      xlab("") + ylab("Queue Lines") +
      guides(fill=FALSE) + 
      scale_fill_brewer()
  }
  
  #######################################  


  output$hist_react <- renderPlot({
    run_hist(input$highlight_hist,input$date_time) %>% 
      print
  })
  observeEvent(input$train, {
    if(!file.exists("data.csv")){
      print("-E- Input file data.csv !!! Not found in directory")
      return()
    } else {
      
      analyse_all_incidents()
    }
  }
  )
  
  # output$table_data <- DT::renderDataTable({
  #                         input$train
  #                         input$incident
  #                         mdata<- read.csv("log_run.csv")
  #                         DT::datatable(mdata[-1],rownames=FALSE)
  #                         
  #                         })
  output$next_expected_h2_delivery <-renderPrint(print("dummy2"))
  output$today_rush_hours <-renderPrint(print("dummy3"))
  
  
  
})
