#Edited by Hoofar Pourzand
#Fetched Originally from Nabeel M A 

# Summary of functionality for the first step
# 0. Clean the ui and server files to remove extra variables, outputs, Error Messages. 
# 1. Read Date and Hour from data.csv file - will be uploaded with the correct format.
# 2. Run randomforest model
# 3. Store the results of this forecast, fitted values and residues in to forecast.csv and log_run.csv
# 4. Render plot from this analysis and other four values to ui.R
# 5. Model can be rerun by pressing rerun button 
# 6. Changing any parameter of model in the GUI will rerun the model

source("global.R")

arma.model.selection <- function (tsdata, p.max, d.max, q.max)
{     print("Start")
      D.max<-1
      best.aic <- 1e9
      best.model <- NA
      best.D <- NA
      best.d <- NA
      best.seasonal<- NA
      seasonal_l<- c(TRUE,FALSE)
      for (seasonal in seasonal_l){
        for (D in 0:D.max) {
          for (d in 0:d.max) {
            r <- list(aic=1e9)
            try(r <- auto.arima(tsdata,max.p=p.max,max.order=50,d=d,D=D,
                                max.d=4,max.D=4,max.P=5, max.Q=5,
                                approximation=FALSE,
                                seasonal=seasonal,stepwise=FALSE,
                                max.q=q.max,trace=FALSE,parallel=FALSE,
                                num.cores=NULL)
            )
            
            #select based on best aic
            #aic is measure of information loss 
            #lower  the better
            if (r$aic < best.aic)
            {
              print(sprintf("best.aic=%f", best.aic))
              print(sprintf("aic=%f", r$aic))
              best.aic <- r$aic
              best.model <- r
              best.d <-d
              best.D<- D
              best.seasonal<- seasonal
            }
          }
        }
      }
      list(best.aic = best.aic, 
           best.model = best.model, 
           best.d = best.d,best.D = best.D,best.seasonal=best.seasonal)
}

rms <- function(x) sqrt(mean(x^2),na.rm=TRUE)
ma <- function(x) mean(abs(x),na.rm=TRUE)

dat<- Sys.Date()
model_file<-paste0("model_saved",dat,".RData")

#Start of connection to shinyServer

shinyServer(function(input, output, session) {
  output$currentTime <- renderUI({
    invalidateLater(as.integer(500),session)
    localTime <- paste0(strong("Local Time: "), 
                        format(Sys.time(), format = "%A, %B %e, %Y | %k:%M:%S %Z"))
    HTML(paste(localTime, sep = '<br/>'))
  })
  
  dateChoiceOptions <- c("Today","Calendar")
  
  output$dateChoiceType <- renderUI({
    selectizeInput('date_type', 'Date', 
                   choices = dateChoiceOptions,
                   options = list(placeholder = 'Select a date below',
                                  onInitialize = I('function() { this.setValue(""); }')
                   )
    )
  })
  
  output$dateCalendar <- renderUI({
    req(input$date_type)
    if((input$date_type)=="Calendar"){
      dateInput("calendarDateInput","Calendar")
    }
  })
  
  chosenDate <- reactiveValues(date="")
  
  # assignDate <- reactive({
  #   if (input$date_type == 'Today'){
  #     chosenDate$date <- 
  #   }
  #   
  # })
  
  output$queueLength <- renderPrint(print(serverdata$queueline[12]))
  output$queueDay <- renderPrint(print(serverdata$datetimehourly[12]))
  output$outofgaslikelihood <- renderPrint(print(serverdata$outofgaslikelihood[12]))

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
#analyse_all_incidents and save results and model to file 
analyse_all_incidents<- function(){  
    #data prepare master data time_series_m
    data<-read.csv("data.csv")
    mont<- factor(data$Month, levels= c(1:12), labels = c('Jan', 'Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))
    time<- as.Date(paste0(data$Year,"-",as.character(mont),"-01"),format("%Y-%b-%d"))
    data_clean<- data
    data_clean$time<- time
    time_series_m <- NULL
    
     for(c in unique(data_clean$Code)){
      time_series<-xts(data_clean$Incidence.Count[data_clean$Code==c],
                       order.by =data_clean$time[data_clean$Code==c] ) 
      colnames(time_series)<-c
      time_series_m<- cbind(time_series_m,time_series)
     }
    print("-I-Training ....")
    ###########################
    # analysis
    nahead<-input$nahead
    p.max=input$p.max
    q.max=input$q.max
    d.max=input$d.max
    #variables for accumulating data over multiple incidents
      fdata_full <- NULL
      mdata_full <- NULL
      mb_list <- NULL
      dnn_list <- NULL
      p_nn_list <- NULL
      error_df <- NULL
      cross_v_df_m <- NULL
      cross_v_df_m_avg_m <- NULL
    #iterate over all the incidents 
    code_list<- unique(data_clean$Code)
    for (code in code_list[1:5]){  
      print(code)
      tdata<-time_series_m[,code]
      tdata<-log(tdata)
      next_n_index<- seq(as.Date(max(index(tdata))),length.out=nahead+1 , by="1 months")
      next_n_index<- next_n_index[-1]
      
      #cross val
      #input<-list(nnsize=25,maxiter=150)
      for (j in 5:0){
      tdata_val<- tdata[1:(nrow(tdata)-(nahead+j))]
      #Create arima model and do validation 
      #to see how the model predicted and actual results will look like 
      #forecast is doen for last nahead value 
      #this data is shown in plot and tables 
      mb<-arma.model.selection(tdata_val,p.max,d.max, q.max)
      model.arima<- mb$best.model
      ar_predv<-forecast(model.arima, h=nahead)
      
      #NN model
      
      fit <- nnetar(tdata_val,size=input$nnsize, maxit=input$maxiter,p=input$nnp)
      p_nn_v<-forecast(fit, h=nahead)
      p_nn_fitted_v_ts<-xts(p_nn_v$fitted,order.by = index(tdata_val) )
      p_nn_forecast_v_ts<- xts(p_nn_v$mean,order.by =index(tdata[(nrow(tdata)+1-(nahead+j)):(nrow(tdata)-j)] ) )
      
      
      
      #Hybrid model concept 
      #arima good in capturing linear effect
      #so create arima model , residue from arima model will have nonlinear component
      #nn is good in capturing nonlinear effect 
      # create nn model of residues 
      #sum of arima and nn give final results
      
      res <- residuals(model.arima)
      res_ts<- xts(res,order.by = index(tdata_val))
      fit <- nnetar(res_ts,size=input$nnsize, maxit=input$maxiter,p=input$nnp)
      d_nn<-forecast(fit, h=nahead)
      
      
      hybrid_prediction<- ar_predv$mean+d_nn$mean
      
      #for reference 
      #print( sqrt(mean((hybrid_prediction[1:nahead]-tdata[(nrow(tdata)+1-nahead):nrow(tdata)])^2)))
      #print( sqrt(mean((ar_predv$mean[1:nahead]-tdata[(nrow(tdata)+1-nahead):nrow(tdata)])^2)))
      
      pr_v_h<-xts(hybrid_prediction,order.by = index(tdata[(nrow(tdata)+1-(nahead+j)):(nrow(tdata)-j)]))
      colnames(pr_v_h)<- "hybrid_validation_results"
      pr_v_a<-xts(ar_predv$mean,order.by = index(tdata[(nrow(tdata)+1-(nahead+j)):(nrow(tdata)-j)]))
      colnames(pr_v_a)<- "arima_validation_results"
      
      hybrid_residues_v<-pr_v_h-tdata[(nrow(tdata)+1-(nahead+j)):(nrow(tdata)-j)]
      hybrid_residues_v_rms<- sqrt(mean(hybrid_residues_v^2,na.rm=TRUE))
      hybrid_residues_fitted_v_rms<- sqrt(mean(d_nn$residuals^2,na.rm=TRUE))
      
      cross_v_df <- data.frame(code,j,val_rms=hybrid_residues_v_rms,fit_rms=hybrid_residues_fitted_v_rms)
      cross_v_df_m<- rbind(cross_v_df_m,cross_v_df)
      } 
      cross_v_df_m_avg_m<-data.frame(summarise(group_by(cross_v_df_m,code),avg_val_rms=mean(val_rms),avg_fitt_rms=mean(fit_rms)))
      
      ###############################
      #run on full data set and create forecast..steps same as above 
      #differernce we use full data set and perfom forecast
      
      #arima
      mb<-arma.model.selection(tdata,p.max,d.max, q.max)
      mb$code<-code
      mb_list<- c(mb_list,mb)
      model.arima<- mb$best.model
      ar_pred<-forecast(model.arima, h=nahead)
      
      err<-data.frame(accuracy(ar_pred))
      err$incident<-code
      err$model<-"ARIMA"
      row.names(err)<-NULL
      error_df<- rbind(error_df,err)
      
      res <- residuals(model.arima)
      res_ts<- xts(res,order.by = index(tdata))
      colnames(res_ts)<- "arima_residuals"
     
      #nn 
      fit <- nnetar(tdata,size=input$nnsize, maxit=input$maxiter,p=input$nnp)
      p_nn<-forecast(fit, h=nahead)
      
      p_nnm<-list(code=code,model=p_nn)
      p_nn_list<- c(p_nn_list,p_nnm)
      
      err<-data.frame(accuracy(p_nn))
      err$incident<-code
      err$model<-"NN"
      row.names(err)<-NULL
      error_df<- rbind(error_df,err)
      
      
      
      p_nn_fitted_ts<-xts(p_nn$fitted,order.by = index(tdata) )
      colnames(p_nn_fitted_ts)<- "nn_fitted"
      p_nn_residuals_ts<-xts(p_nn$residuals,order.by = index(tdata) )
      colnames(p_nn_residuals_ts)<- "nn_residuals"
      p_nn_forecast_ts<- xts(p_nn$mean,order.by =next_n_index )
      colnames(p_nn_forecast_ts)<- "nn_forecast"
      
      #hybrid
      fit <- nnetar(res_ts,size=input$nnsize, maxit=input$maxiter,p=input$nnp)
      d_nn<-forecast(fit, h=nahead)
      d_nnm<-list(code=code,model=d_nn)
      dnn_list<- c(dnn_list,d_nnm)
      
      err<-data.frame(accuracy(d_nn))
      err$incident<-code
      err$model<-"Hybrid ARIMA and NN"
      row.names(err)<-NULL
      error_df<- rbind(error_df,err)
      
      
      dnn_res <- residuals(d_nn)
      dnn_res_ts<- xts(dnn_res,order.by = index(tdata))
      colnames(dnn_res_ts)<- "hybrid_nn_residuals"
      dnn_fitted_ts<- xts(d_nn$fitted,order.by = index(tdata))
      colnames(dnn_fitted_ts)<- "hybrid_nn_fitted_nn"
      
      
      hybrid_prediction<- ar_pred$mean+d_nn$mean
      h_fitted<- ar_pred$fitted+d_nn$fitted
      h_fitted_ts<- xts(h_fitted,order.by = index(tdata))
      colnames(h_fitted_ts)<- "hybrid_fitted"
      
      
      
      pr_f<-xts(hybrid_prediction,order.by = next_n_index)
      colnames(pr_f)<- "hybrid_forecast"
      
      ar_fited<-xts(ar_pred$fitted,order.by = index(tdata))
      colnames(ar_fited)<- "arima_fitted"
      rr_f<-xts(ar_pred$mean,order.by = next_n_index)
      colnames(rr_f)<- "arima_forecast"
      #store forecast data to variable 
      fdata<-data.frame(date=next_n_index,hybrid_forecast=round(hybrid_prediction),ARIMA_forecast=round(ar_pred$mean),NN_forecast=round(p_nn$mean),
                        arima_val_results=round(pr_v_a),nn_val_results=round(p_nn_forecast_v_ts),hybrid_val_results=round(pr_v_h))
      
      fdata$code <- code
      rownames(fdata) <- NULL
      #accumulate across all the incidents into onevariable
      fdata_full<- bind_rows(fdata_full, fdata)
      h_residuals<- tdata-h_fitted_ts
      colnames(h_residuals)<- "hybrid_residuals"
      #store model  data to variable 
      mdata<-cbind(tdata,ar_fited)
      mdata<-cbind(mdata,dnn_fitted_ts)
       mdata<-cbind(mdata,h_fitted_ts)
      mdata<-cbind(mdata,p_nn_fitted_ts)
      
      mdata<-cbind(mdata,res_ts)
      mdata<-cbind(mdata,p_nn_residuals_ts)
      mdata<-cbind(mdata,dnn_res_ts)
      mdata<-cbind(mdata,h_residuals)
      mdata<-round(mdata)
      mdata<-as.data.frame(mdata)
      mdata$code <- code
      mdata$date<- index(tdata)
      colnames(mdata)[1]<-"incident_count"
      #accumulate across all the incidents into onevariable
      mdata_full<- bind_rows(mdata_full,mdata)
    }
    if(input$save|!file.exists("log_run.csv")){
      print("-I- Updatign saved files")  
      write.csv(mdata_full,"log_run.csv")
      write.csv(fdata_full,"forecast.csv")
      write.csv(error_df[,c("model","incident","RMSE","MAE")],"errors.csv")
      write.csv(cross_v_df_m_avg_m,"cross_validation_results.csv")
      write.csv(cross_v_df_m,"cross_validation_detail_log.csv")
      #save the best models 
      save(dnn_list,mb_list,p_nn_list,file=model_file)
    }
}

# Histogram Reaction function

run_hist <- function(hour_range,date_range) {
  filter(serverdata,datetimehourly>date_range[1] & datetimehourly<date_range[2]) %>% 
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
  # read_table_data <- function(){
  #   data<-read.csv("data.csv")
  #   return(unique(data$Code))
  # }
  #  output$ui <- renderUI({
  #                          if(!file.exists("data.csv")){
  #                            print("-E- Input file data.csv !!! Not found in directory")
  #                            flush.console()
  #                            return()
  #                          } 
  #                        code_list<-read_table_data()
  #                       selectInput("incident", "incident",
  #                                   choices = code_list,
  #                                   selected = code_list[1]
  #                       )})
  # 
  # output$plot_main <- renderPlot({ 
  #                                   #if(!file.exists("log_run.csv")){
  #                                    # print("-I- First Run , Training model")
  #                                     #analyse_all_incidents()
  #                                     #plot_hybrid_forecast() 
  #                                    # return()
  #                                   #} else {
  #                                    # print("-I- A model already exist , Dispalying data from that.")
  #                                   #plot_hybrid_forecast() 
  #                                   #}
  #                                     })
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
