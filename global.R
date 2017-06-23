library(zoo)
library(ggplot2)
library(xts)
library(forecast)
library(RSNNS)
library(dplyr)
library(readr)
library(lubridate)
library(plotly)

#function.R wrapper function for getting and receiving data from server.R 
#some dummy data to start the develepement cycle on ui.R and server.R
#everyline stores data for one hour
serverData <- read_csv("data.csv")
serverData$datetimehourly <- mdy_hm(serverData$datetimehourly, tz = "UTC")
serverData$yearMonthDay <- as.POSIXct(format(serverData$datetimehourly, format = "%Y-%m-%d"))

set_time <- function() {
  this_hour <- hour(Sys.time())
  if(this_hour<21 && this_hour>2) {
    return(c(this_hour-3,this_hour+3))
  } else {
    if(this_hour>12) {
      return(c(this_hour-6,this_hour))
    } else {
      return(c(this_hour,this_hour+6))
    }
  }
}

#recive a date and time from server.R 


#send back the following values to the server.R for rendering

