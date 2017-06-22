library(zoo)
library(ggplot2)
library(xts)
library("forecast")
library(RSNNS)
library(dplyr)
require(readr)
library(lubridate)

#function.R wrapper function for getting and reciving data from Sercver.R 
#some dummy data to start the develepement cycle on ui.R and server.R
#everyline stores data for one hour
serverdata <- read_csv("data.csv")
serverdata$datetimehourly <- mdy_hm(serverdata$datetimehourly, tz="GMT")





#recive a date and time from server.R 


#send back the following values to the server.R for rendering

