#function.R wrapper function for getting and reciving data from Sercver.R 
#some dummy data to start the develepement cycle on ui.R and server.R
#everyline stores data for one hour
serverdata <- read.csv("../anaheiminput/data.csv")
serverdata$datetimehourly <- as.POSIXlt(test$datetimehourly, tz="GMT",format="%m/%d/%y %H:%M ")



#recive a date and time from server.R 


#send back the following values to the server.R for rendering

