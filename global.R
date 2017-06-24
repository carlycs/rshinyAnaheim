library(zoo)
library(ggplot2)
library(xts)
library(forecast)
library(RSNNS)
library(dplyr)
library(readr)
library(lubridate)
library(plotly)
library(RColorBrewer)

#function.R wrapper function for getting and receiving data from server.R 
#some dummy data to start the develepement cycle on ui.R and server.R
#everyline stores data for one hour
serverData <- read_csv("data.csv")

serverData$datetimehourly <- mdy_hm(serverData$datetimehourly, tz = "UTC")
serverData$datetimehourly <- as.POSIXct(serverData$datetimehourly)
serverData$yearMonthDay <- as.POSIXct(format(serverData$datetimehourly, format = "%Y-%m-%d"))
serverData$hour <- format(serverData$datetimehourly, format = "%H")

scalePalette <- brewer.pal(6,"RdYlGn")
scalePalette <- scalePalette[6:1]

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


# Login Related:
userAuthDatabase <- data.frame(UserId=c("Hoofar", "User"),
                               Password=c("Manager", "User"),
                               Roles=c("Manager", "User"))

validateLogin <- function(userId, password){
  stopifnot(exists("userAuthDatabase"))
  authResult <- dplyr::filter(userAuthDatabase, UserId == userId & Password == password)
  if(nrow(authResult) == 1){
    # Successful login
    with(
      authResult,
      list(login=T, user = as.character(userId), role = as.character(Roles))
    )
  }
  else {
    # Unsuccessful login
    list(login=F, user = NULL, role = NULL)
  }
}

composeLoginModal <- function(...)
  # generate a modal with the inputs for a login a well as initialization
  # and password recovery links
{
  showModal(
    modalDialog(
      id        = "loginmodal"
      , size      = 's'
      , easyClose = FALSE 
      , div(
        id = "modal-contents"
        , textInput('login_user', 'Login')
        , passwordInput('login_passwd', 'Password')
        , div(...)
        , actionButton(
          inputId = 'login_button'
          , label   = 'Login'
          , class   = 'btn action-button btn-success'
          , icon    = icon('sign-in')
        ) #/ login-button
      ) #/ modal-contents
      # , footer = div(id = "modal-footer" 
      #                , a(id = "forgot-login-link"
      #                    , href = forgot_password_message
      #                    , p("Forgot Password", style = "display: inline;")
      #                )
      #                , HTML("&bull;")
      #                , a(id = "request-login-link"
      #                    , href = request_login_message
      #                    , p("Request Login", style = "display: inline;")
      #                )
      # ) #/ modal-footer
    ) #/ modalDialog
  ) #/ showModal
}

showConfirmModal <- function(id, ...) {
  showModal(
    modalDialog(
      id        = sprintf("%s-confirm-modal", id)
      , size      = 's'
      , easyClose = TRUE
      , div(...)
      , div(style = "text-align: right"
            , actionButton(sprintf("%s_ok", id), "OK", icon = icon("check"), style = "display: inline;")
            , actionButton(sprintf("%s_cancel", id), "Cancel", icon = icon("times"), style = "display: inline;")
      )
      , footer = NULL
    ) #/ modalDialog
  ) #/ showModal
}

