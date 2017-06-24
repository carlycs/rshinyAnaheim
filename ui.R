library(shiny)

source("ui.main.R")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  uiOutput("mainUI")
))

