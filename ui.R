
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  titlePanel("Banner network map"),
  textInput("text", label = h3("Text input"), value = "TOYOTA"),
  hr(),

  
  fluidRow(
    column(4, h5(textOutput("map_head"), align = "center"), plotOutput("map"))
  )
))
