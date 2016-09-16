
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  titlePanel("Banner network map"),
  fluidRow(wellPanel(
    fluidRow(
      column(4, textInput("text", label = h3("Text input"), value = "TOYOTA")),
      column(4, radioButtons("checkGroup", label = h3("Site sorting"), 
                             choices = c('URL' = 1, 'Sales House' = 2, 'Category' = 3),
                             selected = 1,
                             inline = T)),
      column(4, 
             dateRangeInput("dates", label = h3("Date range"), min = "2015-01-01", max = "2016-08-31", 
                                                              start = '2016-06-01', end = '2016-08-31'))
    ))),
  
  hr(),

  
  fluidRow(
    column(4, h5(textOutput("map_head"), align = "center"), plotOutput("map"))
  )
))
