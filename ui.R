
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
      column(4, textInput("text", label = h3("Subbrand input"), value = "TOYOTA")),
      column(2, radioButtons("checkGroup", label = h3("Site sorting"), 
                             choices = c('URL' = 1, 'Sales House' = 2, 'Category' = 3),
                             selected = 1,
                             inline = T)),
      column(3, numericInput("top_net", label = h3("Number of networks"), value = 5)),
      column(3, 
             dateRangeInput("dates", label = h3("Date range"), min = "2015-01-01", max = "2016-08-31", 
                                                              start = '2016-01-01', end = '2016-08-31'))
    ))),

  fluidRow(
    column(width = 4, h5(textOutput("subbrands_head"), align = "center"), verbatimTextOutput("subbrands_list"), align = "left"),
    column(width = 8, plotOutput("map"))
  )
))
