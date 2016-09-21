
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
      column(2, textInput("text", label = h5("Subbrand input"), value = "TOYOTA")),
      column(2, textInput("category", label = h5("Site category (comma separated)"), value = "ALL")),
      column(2, radioButtons("radio", label = h5("Facet to plot"),
                             choices = list("Subbrands_list ~ site_net" = 1, "Network ~ subbrands_list" = 2, "Site ~ subbrands_list" = 3), 
                             selected = 1)),
      column(1, textInput("format", label = h5("Format"), value = '2000x4000')),
      column(1, numericInput("top_net", label = h5("Top networks"), value = 3)),
      column(1, numericInput("top_sub", label = h5("Top subbrands"), value = 10)),
      column(2, 
             dateRangeInput("dates", label = h5("Date range"), min = "2015-01-01", max = "2016-08-31", 
                                                              start = '2016-01-01', end = '2016-08-31'))
    ))),

  fluidRow(
    column(width = 12, plotOutput("map"))
  )
))
