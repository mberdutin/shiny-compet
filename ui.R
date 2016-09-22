
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
      # get
      column(2, textInput("text", label = h5("Subbrand (e.g. kia -nokia -аврора)"), value = "toyota"), actionButton("go", "Plot")),
      column(2, dateRangeInput("dates", label = h5("Date range"), min = "2015-01-01", max = "2016-08-31", 
                               start = '2016-01-01', end = '2016-08-31')),
      # filter
      column(2, textInput("category", label = h5("Site category (e.g. auto, news)"), value = "all")),
      column(1, numericInput("top_net", label = h5("Top networks"), value = 10)),
      column(1, numericInput("top_sub", label = h5("Top subbrands"), value = 10)),
      column(1, numericInput("clean", label = h5("Min days per row"), value = 3)),
      # plot
      column(1, textInput("format", label = h5("Plot's format"), value = '___x___'), checkboxInput("auto_format", label = "Auto format", value = TRUE)),
      column(2, radioButtons("radio", label = h5("Facet to plot"),
                             choices = list("Subbrands ~ site + network" = 1, "Site ~ subbrands + networks" = 2, "Network ~ subbrands" = 3), 
                             selected = 1))

    ))),

  fluidRow(
    column(width = 12, plotOutput("map"))
  )
))
