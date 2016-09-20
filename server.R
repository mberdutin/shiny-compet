
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(RPostgreSQL)
library(dplyr)
library(ggplot2)
library(scales)
library(viridis)
library(ggthemes)
library(gridExtra)
library(lubridate)

get_data <- function(brand, date) {
  mydb <- dbConnect(pg, dbname="max", user="max", password="docker", host="10.12.0.104", port=5432)
  on.exit({ 
    dbDisconnect(mydb)
    cat('disconnect ')})
  query <- paste0("
                  select subbrands_list, banner_network, site, site_category, type, date, count(*) n_formats  
                  from bannerdays
                  where date between date'", date[1], "' and date'", date[2], "'", "
                    and subbrands_list not like '%NOKIA%' and lower(subbrands_list) like '%", tolower(brand), "%'
                  group by subbrands_list, banner_network, site, site_category, type, date
                  ")
  return(dbGetQuery(mydb, query))
}
plot_brand <- function(data, site_order, top_net) {
  placement <- data %>%
    group_by(banner_network) %>%
    mutate(strength = n()) %>%
    ungroup() %>%
    mutate(week = week(date)) %>%
    mutate(rank = dense_rank(desc(strength))) %>%
    filter(rank <= top_net) %>%
    left_join(site_SH)
  lev_banner <- placement %>% distinct(banner_network, rank) %>% arrange(rank)
  lev_site   <- placement %>% distinct(site) %>% arrange(desc(site))
  lev_category   <- placement %>% group_by(site_category) %>% mutate(m = mean(n_formats)) %>% distinct(site, m) %>% arrange(m, site_category)
  lev_SH   <- placement %>% group_by(SH) %>% mutate(m = mean(n_formats)) %>% distinct(site, m) %>% arrange(m, SH)
  exp_grid <- expand.grid(banner_network = unique(placement$banner_network), 
                          site = unique(placement$site), 
                          date = seq(min(placement$date), max(placement$date), 'days'), N = 0, stringsAsFactors = F) %>% 
    inner_join(site_category) %>%
    left_join(site_SH)
  
  placement_expand <- placement %>%
    full_join(exp_grid) %>%
    mutate(n_formats = ifelse(is.na(n_formats), N, n_formats)) %>%
    mutate(banner_network = factor(banner_network, levels = lev_banner$banner_network)) %>%
    mutate(site_f = factor(site, levels = lev_site$site)) %>%
    mutate(type_fl = type == 'network')

  if (site_order == 2) placement_expand$site_f <- factor(placement_expand$site, levels = lev_SH$site)
  if (site_order == 3) placement_expand$site_f <- factor(placement_expand$site, levels = lev_category$site)
  
  ggplot(placement_expand, aes(x = date, y = site_f)) +
    geom_tile(color = 'black', size = 0.01, aes(fill = n_formats)) +
    scale_fill_gradient(low = "white", high = "blue") +
    facet_wrap(~ banner_network, ncol = length(unique(placement$banner_network))) + 
    coord_equal() +
    geom_point(aes(size = ifelse(type_fl, 'network', 'other'))) +
    scale_size_manual(values=c(network = 0.7, other = NA), guide="none") +
    theme_tufte() + 
    theme(plot.title = element_text(hjust = 0), axis.ticks = element_blank()) + 
    labs(x = "date", y = "site", title = paste0("TOP", top_net, " networks, ", nrow(placement), " bannerdays"))
}

pg <- dbDriver("PostgreSQL")
mydb <- dbConnect(pg, dbname="max", user="max", password="docker", host="10.12.0.104", port=5432)
site_SH <- dbGetQuery(mydb, 'select * from "site_SH"')
site_category <- dbGetQuery(mydb, 'select * from "site_category"')
dbDisconnect(mydb)

shinyServer(function(input, output, session) {
  withProgress(message = 'Retrieving data',
               detail = 'one second...', value = 0, {
                 dataInput <- reactive({ get_data(input$text, input$dates) })
                 # widthInput <- reactive({ 100 + 25 * input$top_net * (week(ymd(input$dates[2])) - week(ymd(input$dates[1]))) })
               })
  
  output$subbrands_list <- renderPrint({ 
    data <- dataInput() %>% group_by(subbrands_list) %>% summarize(n = n()) %>% arrange(desc(n)) %>% select(subbrands_list)
    Encoding(data$subbrands_list) <- 'UTF-8' 
    data$subbrands_list
    })
  output$subbrands_head <- renderText ({ "Ploted subbrands:" })

  output$map <- renderPlot({
    plot_brand(data = dataInput(), site_order = input$checkGroup, top_net = input$top_net)
    
  }, width = 1200, height = 1000)
  session$onSessionEnded(function() {
    dbDisconnect(mydb)
    cat('disconnect ')
    })
})


