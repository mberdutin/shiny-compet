
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
library(grid)
library(gtable)
library(lubridate)
library(stringi)

get_data <- function(brand, date) {
  cat('get\n')
  mydb <- dbConnect(pg, dbname="max", user="max", password="docker", host="10.12.0.104", port=5432)
  on.exit({ 
    dbDisconnect(mydb)
    cat('disconnect ')})
  brand <- tolower(brand)
  if (stri_detect_regex(brand, ' -')) {
    brand_incl <- substr(brand, 1, stri_locate_first_fixed(brand, ' -') - 1)
    brand_excl <- substr(brand, stri_locate_first_fixed(brand, ' -') + 2, nchar(brand))
    query <- paste0("
                  select subbrands_list, banner_network, site, site_category, type, date, count(*) n_formats  
                  from bannerdays
                  where date between date'", date[1], "' and date'", date[2], "'", "
                    and lower(subbrands_list) not similar to '%(", paste(strsplit(brand_excl, ' -')[[1]], collapse = '|'), ")%' and lower(subbrands_list) like '%", brand_incl, "%'
                  group by subbrands_list, banner_network, site, site_category, type, date
                  ")
  } else {
    query <- paste0("
                  select subbrands_list, banner_network, site, site_category, type, date, count(*) n_formats  
                  from bannerdays
                  where date between date'", date[1], "' and date'", date[2], "'", "
                    and lower(subbrands_list) like '%", brand, "%'
                  group by subbrands_list, banner_network, site, site_category, type, date
                  ")
  }
  
  data <- dbGetQuery(mydb, query)
  Encoding(data$subbrands_list) <- 'UTF-8' 
  return(data)
}
filter_data <- function(data, top_net, top_sub, category, clean) {
  cat('filter\n')
  placement <- data %>%
    mutate(subbrands_list = substr(subbrands_list, 1, 40)) %>%
    mutate(banner_network = gsub('googlesyndication', 'googles', banner_network)) %>%
    group_by(subbrands_list) %>%
    mutate(strength_sub = n()) %>%
    ungroup() %>%
    mutate(rank_sub = dense_rank(desc(strength_sub))) %>%
    filter(rank_sub <= top_sub) %>%
    group_by(banner_network) %>%
    mutate(strength = n()) %>%
    rowwise() %>% 
    mutate(site_net = paste(site, banner_network, sep = ', ')) %>%
    mutate(sub_net = paste(subbrands_list, banner_network, sep = ', ')) %>%
    ungroup() %>%
    mutate(week = week(date)) %>%
    mutate(rank = dense_rank(desc(strength))) %>%
    filter(rank <= top_net)
    
  if (tolower(category) != 'all') placement <- filter(placement, stri_detect_regex(site_category, gsub(', ', '|', tolower(category))))
  if (clean > 0) placement <- placement %>% group_by(subbrands_list, banner_network, site) %>% filter(length(unique(date)) > clean) %>% ungroup()
  placement
}
plot_brand <- function(placement, plot_type) {
  cat('plot\n')
  lev_banner <- placement %>% distinct(banner_network, rank) %>% arrange(rank)
  lev_site_net   <- placement %>% distinct(site_net) %>% arrange(desc(site_net))
  lev_sub_net   <- placement %>% distinct(sub_net) %>% arrange(desc(sub_net))
  lev_category   <- placement %>% group_by(site_category) %>% mutate(m = mean(n_formats)) %>% distinct(site, m) %>% arrange(m, site_category)
  lev_sub <- placement %>% distinct(subbrands_list, rank_sub) %>% arrange(rank_sub)
  date_range <- seq(min(placement$date), max(placement$date), 'days')
  
  plot_sub <- function() {
    one_sub <- function(subbrand) {
      placement <- placement %>% filter(subbrands_list == subbrand)
      exp_grid <- expand.grid(site_net = unique(placement$site_net), 
                              date = date_range, N = 0, stringsAsFactors = F) 
      
      placement_expand <- placement %>%
        full_join(exp_grid) %>%
        mutate(n_formats = ifelse(is.na(n_formats), N, n_formats)) %>%
        mutate(site_f = factor(site_net, levels = lev_site_net$site_net)) %>%
        mutate(type_fl = type == 'network') %>%
        filter(!is.na(site_f))
      
      gg <- ggplot(placement_expand, aes(x = date, y = site_f)) +
        geom_tile(color = 'black', size = 0.007, aes(fill = n_formats)) +
        scale_fill_gradient(low = "white", high = "blue") +
        coord_equal() +
        labs(x = NULL, y = NULL, title = paste0(subbrand, ', ', nrow(placement), " formatdays")) +
        geom_point(aes(size = ifelse(type_fl, 'network', 'other'))) +
        scale_size_manual(values=c(network = 0.7, other = NA), guide="none") +
        scale_x_date(date_breaks = "2 weeks", expand=c(0,0)) +
        theme_tufte() +
        theme(title = element_text(size = 11), plot.title = element_text(hjust = 0), axis.ticks = element_blank(), legend.position = "none") +
        theme(panel.border = element_blank())
      ggplotGrob(gg)
      
    }
    cclist <- lapply(lev_sub$subbrands_list, one_sub)
    cclist[["size"]] <- 'first'
    g <- do.call(rbind, cclist)
    grid.newpage()
    grid.draw(g)
  }
  plot_site <- function() {
    one_site <- function(my_site) {
      placement <- placement %>% filter(site == my_site)
      exp_grid <- expand.grid(sub_net = unique(placement$sub_net), 
                              date = date_range, N = 0, stringsAsFactors = F) 
      
      placement_expand <- placement %>%
        full_join(exp_grid) %>%
        mutate(n_formats = ifelse(is.na(n_formats), N, n_formats)) %>%
        mutate(sub_net_f = factor(sub_net, levels = lev_sub_net$sub_net)) %>%
        mutate(type_fl = type == 'network') %>%
        filter(!is.na(sub_net_f))
      
      gg <- ggplot(placement_expand, aes(x = date, y = sub_net_f)) +
        geom_tile(color = 'black', size = 0.007, aes(fill = n_formats)) +
        scale_fill_gradient(low = "white", high = "blue") +
        coord_equal() +
        labs(x = NULL, y = NULL, title = paste0(my_site, ', ', nrow(placement), " formatdays")) +
        geom_point(aes(size = ifelse(type_fl, 'network', 'other'))) +
        scale_size_manual(values=c(network = 0.7, other = NA), guide="none") +
        scale_x_date(date_breaks = "2 weeks", expand=c(0,0)) +
        theme_tufte() +
        theme(title = element_text(size = 11), plot.title = element_text(hjust = 0), axis.ticks = element_blank(), legend.position = "none") +
        theme(panel.border = element_blank())
      ggplotGrob(gg)
      
    }
    cclist <- lapply(unique(placement$site), one_site)
    cclist[["size"]] <- 'first'
    g <- do.call(rbind, cclist)
    grid.newpage()
    grid.draw(g)
  }
  plot_net <- function() {
    one_net <- function(network) {
      placement <- placement %>% filter(banner_network == network) %>% group_by(subbrands_list, date) %>% summarise(n_siteformats = sum(n_formats))
      exp_grid <- expand.grid(subbrands_list = unique(placement$subbrands_list),
                              date = date_range, N = 0, stringsAsFactors = F) 
      
      placement_expand <- placement %>%
        full_join(exp_grid) %>%
        mutate(n_siteformats = ifelse(is.na(n_siteformats), N, n_siteformats)) 

      gg <- ggplot(placement_expand, aes(x = date, y = subbrands_list)) +
        geom_tile(color = 'black', size = 0.007, aes(fill = n_siteformats)) +
        scale_fill_gradient(low = "white", high = "blue") +
        coord_equal() +
        labs(x = NULL, y = NULL, title = paste0(network, ', ', sum(placement$n_siteformats), " siteformats")) +
        theme_tufte() +
        scale_x_date(date_breaks = '2 weeks', expand=c(0,0)) +
        theme(title = element_text(size = 11), plot.title = element_text(hjust = 0), axis.ticks = element_blank(), legend.position = "none") #+
        theme(panel.border = element_blank())
      ggplotGrob(gg)
      
    }
    cclist <- lapply(unique(placement$banner_network), one_net)
    cclist[["size"]] <- 'first'
    g <- do.call(rbind, cclist)
    grid.newpage()
    grid.draw(g)
  }
  switch(as.integer(plot_type), plot_sub(), plot_site(), plot_net())

}

pg <- dbDriver("PostgreSQL")

shinyServer(function(input, output) {
  v <- reactiveValues(doPlot = FALSE)
  observeEvent(input$go, {
    # 0 will be coerced to FALSE
    # 1+ will be coerced to TRUE
    v$doPlot <- input$go
  })

  dataInput <- reactive({ get_data(input$text, input$dates) })
  filteredInput <- reactive({ filter_data(dataInput(), input$top_net, input$top_sub, input$category, input$clean) })


  output$map <- renderPlot({ 
    if (v$doPlot == FALSE) return()
    isolate({plot_brand(filteredInput(), plot_type = input$radio) })
    }, 
      width = function() { as.integer(strsplit(input$format, 'x')[[1]][1]) }, height = function() { as.integer(strsplit(input$format, 'x')[[1]][2])})

})


