
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

plot_brand <- function(brand = 'TOYOTA', site_order = 1, top_net = 5, year = '2016', week1 = '30', week2 = '38') {
  mydb <- dbConnect(pg, dbname="max", user="max", password="docker", host="10.12.0.104", port=5432)
  on.exit(dbDisconnect(mydb))
  query <- paste0("
                 select distinct banner_network, site, site_category, week, type, ", '"Nsites" ', 
                 "from bannerdays 
                 where year in (", year, ") and week >= ", week1, " and week <= ", week2, " 
                 and subbrands_list not like '%NOKIA%'
                 and subbrands_list like '%", brand, "%'
                 ")
  placement <- dbGetQuery(mydb, query) %>%
    group_by(banner_network) %>%
    mutate(strength = n()) %>%
    ungroup() %>%
    mutate(rank = dense_rank(desc(strength))) %>%
    filter(rank <= top_net) %>%
    left_join(site_SH)
  lev_banner <- placement %>% distinct(banner_network, rank) %>% arrange(rank)
  lev_site   <- placement %>% distinct(site) %>% arrange(desc(site))
  lev_category   <- placement %>% group_by(site_category) %>% mutate(m = mean(Nsites)) %>% distinct(site, m) %>% arrange(m, site_category)
  lev_SH   <- placement %>% group_by(SH) %>% mutate(m = mean(Nsites)) %>% distinct(site, m) %>% arrange(m, SH)
  exp_grid <- expand.grid(banner_network = unique(placement$banner_network), 
                          site = unique(placement$site), 
                          week = unique(placement$week), N = 0, stringsAsFactors = F) %>% 
    inner_join(site_category) %>%
    left_join(site_SH)
  
  placement_expand <- placement %>%
    full_join(exp_grid) %>%
    mutate(Nsites = ifelse(is.na(Nsites), N, Nsites)) %>%
    mutate(banner_network = factor(banner_network, levels = lev_banner$banner_network)) %>%
    mutate(site_f = factor(site, levels = lev_site$site)) %>%
    mutate(type_fl = type == 'network')
  if (site_order == 2) placement_expand$site_f <- factor(placement_expand$site, levels = lev_SH$site)
  if (site_order == 3) placement_expand$site_f <- factor(placement_expand$site, levels = lev_category$site)
  
  ggplot(placement_expand, aes(x = week, y = site_f)) +
    geom_tile(color = 'black', size = 0.1, aes(fill = Nsites)) +
    scale_fill_gradient(low = "white", high = "blue") +
    facet_wrap(~ banner_network, ncol = length(unique(placement$banner_network))) + 
    coord_equal() +
    geom_point(aes(size = ifelse(type_fl, 'network', 'other'))) +
    scale_size_manual(values=c(network = 0.7, other = NA), guide="none") +
    theme_tufte() + 
    theme(plot.title = element_text(hjust = 0), axis.ticks = element_blank()) + 
    labs(x = "week per network", y = "site", title = paste0(brand, " placement in summer: TOP", top_net, " networks, ", nrow(placement), " banner-weeks"))
}

plot_subbrands <- function(brand = 'TOYOTA', site_order = 'URL', top_net = 5, top_sub = 2, to_file = FALSE) {
  query = paste0("
                 select distinct subbrands_list, banner_network, site, site_category, week, type, Nsites 
                 from bannerdays 
                 where year = 2016 and month in (6, 7, 8) 
                 and subbrands_list not like '%NOKIA%'
                 and subbrands_list like '%", brand, "%'
                 ")
  placement <- dbGetQuery(mydb, query) 
  for (i in names(placement)) if(class(placement[, i]) == 'character') Encoding(placement[, i]) <- 'UTF-8'
  placement <- placement %>%
    group_by(subbrands_list) %>%
    mutate(strength_sub = n()) %>%
    ungroup() %>%
    mutate(rank_sub = dense_rank(desc(strength_sub))) %>%
    filter(rank_sub <= top_sub) %>%
    group_by(banner_network) %>%
    mutate(strength_net = n()) %>%
    ungroup() %>% 
    mutate(rank_net = dense_rank(desc(strength_net))) %>%
    filter(rank_net <= top_net) %>%
    left_join(site_SH)
  lev_sub <- placement %>% distinct(subbrands_list, rank_sub) %>% arrange(rank_sub)
  lev_banner <- placement %>% distinct(banner_network, rank_net) %>% arrange(rank_net)
  lev_site   <- placement %>% distinct(site) %>% arrange(desc(site))
  lev_category   <- placement %>% group_by(site_category) %>% mutate(m = mean(Nsites)) %>% distinct(site, m) %>% arrange(m, site_category)
  lev_SH   <- placement %>% group_by(SH) %>% mutate(m = mean(Nsites)) %>% distinct(site, m) %>% arrange(m, SH)
  weeks <- unique(placement$week)
  measure_width  <- lev_banner %>% nrow() * 10
  measure_height <- placement %>% group_by(subbrands_list, site) %>% summarize(n = n()) %>% nrow() / 2 %>% floor() + 
    placement %>% group_by(subbrands_list) %>% summarize(n = n()) %>% nrow() * 3
  
  one_sub <- function(subbrand) {
    placement <- placement %>% filter(subbrands_list == subbrand)
    exp_grid <- expand.grid(banner_network = unique(placement$banner_network), 
                            site = unique(placement$site), 
                            week = weeks, N = 0, stringsAsFactors = F) %>% 
      inner_join(site_category) %>%
      left_join(site_SH)
    
    placement_expand <- placement %>%
      full_join(exp_grid) %>%
      mutate(Nsites = ifelse(is.na(Nsites), N, Nsites)) %>%
      mutate(banner_network = factor(banner_network, levels = lev_banner$banner_network)) %>%
      mutate(site_f = factor(site, levels = lev_site$site)) %>%
      mutate(type_fl = type == 'network')
    if (site_order == 'SH') placement_expand$site_f <- factor(placement_expand$site, levels = lev_SH$site)
    if (site_order == 'category') placement_expand$site_f <- factor(placement_expand$site, levels = lev_category$site)
    
    gg <- ggplot(placement_expand, aes(x = week, y = site_f)) +
      geom_tile(color="black", size = 0.1, aes(fill = Nsites)) +
      scale_fill_gradient(low = "white", high = "blue") +
      facet_wrap(~ banner_network, ncol = length(unique(placement$banner_network))) +
      coord_equal() +
      labs(x = NULL, y = NULL, title = paste0(subbrand, ', ', nrow(placement), " banner-weeks")) +
      geom_point(aes(size = ifelse(type_fl, 'network', 'other'))) +
      scale_size_manual(values = c(network = 0.7, other = NA), guide = "none") +
      scale_x_continuous(breaks = floor(quantile(weeks, names = F))) +
      theme_tufte() +
      theme(title = element_text(size = 11), plot.title = element_text(hjust = 0), axis.ticks = element_blank(), legend.position = "none") +
      theme(panel.border = element_blank(), panel.margin.x = unit(0.5, "cm"), panel.margin.y = unit(0.5, "cm"))
    gg
  }
  cclist <- lapply(lev_sub$subbrands_list, one_sub)
  cclist[["ncol"]] <- 1
  if (to_file) {
    g <- do.call(arrangeGrob, cclist)
    ggsave(paste0(brand, "_roll_", length(lev_sub$subbrands_list), ".png"), plot = g, width = measure_width, height = measure_height, units = "cm", limitsize = F)
  }
  else do.call(grid.arrange, cclist)
}

pg <- dbDriver("PostgreSQL")
mydb <- dbConnect(pg, dbname="max", user="max", password="docker", host="10.12.0.104", port=5432)
site_SH <- dbGetQuery(mydb, 'select * from "site_SH"')
site_category <- dbGetQuery(mydb, 'select * from "site_category"')
dbDisconnect(mydb)

shinyServer(function(input, output, session) {
  

  output$map <- renderPlot({
    start_date <- ymd(input$dates[1])
    end_date <- ymd(input$dates[2])
    year_str <- paste(year(start_date), year(end_date), sep = ', ')
    start_week <- as.character(week(start_date))
    end_week <- as.character(week(end_date))
    plot_brand(brand = input$text, site_order = input$checkGroup, top_net = 3, year = year_str, week1 = start_week, week2 = end_week)
    
  }, width = 2000, height = 1000)
  session$onSessionEnded(function() {
    dbDisconnect(mydb)
    cat('disconnect')
    })
})


