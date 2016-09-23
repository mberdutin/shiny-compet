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

pg <- dbDriver("PostgreSQL")
mydb <- dbConnect(pg, dbname="max", user="max", password="docker", host="10.12.0.104", port=5432)
site_SH <- dbGetQuery(mydb, 'select * from "site_SH"')
site_category <- dbGetQuery(mydb, 'select * from "site_category"')
dbDisconnect(mydb)

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
  data <- dbGetQuery(mydb, query)
  Encoding(data$subbrands_list) <- 'UTF-8' 
  return(data)
}
plot_brand <- function(data, top_net, top_sub, category) {
  placement <- data %>%
    group_by(subbrands_list) %>%
    mutate(strength_sub = n()) %>%
    ungroup() %>%
    mutate(rank_sub = dense_rank(desc(strength_sub))) %>%
    filter(rank_sub <= top_sub) %>%
    group_by(banner_network) %>%
    mutate(strength = n()) %>%
    rowwise() %>% 
    mutate(site_net = paste(site, banner_network, sep = ', ')) %>%
    ungroup() %>%
    mutate(week = week(date)) %>%
    mutate(rank = dense_rank(desc(strength))) %>%
    filter(rank <= top_net)
  lev_banner <- placement %>% distinct(banner_network, rank) %>% arrange(rank)
  lev_site_net   <- placement %>% distinct(site_net) %>% arrange(desc(site_net))
  lev_category   <- placement %>% group_by(site_category) %>% mutate(m = mean(n_formats)) %>% distinct(site, m) %>% arrange(m, site_category)
  lev_sub <- placement %>% distinct(subbrands_list, rank_sub) %>% arrange(rank_sub)
  date_range <- seq(min(placement$date), max(placement$date), 'days')
  
  one_sub <- function(subbrand) {
    placement <- placement %>% filter(subbrands_list == subbrand)
    exp_grid <- expand.grid(subbrands_list = unique(placement$subbrands_list),
                            site_net = unique(placement$site_net), 
                            date = date_range, N = 0, stringsAsFactors = F) %>% 
      mutate(site = substr(stri_extract_first_regex(site_net, '^.*,'), 1, nchar(stri_extract_first_regex(site_net, '^.*,')) - 1)) %>%
      inner_join(site_category) 
    
    placement_expand <- placement %>%
      full_join(exp_grid) %>%
      mutate(n_formats = ifelse(is.na(n_formats), N, n_formats)) %>%
      mutate(site_f = factor(site_net, levels = lev_site_net$site_net)) %>%
      mutate(type_fl = type == 'network') %>%
      filter(!is.na(site_f))
    if (tolower(category) != 'all') placement_expand <- filter(placement_expand, stri_detect_regex(site_category, gsub(', ', '|', tolower(category))))
    
    
    gg <- ggplot(placement_expand, aes(x = date, y = site_f)) +
      geom_tile(color = 'black', size = 0.007, aes(fill = n_formats)) +
      scale_fill_gradient(low = "white", high = "blue") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank()) +
      # facet_wrap(~ site_category, nrow = length(unique(placement$site_category)), dir = 'v', switch = 'y', scales = 'free') +
      # coord_equal() +
      labs(x = NULL, y = NULL, title = paste0(subbrand, ', ', nrow(placement), " banner-weeks")) #+
      # geom_point(aes(size = ifelse(type_fl, 'network', 'other'))) +
      # scale_size_manual(values=c(network = 0.7, other = NA), guide="none") #+
      # theme_tufte() +
      # theme(title = element_text(size = 11), plot.title = element_text(hjust = 0), axis.ticks = element_blank(), legend.position = "none")
    ggplotGrob(gg)
  }
  cclist <- lapply(lev_sub$subbrands_list, one_sub)
  cclist[["size"]] <- 'first'
  g <- do.call(rbind, cclist)
  grid.newpage()
  grid.draw(g)
}

data <- get_data('toyota', date = c('2016-01-01', '2016-01-31')) 
top_net <- 3
top_sub <- 1
category <- 'auto, news'

plot_brand(data, top_net, top_sub, category)
