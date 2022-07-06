###################################
## SCA
###################################

## packages
source("R/package.R")
source("R/help.R")
source("R/rehelp.R")
source("R/query.R")

## load in cities
city_list <- 
  read_csv("data/metrolist_two.csv") %>%
  drop_na(metro_name) %>%
  filter(rank > 0, rank < 102) %>%
  pull(metro_name) %>%
  unique()

## select city
city <- city_list[7]

## fips codes
codes <- get_codes(city)

## clusters
clusters <- get_clustering(codes, "top_category", "restaurant", 500, 5)

clusters %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  select(cluster_num) %>% 
  plot()

clusters%>%
  drop_na() %>%
  group_by(cluster_num) %>%
  summarise(n = n()) %>%
  pull(n) %>%
  hist(breaks = 50)

## concentration
p <- 
  clusters%>%
  drop_na() %>%
  group_by(cluster_num) %>%
  summarise(n = n()) %>%
  ungroup() %>% 
  mutate(total = sum(n),
         clusters = n()) %>%
  mutate(p = n / total) %>%
  pull(p)
  
# 5.59
sei <- -sum(p * log(p))





