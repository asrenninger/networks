## TfL API experiments
library(tidyverse)
library(httr)
library(sf)

# base
base <- "https://api.tfl.gov.uk"

# query
modes <- "/Road/All/Status"
dates <- "?startDate=2022-04-01&endDate=2022-04-30"
types <- "&format=json"
query <- paste0(base, modes, dates)

status <-  
  httr::GET(query) %>%
  magrittr::use_series("content") %>% 
  rawToChar() %>%
  jsonlite::fromJSON() %>%
  glimpse() %>%
  mutate(envelope = purrr::map(envelope, ~jsonlite::fromJSON(.x)),
       envelope = purrr::map(envelope, ~st_polygonize(st_linestring(.x)))) %>%
  rename(geometry = envelope) %>%
  st_as_sf(crs = 4326)

# see if it worked
mapview::mapview(status, zcol = "statusSeverity")

# getting idividual roadworks
roadworks <- disruptions$url[1]

get_polygons <- function(roadworks){
  
  coordinates <- matrix(c(roadworks$geometry$coordinates[[1]][, , 1], roadworks$geometry$coordinates[[1]][, , 2]), ncol = 2)
  lines <- 
    st_linestring(coordinates) %>%
    st_polygonize()
  
  return(lines)
  
}

modes <- "/Road/All/Disruption"
dates <- "?startDate=2022-04-01&endDate=2022-04-30"
types <- "&format=json"
query <- paste0(base, modes, dates)

disruptions <- 
  httr::GET(query) %>%
  magrittr::use_series("content") %>% 
  rawToChar() %>%
  jsonlite::fromJSON() %>% 
  glimpse() 

url <- disruptions$url[1]

query <- paste0(base, url, dates, types)

testing <- 
  httr::GET(query) %>%
  magrittr::use_series("content") %>% 
  rawToChar() %>%
  jsonlite::fromJSON() %>% 
  glimpse() 

poly <- get_polygons(testing)

# see if it worked
mapview::mapview(poly)

