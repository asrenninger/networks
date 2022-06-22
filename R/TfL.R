## TfL API experiments
install.packages("tidyverse")
install.packages("sf")
install.packages("httr")
install.packages("mapview")

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
roadworks <- status$url[1]

get_polygons <- function(roadworks){
  
  coordinates <- matrix(c(roadworks$geometry$coordinates[[1]][, , 1], roadworks$geometry$coordinates[[1]][, , 2]), ncol = 2)
  lines <- 
    st_linestring(coordinates) %>%
    st_polygonize() %>%
    st_sfc() %>% 
    st_set_crs(4326)
  
  return(lines)
  
}

get_polygons_2 <- function(roadworks){
  
  coordinates <- matrix(testing$geometry$coordinates[[1]][[1]], byrow = FALSE, ncol = 2)
  lines <- 
    st_linestring(coordinates) %>%
    st_polygonize() %>%
    st_sfc() %>% 
    st_set_crs(4326)
  
  return(lines)
  
}

modes <- "/Road/All/Disruption"
dates <- "?startDate=2021-04-01&endDate=2021-04-30"
types <- "&format=json"
query <- paste0(base, modes, dates)

disruptions <- 
  httr::GET(query) %>%
  magrittr::use_series("content") %>% 
  rawToChar() %>%
  jsonlite::fromJSON() %>% 
  glimpse() 

url <- disruptions$url[!str_detect(disruptions$url, "PWC")][8]

query <- paste0(base, url, dates, types)

testing <- 
  httr::GET(query) %>%
  magrittr::use_series("content") %>% 
  rawToChar() %>%
  jsonlite::fromJSON() %>% 
  glimpse() 

poly <- try(get_polygons(testing))

if(pluck(class(poly), 1) %in% "try-error") {
  
  poly <- get_polygons_2(testing)
  poly <- st_as_sf(poly)
  
}else{
  
  poly <- st_as_sf(poly)
  
}

# see if it worked
mapview::mapview(poly)

# whole city
all_poly <- 
  map_dfr(disruptions$url[!str_detect(disruptions$url, "PWC")],
          function(x){
            
            query <- paste0(base, x, dates, types)
            print(query)
            
            roadworks <- 
              httr::GET(query) %>%
              magrittr::use_series("content") %>% 
              rawToChar() %>%
              jsonlite::fromJSON()
            
            
            if(is.null(roadworks$geometry)) { 
              
              poly <- 
                st_point(testing$geography$coordinates[[1]]) %>%
                st_sfc() %>%
                st_set_crs(4326) %>%
                st_as_sf()
              
            }else{
              poly <- try(get_polygons(roadworks))
              
              if(pluck(class(poly), 1) %in% "try-error") {
                
                poly <- get_polygons_2(roadworks)
                poly <- st_as_sf(poly)
                
              }else{
                
                poly <- st_as_sf(poly)
                
              }
              
            }
            
            Sys.sleep(4)
            return(poly)
            
          })

# see if it worked... again
all_poly %>% 
  filter(st_geometry_type(x) != "POINT") %>% 
  mapview::mapview()

# different months? 
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

all_poly_2 <- 
  map_dfr(disruptions$url[!str_detect(disruptions$url, "PWC")],
          function(x){
            
            query <- paste0(base, x, dates, types)
            print(query)
            
            roadworks <- 
              httr::GET(query) %>%
              magrittr::use_series("content") %>% 
              rawToChar() %>%
              jsonlite::fromJSON()
            
            
            if(is.null(roadworks$geometry)) { 
              
              poly <- 
                st_point(testing$geography$coordinates[[1]]) %>%
                st_sfc() %>%
                st_set_crs(4326) %>%
                st_as_sf()
              
            }else{
              poly <- try(get_polygons(roadworks))
              
              if(pluck(class(poly), 1) %in% "try-error") {
                
                poly <- get_polygons_2(roadworks)
                poly <- st_as_sf(poly)
                
              }else{
                
                poly <- st_as_sf(poly)
                
              }
              
            }
            
            Sys.sleep(4)
            return(poly)
            
          })

# comparing to before... 
all_poly_2 %>% 
  filter(st_geometry_type(x) != "POINT") %>% 
  mapview::mapview()





