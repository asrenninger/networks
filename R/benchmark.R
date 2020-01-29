library(tigris)
library(sf)

options(tigris_use_cache = TRUE)

##

roads <- roads("PA", "Philadelphia", class = 'sf')

##

library(tidyverse)

##

roads_local <- 
  roads %>% 
  filter(str_detect(MTFCC, "S1200|S1400")) %>%
  filter(str_detect(RTTYP, "M|C"))

## Method 1 

start <- Sys.time()

difference <- st_difference(roads_local)

end <- Sys.time()

diff_1 <- end - start

## Method 2

start <- Sys.time()

duplicates <- 
  roads_local %>% 
  st_equals() %>%
  as_tibble() %>%
  rename(original = row.id,
         duplicate = col.id) %>%
  group_by(original) %>%
  mutate(grouping = max(duplicate)) %>%
  ungroup() %>%
  distinct(grouping)

difference <- 
  roads_local %>% 
  slice(duplicates$grouping)

containers <- 
  difference %>% 
  st_contains() %>%
  as_tibble() %>%
  rename(original = row.id,
         duplicate = col.id) %>%
  filter(original != duplicate) 

difference <-
  difference %>%
  slice(-unique(containers$duplicate))

end <- Sys.time()

diff_2 <- end - start

##

diff_1
diff_2

## 1: 48 seconds | 2: 5.4 seconds


