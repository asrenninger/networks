####################################
## Query functions
####################################

library(tidyverse)
library(glue)
library(bigrquery)
library(sf)

## load project information

projectid <- jsonlite::fromJSON("secrets/musa-509-75492c1cf2ae.json")$project_id

## a function to extract all county fips codes from a metro area based on a key word

get_codes <- function(key_word) {
  
  kword <- str_to_lower(key_word)
  metro <- 
    read_csv("https://raw.githubusercontent.com/asrenninger/networks/master/data/metrolist.csv", n_max = 1159, col_names = FALSE) %>%
    set_names(c('metro_fips', 'metro_name', 'county_fips', 'county_name')) 
  
  codes <- 
    metro %>%
    filter(str_detect(str_to_lower(metro_name), kword)) %>%
    pull(county_fips) %>%
    glue_collapse(sep = "\', \'")
  
  tuple <- paste("\'", codes, "\'", sep = "")
  
  return(tuple)
  
}

## a function to get nodes for an block group-block group network

get_nodes <- function(fips) {
  
  query <- glue("SELECT geo_id as cbg, 
                 ST_X(ST_CENTROID(blockgroup_geom)) as X,
                 ST_Y(ST_CENTROID(blockgroup_geom)) as Y,
               FROM \`bigquery-public-data.geo_census_blockgroups.us_blockgroups_national\`
               WHERE SUBSTR(lpad(geo_id, 12, \'0\'), 0, 5) IN ({{fips}})",
                .open = '{{', .close = '}}')
  
  df <- bq_project_query(projectid, query)
  return(bq_table_download(df))
  
}

## a function to get origin-destination paths in long format

get_edges <- function(fips, month, cbgs) {
  
  query <- glue("SELECT poi_cbg, home_cbg, sum(visits) as visits
               FROM (SELECT 
                      lpad(CAST(poi_cbg AS STRING), 12, \'0\') as poi_cbg, 
                      REGEXP_EXTRACT(unnested, \'(.*?):\') as home_cbg, 
                      CAST(REGEXP_EXTRACT(unnested, \':(.*)\') AS NUMERIC) as visits
               FROM \`{{projectid}}.safegraph.2020_{{month}}\`
               CROSS JOIN UNNEST(SPLIT(regexp_replace(REPLACE(REPLACE(visitor_home_cbgs, \'{\', \'\'), \'}\', \'\'), \'\"\', \'\'))) as unnested
               WHERE SUBSTR(lpad(CAST(poi_cbg AS STRING), 12, \'0\'), 0, 5) IN ({{fips}}) AND visitor_home_cbgs != \'{}\')
               GROUP BY poi_cbg, home_cbg", 
                .open = '{{', .close = '}}')
  
  df <- bq_project_query(projectid, query)
  df <- bq_table_download(df)
  
  df <- df %>%
    rename(focal = poi_cbg,
           target = home_cbg,
           weight = visits) %>%
    filter(target %in% cbgs) %>%
    filter(weight > 10)
  
  return(df)
  
}

##



