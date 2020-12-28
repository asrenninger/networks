library(glue)
library(tidyverse)
library(sf)
library(bigrquery)
library(tmap)
library(tmaptools)

tmap_mode("view")

projectid <- jsonlite::fromJSON("secrets/musa-509-75492c1cf2ae.json") %>% magrittr::extract("project_id")
  
##

metro <- 
  read_csv("https://raw.githubusercontent.com/asrenninger/networks/master/data/metrolist.csv", n_max = 1159, col_names = FALSE) %>%
  set_names(c('metro_fips', 'metro_name', 'county_fips', 'county_name')) 

codes <- 
  metro %>%
  filter(str_detect(str_to_lower(metro_name), "philadelphia")) %>%
  pull(county_fips) %>%
  glue_collapse(sep = "\', \'")

tuple <- paste("\'", codes, "\'", sep = "")

##

query <- glue("SELECT * 
               FROM \`tidal-digit-291220.safegraph.places\` as p
               JOIN (SELECT geo_id as cbg, 
                        blockgroup_geom as geometry,
                     FROM \`bigquery-public-data.geo_census_blockgroups.us_blockgroups_national\`
                     WHERE SUBSTR(lpad(geo_id, 12, \'0\'), 0, 5) IN ({{tuple}})) as b
               ON ST_Intersects(b.geometry, ST_GEOGPOINT(p.longitude, p.latitude))", .open = '{{', .close = '}}')

df <- bq_project_query(projectid, query)

bq_table_download(df) %>%
  filter(longitude != 0 & latitude != 0) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(3857) %>% 
  select(location_name, city) %>%
  plot()

##

month <- "04"

##

query <- glue("SELECT poi_cbg, home_cbg, sum(visits) as visits
               FROM (SELECT 
                      lpad(CAST(poi_cbg AS STRING), 12, \'0\') as poi_cbg, 
                      REGEXP_EXTRACT(unnested, \'(.*?):\') as home_cbg, 
                      CAST(REGEXP_EXTRACT(unnested, \':(.*)\') AS NUMERIC) as visits
               FROM \`{{projectid}}.safegraph.2020_{{month}}\`
               CROSS JOIN UNNEST(SPLIT(regexp_replace(REPLACE(REPLACE(visitor_home_cbgs, \'{\', \'\'), \'}\', \'\'), \'\"\', \'\'))) as unnested
               WHERE SUBSTR(lpad(CAST(poi_cbg AS STRING), 12, \'0\'), 0, 5) IN ({{tuple}}) AND visitor_home_cbgs != \'{}\')
               GROUP BY poi_cbg, home_cbg", 
              .open = '{{', .close = '}}')

df <- bq_project_query(projectid, query)
bq_table_download(df)

query <- glue("SELECT geo_id as cbg, 
                 ST_X(ST_CENTROID(blockgroup_geom)) as X,
                 ST_Y(ST_CENTROID(blockgroup_geom)) as Y,
               FROM \`bigquery-public-data.geo_census_blockgroups.us_blockgroups_national\`
               WHERE SUBSTR(lpad(geo_id, 12, \'0\'), 0, 5) IN ({{tuple}})",
              .open = '{{', .close = '}}')

df <- bq_project_query(projectid, query)
bq_table_download(df) %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326) %>% 
  st_transform(3702) %>% 
  plot()

query <- glue("SELECT geo_id as cbg, 
                 blockgroup_geom as geometry,
               FROM \`bigquery-public-data.geo_census_blockgroups.us_blockgroups_national\`
               WHERE SUBSTR(lpad(geo_id, 12, \'0\'), 0, 5) IN ({{tuple}})",
              .open = '{{', .close = '}}')

df <- bq_project_query(projectid, query)
bq_table_download(df) %>%
  st_as_sf() %>%
  st_transform(3702) %>% 
  plot()
  
