####################################
## Query functions
####################################

library(tidyverse)
library(glue)
library(bigrquery)
library(sf)

## load project information
projectid <- jsonlite::fromJSON("secrets/spatial-interaction-project-b528921f271f.json")$project_id

## creating an list of year-month combinations to download
ranger <- function(start_date, end_date){
  
  range <- seq.Date(as.Date(paste0(start_date, "-01")), as.Date(paste0(end_date, "-01")), by = "month")
  return(gsub("-", "_", substr(range, 1, 7)))
  
}

## a function to extract all county fips codes from a metro area based on a key word
search_codes <- function(key_word) {
   
  kword <- str_to_lower(key_word)
  metro <- read_csv("https://raw.githubusercontent.com/asrenninger/networks/master/data/metrolist_two.csv")
  
  codes <- 
    metro %>%
    filter(str_detect(str_to_lower(metro_name), kword)) %>%
    pull(county_fips) %>%
    glue_collapse(sep = "\', \'")
  
  tuple <- paste("\'", codes, "\'", sep = "")
  
  return(tuple)

}

get_codes <- function(metro_area) {
  
  kword <- str_to_lower(metro_area)
  metro <- read_csv("https://raw.githubusercontent.com/asrenninger/networks/master/data/metrolist_two.csv")
  
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

## a function for getting time series data
get_trends <- function(fips, month) {
  
  query <- glue("SELECT safegraph_place_id, location_name, top_category, sub_category, visits,
                   DATE(year, month, RANK() OVER(PARTITION BY safegraph_place_id ORDER BY index)) AS date,
                 FROM (SELECT safegraph_place_id, location_name, 
                         CAST(unnested AS NUMERIC) AS visits,
                         EXTRACT(year FROM date_range_start) AS year,
                         EXTRACT(month FROM date_range_start) AS month,
                         ROW_NUMBER() OVER(ORDER BY safegraph_place_id) AS index
                      FROM \`{{projectid}}.safegraph.2020_{{month}}`
                      CROSS JOIN UNNEST(SPLIT(REPLACE(REPLACE(visits_by_day, \'[\', \'\'), \']\', \'\'))) AS unnested
                      WHERE SUBSTR(lpad(CAST(poi_cbg AS STRING), 12, \'0\'), 0, 5) IN ({{fips}})) AS m
                JOIN (SELECT safegraph_place_id AS join_id, top_category, sub_category
                      FROM \`{{projectid}}.safegraph.places\`) AS p
                ON m.safegraph_place_id = p.join_id", 
                .open = '{{', .close = '}}')
  
  df <- bq_project_query(projectid, query)
  df <- bq_table_download(df)
  
  return(df)
  
}

## a function for getting time series data with a where clause
search_trends <- function(fips, month, category, clause) {
  
  clause <- str_to_title(clause)
  
  query <- glue("SELECT safegraph_place_id, location_name, top_category, sub_category, visits,
                   DATE(year, month, RANK() OVER(PARTITION BY safegraph_place_id ORDER BY index)) AS date,
                 FROM (SELECT safegraph_place_id, location_name, 
                         CAST(unnested AS NUMERIC) AS visits,
                         EXTRACT(year FROM date_range_start) AS year,
                         EXTRACT(month FROM date_range_start) AS month,
                         ROW_NUMBER() OVER(ORDER BY safegraph_place_id) AS index
                      FROM \`{{projectid}}.safegraph.{{month}}\`
                      CROSS JOIN UNNEST(SPLIT(REPLACE(REPLACE(visits_by_day, \'[\', \'\'), \']\', \'\'))) AS unnested
                      WHERE SUBSTR(lpad(CAST(poi_cbg AS STRING), 12, \'0\'), 0, 5) IN ({{fips}})) AS m
                JOIN (SELECT safegraph_place_id AS join_id, top_category, sub_category
                      FROM \`{{projectid}}.safegraph.places\`) AS p
                ON m.safegraph_place_id = p.join_id
                WHERE REGEXP_CONTAINS({{category}}, \'{{clause}}\')", 
                .open = '{{', .close = '}}')
  
  df <- bq_project_query(projectid, query)
  df <- bq_table_download(df)
  
  return(df)
  
}

## a function for getting points of interest by geography
get_pois <- function(fips, month) {
 
  query <- glue("SELECT safegraph_place_id, location_name, brand, top_category, sub_category, poi_cbg, latitude, longitude
                FROM (SELECT
                  safegraph_place_id,
                  location_name,
                  lpad(CAST(poi_cbg AS STRING), 12, \'0\') as poi_cbg
                FROM \`{{projectid}}.safegraph.2020_{{month}}\`
                WHERE SUBSTR(lpad(CAST(poi_cbg AS STRING), 12, \'0\'), 0, 5) IN ({{fips}})) as m 
                JOIN (SELECT safegraph_place_id AS join_id, brands AS brand, top_category, sub_category, latitude, longitude
                      FROM \`{{projectid}}.safegraph.places\`) AS p
                ON m.safegraph_place_id = p.join_id",
                .open = '{{', .close = '}}')
  
  df <- bq_project_query(projectid, query)
  df <- bq_table_download(df)
  
  return(df)
   
}

## a function for getting points of interest by geography
get_bipartite <- function(fips, month, cbgs, min) {
  
  query <- glue("SELECT poi_id, poi_cbg, home_cbg, visits,top_category, sub_category, latitude, longitude,
                 FROM (SELECT 
                        safegraph_place_id as poi_id,
                        lpad(CAST(poi_cbg AS STRING), 12, \'0\') as poi_cbg, 
                        REGEXP_EXTRACT(unnested, \'(.*?):\') as home_cbg, 
                        CAST(REGEXP_EXTRACT(unnested, \':(.*)\') AS NUMERIC) as visits
                FROM \`{{projectid}}.safegraph.2020_{{month}}\`
                CROSS JOIN UNNEST(SPLIT(regexp_replace(REPLACE(REPLACE(visitor_home_cbgs, \'{\', \'\'), \'}\', \'\'), \'\"\', \'\'))) as unnested
                WHERE SUBSTR(lpad(CAST(poi_cbg AS STRING), 12, \'0\'), 0, 5) IN ({{fips}}) AND visitor_home_cbgs != \'{}\')
                JOIN (SELECT safegraph_place_id AS join_id, top_category, sub_category, latitude, longitude
                      FROM \`{{projectid}}.safegraph.places\`) AS p
                ON poi_id = p.join_id", 
                .open = '{{', .close = '}}')
  
  df <- bq_project_query(projectid, query)
  df <- bq_table_download(df)
  
  df <- df %>%
    filter(home_cbg %in% cbgs) %>%
    filter(visits > min)
  
  return(df)
  
}

## bespoke for spatial interaction modelling 
get_interactions <- function(fips, month, category, keyword, cbgs, min) {
  
  query <- glue("SELECT poi_cbg, home_cbg, sum(visits) AS visits
                 FROM (SELECT poi_id, poi_cbg, home_cbg, visits, top_category, sub_category, latitude, longitude,
                       FROM (SELECT 
                             safegraph_place_id as poi_id,
                             lpad(CAST(poi_cbg AS STRING), 12, \'0\') as poi_cbg, 
                             REGEXP_EXTRACT(unnested, \'(.*?):\') as home_cbg, 
                             CAST(REGEXP_EXTRACT(unnested, \':(.*)\') AS NUMERIC) as visits
                             FROM \`{{projectid}}.safegraph.{{month}}\`
                             CROSS JOIN UNNEST(SPLIT(regexp_replace(REPLACE(REPLACE(visitor_home_cbgs, \'{\', \'\'), \'}\', \'\'), \'\"\', \'\'))) as unnested
                             WHERE SUBSTR(lpad(CAST(poi_cbg AS STRING), 12, \'0\'), 0, 5) IN ({{fips}}) AND visitor_home_cbgs != \'{}\')
                             JOIN (SELECT safegraph_place_id AS join_id, top_category, sub_category, latitude, longitude
                             FROM \`{{projectid}}.safegraph.places\`) AS p
                             ON poi_id = p.join_id) as b
                  WHERE REGEXP_CONTAINS({{catorgy}}, \'{{keyword}}\')
                  GROUP BY poi_cbg, home_cbg", 
                .open = '{{', .close = '}}')
  
  df <- bq_project_query(projectid, query)
  df <- bq_table_download(df)
  
  df <- df %>%
    filter(home_cbg %in% cbgs) %>%
    filter(visits > min)
  
  return(df)
  
}

## income and racial differences
get_profiles <- function(fips, month, category, clause) {
  
  clause <- str_to_title(clause)
  
  query <- glue("SELECT safegraph_place_id, location_name, latitude, longitude, top_category, sub_category, 
                       weighted_average, weighted_median, pct_white, pct_black, pct_hispanic, pct_other,
                FROM (SELECT safegraph_place_id,
                      SUM(median_income * visits) / SUM(visits) as weighted_median,
                      SUM(mean_income * visits) / SUM(visits) as weighted_average,
                      SUM(white_pop * visits) / (SUM(total_pop * visits) + 1) as pct_white,
                      SUM(black_pop * visits) / (SUM(total_pop * visits) + 1) as pct_black,
                      SUM(hispanic_pop * visits) / (SUM(total_pop * visits) + 1) as pct_hispanic,
                      SUM(other_pop * visits) / (SUM(total_pop * visits) + 1) as pct_other,
                FROM (SELECT safegraph_place_id, home_cbg, visits
                      FROM (SELECT safegraph_place_id, poi_cbg, REGEXP_EXTRACT(unnested, \'(.*?):\') AS home_cbg, SAFE_CAST(REGEXP_EXTRACT(unnested, \':(.*)\') AS NUMERIC) AS visits
                            FROM `{{projectid}}.safegraph.{{month}}`
                            CROSS JOIN
                            UNNEST(SPLIT(REGEXP_REPLACE(REPLACE(REPLACE(visitor_home_cbgs, \'{\', \'\'), \'}\', \'\'), \'\"\', \'\'))) AS unnested
                            WHERE SUBSTR(lpad(CAST(poi_cbg AS STRING), 12, \'0\'), 0, 5) IN ({{fips}}) AND visitor_home_cbgs != \'{}\')) as p
                      join (SELECT SUBSTR(lpad(CAST(geo_id AS STRING), 12,\'0\'), 0, 5) as county, geo_id as cbg, 
                                   total_pop, income_per_capita as mean_income, median_income, income_per_capita * total_pop as aggregate_income,
                                   black_pop, white_pop, hispanic_pop, total_pop - black_pop - white_pop - hispanic_pop as other_pop,
                            FROM \`bigquery-public-data.census_bureau_acs.blockgroup_2018_5yr\`) as c
                      ON p.home_cbg = c.cbg
                GROUP BY safegraph_place_id) i
                JOIN (SELECT safegraph_place_id AS join_id, location_name, top_category, sub_category, latitude, longitude 
                      FROM \`{{projectid}}.safegraph.places\`) AS p
                ON i.safegraph_place_id = p.join_id
                WHERE REGEXP_CONTAINS({{category}}, \'{{clause}}\')", 
                .open = '{{', .close = '}}')
  
  print(query)
  
  df <- bq_project_query(projectid, query)
  df <- bq_table_download(df)
  
  df <- mutate(df, 
               year = parse_number(str_sub(month, 1, 4)),
               month = lubridate::month(parse_number(str_sub(month, 6, 7)), label = TRUE, abbr = FALSE),
               GEOID = fips)
  
  return(df)
  
}

