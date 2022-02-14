## preprocess
left_join(
  vroom::vroom("data/census/data/cbg_b02.csv") %>%
    select(census_block_group, B02001e1:B02001e5),
  vroom::vroom("data/census/data/cbg_b03.csv")  %>% 
    select(census_block_group, B03002e1, B03002e2)) %>%
  write_csv("data/census/data/cbg_b02xb03.csv")

vroom::vroom("data/census/data/cbg_b19.csv") %>% 
  select(census_block_group, B19301e1, B19301m1,
         B19101e1, B19101e2, B19101e3, B19101e4, B19101e5, B19101e6, B19101e7, B19101e8, B19101e9, 
         B19101e10, B19101e11, B19101e12, B19101e13, B19101e14, B19101e15, B19101e16, B19101e17) %>%
  write_csv("data/census/data/cbg_b19x.csv")

## get all metros
city_list <- 
  read_csv("data/metrolist_two.csv") %>%
  drop_na(metro_name) %>%
  filter(rank > 0, rank < 102) %>%
  pull(metro_name) %>%
  unique()

tictoc::tic()
## get fips codes
codes <- get_codes(city_list[5])

## get node data
nodes <- get_nodes(codes)

## create index
index <- ranger("2020-01", "2021-08")

## get edge data
edges <- map_df(index, function(x) { 
  get_edges(codes, x, nodes$cbg) %>% 
    mutate(year = parse_number(str_sub(x, 1, 4)),
           month = lubridate::month(parse_number(str_sub(x, 6, 7)), label = TRUE, abbr = FALSE)) 
})

## clean edge data
ym <- 
  edges %>% 
  mutate(period = glue("{month}, {year}")) %>% 
  pull(period) %>% 
  unique()

edges <- mutate(edges, period = factor(glue("{month}, {year}"), levels = ym))

## get covariates
census <- get_census(nodes)
distance <- get_distance(edges, nodes)

ready <- 
  edges %>% 
  mutate(distance = distance$distance) %>%
  group_by(period) %>% 
  group_split()

## statistics
library(furrr)
plan(multisession, workers = 6)

statistics <- furrr::future_map(ready, ~get_statistics(.x, nodes, census))
correlation <- get_correlations(ready, nodes)

## finish
tictoc::toc()

local <- map_df(statistics, ~pluck(.x, 1))
global <- map_df(statistics, ~pluck(.x, 2))

