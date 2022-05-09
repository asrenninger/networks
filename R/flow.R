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

## go... 
map(rev(city_list), ~get_metrics(.x))

## the workflow
get_metrics <- function(city){
  
  print(city)
  start <- Sys.time()
  ## getting fips codes, for immediate bay area
  codes <- get_codes(city)
  
  ## get node data
  try(get_nodes(codes))
  
  nodes <- get_nodes(codes)
  print(dim(nodes))
  
  ## create index
  index <- ranger("2020-01", "2020-12")
  
  ## get edge data
  edges <-
    map_df(index[1:12], function(x) { 
      get_interactions(fips = codes, month = x, category = "top_category", keyword = "grocery store", cbgs = nodes$cbg, min = 5) %>% 
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
  statistics <- map(ready, ~get_statistics(.x, nodes, census))
  correlations <- get_correlations(ready, nodes)
  
  colnames(correlations) <- ym
  rownames(correlations) <- ym
  
  ## finish
  local <- map_df(statistics, ~pluck(.x, 1))
  global <- map_df(statistics, ~pluck(.x, 2))
  
  ## nmi
  partitions <- map(statistics, ~pluck(.x, 3))
  
  nmi <- 
    map_dfr(1:12, 
            function(x) {
              
              map_dfr(1:12, 
                      function(y){
                        
                        tibble(month_source = ym[x], month_target = ym[y], 
                               nmi = igraph::compare(partitions[[x]], partitions[[y]], method = 'nmi'))
                        
                      })
              
            })
  
  correlations %>%
    as_tibble() %>%
    rownames_to_column(var = "month_source") %>%
    mutate(month_source = ym) %>%
    pivot_longer(!month_source, names_to = "month_target", values_to = "correlation") %>%
    left_join(nmi) %>%
    write_csv(glue("data/processed/2022_05/grocers/correlations/{city}.csv"))
  
  segregation_variables <-
    census %>%
    transmute(GEOID,
              nonwhite = population - white,
              white = white,
              nonrich = lower,
              rich = upper)
  
  dissimilarity <- get_dissimilarity(local, segregation_variables, periods = ym)
  
  communities <- 
    local %>%
    left_join(census) %>%
    select(GEOID, period, infomap, population) %>%
    group_by(period, infomap) %>%
    summarise(n = n(), 
              population = sum(population)) %>%
    ungroup() %>%
    group_by(period) %>%
    summarise(community_n = mean(n),
              community_population = max(population),
              community_n_denom = sum(n),
              community_population_denom = sum(population)) %>%
    ungroup() %>%
    transmute(period, 
              community_size = community_n / community_n_denom,
              community_concentration = community_population / community_population_denom)
  
  k <- 
    local %>%
    group_by(period) %>%
    summarise(k_mean = mean(k_all, na.rm = TRUE),
              k_sum = sum(k_all, na.rm = TRUE),
              k_skew = e1071::kurtosis(k_all, na.rm = TRUE),
              k_balance = mean(degree_balance, na.rm = TRUE)) %>%
    ungroup()
  
  global %>% 
    left_join(dissimilarity) %>%
    mutate(race_baseline = MLID::id(as.data.frame(segregation_variables), vars = c("nonwhite", "white")) %>% magrittr::extract2(1),
           income_baseline = MLID::id(as.data.frame(segregation_variables), vars = c("nonrich", "rich")) %>% magrittr::extract2(1)) %>%
    left_join(communities) %>%
    left_join(k) %>%
    glimpse() %>%
    write_csv(glue("data/processed/2022_05/grocers/global/{city}.csv"))
  
  local %>%
    write_csv(glue("data/processed/2022_05/grocers/local/{city}.csv"))
  
  end <- Sys.time()
  print(end - start)
  
}
