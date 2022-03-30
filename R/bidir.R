####################################
## BiDir Distance
## 1 - F(A, Pb) / F(A, Pa)
####################################

city_list <- 
  read_csv("data/metrolist_two.csv") %>%
  drop_na(metro_name) %>%
  filter(rank > 0, rank < 102) %>%
  pull(metro_name) %>%
  unique()

metrics <- map_df(city_list, ~get_bidir(.x))

## wrap it up
get_bidir <- function(metro){
  
  tictoc::tic()
  
  # get fips codes
  codes <- get_codes(metro)
  
  # get node data
  nodes <- get_nodes(codes)
  
  # create index
  index <- ranger("2020-01", "2021-08")
  
  # get edge data
  edges <- map_df(index, function(x) { 
    get_edges(codes, x, nodes$cbg) %>% 
      mutate(year = parse_number(str_sub(x, 1, 4)),
             month = lubridate::month(parse_number(str_sub(x, 6, 7)), label = TRUE, abbr = FALSE)) 
  })
  
  ym <- 
    edges %>% 
    mutate(period = glue("{month}, {year}")) %>% 
    pull(period) %>% 
    unique()
  
  edges <- mutate(edges, period = factor(glue("{month}, {year}"), levels = ym))
  
  # a list of graphs
  ready <- 
    edges %>% 
    group_by(period) %>% 
    group_split()
  
  # partitions
  partitions <- map(ready, ~get_partitions(.x, nodes))
  
  # qualities 
  qualities <- map_df(ready, ~get_quality(.x, nodes, partitions))
  
  # bidir 
  denominators <-
    qualities %>%
    filter(a == b) %>%
    transmute(a, 
              denominator = q_louvain)
  
  distances <- 
    qualities %>%
    left_join(denominators) %>%
    mutate(d_ab = 1 - (q_louvain/ denominator))
  
  left_join(distances %>% 
              filter(a == "January, 2020"),
            distances %>% 
              filter(b == "January, 2020") %>%
              transmute(b = a,
                        d_ba = d_ab)) %>%
    select(-q_louvain, -denominator) %>%
    mutate(city = metro) %>%
    write_csv(glue("data/processed/bidir/bidir_{metro}.csv"))
  
  tictoc::toc()
  
}

## get a list of partitions
get_partitions <-
  function(edges, nodes){
    
    temp_edges <- 
      edges %>%
      transmute(from = target,
                to = focal, 
                weight) 
    
    temp_nodes <- transmute(nodes, cbg)
    
    graph <- 
      temp_edges %>%
      graph_from_data_frame(vertices = select(temp_nodes, cbg), directed = TRUE) %>%
      set_edge_attr("weight", value = temp_edges$weight) 
    
    graph <- simplify(graph)
    
    louvain_clusters <- igraph::cluster_louvain(as.undirected(graph), weights = E(graph)$weight)
    
    partitions <- 
      tibble(GEOID = V(graph)$name,
             louvain = louvain_clusters$membership,
             period = edges$period[1])
    
    return(partitions)
    
  }

## get a list of qualities by partition
get_quality <-
  function(edges, nodes, partitions){
    
    temp_edges <- 
      edges %>%
      transmute(from = target,
                to = focal, 
                weight) 
    
    temp_nodes <- transmute(nodes, cbg)
    
    graph <- 
      temp_edges %>%
      graph_from_data_frame(vertices = select(temp_nodes, cbg), directed = TRUE) %>%
      set_edge_attr("weight", value = temp_edges$weight) 
    
    graph <- simplify(graph)
    
    qualities <-
      map_df(partitions, function(x){
        
        quality <-
          tibble(a = edges$period[1],
                 b = x$period[1], 
                 q_louvain = modularity(as.undirected(graph), x$louvain, weights = E(graph)$weight))
        
        return(quality)
        
      })
    
    return(qualities)
    
  }


