#################################################
## Let's do this...
#################################################
get_distance <-
  function(edges, nodes){
    
    distances <- od::od_to_sf(edges, 
                              nodes %>% 
                                st_as_sf(coords = c("X", "Y"), crs = 4326) %>% 
                                st_transform(4269)) %>% 
      st_transform(3857) %>%
      transmute(focal, target, period,
                distance = units::drop_units(units::set_units(st_length(geometry), km))) %>%
      st_drop_geometry()
    
    return(distances)
    
  }

get_census <-
  function(nodes){
    
    income <- 
      vroom::vroom("data/census/data/cbg_b19x.csv") %>% 
      filter(census_block_group %in% nodes$cbg) %>%
      select(census_block_group, B19301e1, B19301m1,
             B19101e1, B19101e2, B19101e3, B19101e4, B19101e5, B19101e6, B19101e7, B19101e8, B19101e9, 
             B19101e10, B19101e11, B19101e12, B19101e13, B19101e14, B19101e15, B19101e16, B19101e17) %>% 
      transmute(GEOID = census_block_group,
                median_income = B19301e1,
                lower_lower = B19101e2 + B19101e3 + B19101e4 + B19101e5, 
                lower_middle = B19101e6 + B19101e7 + B19101e8 + B19101e9, 
                middle = B19101e10 + B19101e11 + B19101e12, 
                upper_middle = B19101e13 + B19101e14, 
                upper_upper = B19101e14 + B19101e16 + B19101e16 + B19101e17) %>%
      mutate(lower = lower_lower + lower_middle,
             upper = upper_middle + upper_upper)
    
    population <- 
      vroom::vroom("data/census/data/cbg_b02xb03.csv") %>% 
      filter(census_block_group %in% nodes$cbg) %>%
      select(census_block_group, B02001e1:B02001e5, B03002e1, B03002e2) %>%
      transmute(GEOID = census_block_group, 
                pct_nonwhite = 1 - (B02001e2 / B02001e1),
                population = B02001e1,
                white = B02001e2,
                black = B02001e3,
                asian = B02001e5,
                hispanic = B03002e1 - B03002e2,
                other = B02001e1 - B02001e2 - B02001e3- B02001e5 - (B03002e1 - B03002e2)) %>%
      mutate(other = if_else(other < 0, 0, other))
    
    demographics <- left_join(population, income)
    
    return(demographics)
    
  }

get_statistics <- 
  function(edges, nodes, node_attributes){
    
    d_in <- 
      edges %>%
      filter(distance != 0) %>%
      group_by(GEOID = focal) %>% 
      summarise(d_in = mean(distance)) %>%
      ungroup() 
    
    d_out <- 
      edges %>%
      filter(distance != 0) %>%
      group_by(GEOID = target) %>% 
      summarise(d_out = mean(distance)) %>% 
      ungroup()
    
    weights <-
      left_join(edges %>%
                  group_by(GEOID = focal) %>%
                  summarise(in_weighted = sum(weight)) %>%
                  ungroup(),
                edges %>%
                  group_by(GEOID = target) %>%
                  summarise(out_weighted = sum(weight)) %>% 
                  ungroup()) %>%
      mutate(degree_balance = in_weighted - out_weighted)
    
    node_attributes <- 
      node_attributes %>%
      left_join(d_in) %>% 
      left_join(d_out) %>%
      left_join(weights) 
    
    temp_edges <- 
      edges %>%
      transmute(from = target,
                to = focal, 
                weight) 
    
    temp_nodes <-
      nodes %>% 
      transmute(cbg) %>% 
      left_join(rename(node_attributes, cbg = GEOID)) %>%
      replace_na(list(d_in = 0, d_out = 0, median_income = 0, pct_nonwhite = 0, 
                      out_weighted = 0, in_weighted = 0))
    
    graph <- 
      temp_edges %>%
      graph_from_data_frame(vertices = select(temp_nodes, cbg), directed = TRUE) %>%
      set_edge_attr("weight", value = temp_edges$weight) %>%
      set_vertex_attr("income", value = temp_nodes$median_income) %>%
      set_vertex_attr("race", value = temp_nodes$pct_nonwhite) %>%
      set_vertex_attr("d_in", value = temp_nodes$d_in) %>%
      set_vertex_attr("d_out", value = temp_nodes$d_out) %>%
      set_vertex_attr("in_weighted", value = temp_nodes$in_weighted) %>%
      set_vertex_attr("out_weighted", value = temp_nodes$out_weighted) 
    
    global <- 
      tibble(period = edges$period[1],
             density = igraph::graph.density(graph, loops = FALSE),
             transitivity = igraph::transitivity(graph),
             assortativity_d_in = igraph::assortativity(graph, V(graph)$d_in),
             assortativity_d_out = igraph::assortativity(graph, V(graph)$d_out),
             assortativity_degree = igraph::assortativity_degree(graph),
             assortativity_degree_null = igraph::assortativity_degree(get_null(graph)),
             assortativity_income = igraph::assortativity(graph, V(graph)$income),
             assortativity_income_null = igraph::assortativity(get_null(graph), V(graph)$race),
             assortativity_race = igraph::assortativity(graph, V(graph)$race),
             assortativity_race_null = igraph::assortativity(get_null(graph), V(graph)$race))
    
    graph <- simplify(graph)
    
    library(furrr)
    plan(multisession, workers = 6)

    infomap_runs <- future_map(1:6, ~igraph::cluster_infomap(graph, nb.trials = 10, e.weights = E(graph)$weight, modularity = TRUE))
    modularities <- map_dbl(infomap_runs, ~magrittr::use_series(.x, "modularity"))
    
    infomap_clusters <- infomap_runs[[which.max(modularities)]]
    
    # infomap_clusters <- igraph::cluster_infomap(graph, nb.trials = 10, e.weights = E(graph)$weight, modularity = TRUE)
    # leiden_clusters <- igraph::cluster_leiden(as.undirected(graph), resolution_parameter = 0.9, objective_function = 'CPM')

    local <- 
      tibble(GEOID = V(graph)$name,
             eigenvector = igraph::eigen_centrality(graph)$vector, 
             k_all = igraph::degree(graph, loops = FALSE),
             k_i = igraph::degree(graph, mode = "in", loops = FALSE),
             k_o = igraph::degree(graph, mode = "out", loops = FALSE),
             eccentricity_i = igraph::eccentricity(graph, mode = "in"),
             eccentricity_o = igraph::eccentricity(graph, mode = "out"),
             transitivity_l = igraph::transitivity(graph, type = "barrat"),
             infomap = infomap_clusters$membership) %>%
      left_join(weights) %>%
      mutate(period = edges$period[1])
   
    global <-
      global %>%
      mutate(L_i = infomap_clusters$codelength,
             Q_i = infomap_clusters$modularity,
             C_i = sum(crossing(infomap_clusters, graph)) / (length(crossing(infomap_clusters, graph)) - sum(crossing(infomap_clusters, graph))))
    
    return(list(local, global, infomap_clusters))
      
  }

get_correlations <- 
  function(edges, nodes){
    
    square <- array(dim = c(length(edges), nrow(nodes), nrow(nodes)))
    
    for (i in 1:length(edges)) {
      
      temp <- 
        edges[[i]] %>%
        transmute(from = target,
                  to = focal, 
                  weight) %>%
        filter(from %in% nodes$cbg) %>%
        arrange(to, from)
      
      adjacencies <- 
        temp %>%
        graph_from_data_frame(vertices = nodes, directed = FALSE) %>%
        set_edge_attr("weight", value = temp$weight) %>% 
        as_adjacency_matrix(attr = "weight") %>% 
        as.matrix()
      
      square[i, , ] <- adjacencies
      
    }
    
    tictoc::tic()
    correlations <- sna::gcor(square)
    tictoc::toc()
    
    return(correlations)
    
  }

get_null <- function(graph){
  
  configuration_model <-
    igraph::sample_degseq(out.deg = igraph::degree(graph, mode = "in", loops = TRUE),
                          in.deg = igraph::degree(graph, mode = "out", loops = TRUE)) %>%
    set_edge_attr("weight", value = sample(E(graph)$weight))

  return(configuration_model)
  
}

get_dissimilarity <-
  function(communities, node_attributes, periods = ym){
    
    race_split <- 
      communities %>% 
      select(GEOID, period, infomap) %>%
      left_join(node_attributes) %>%
      group_by(period, infomap) %>%
      summarise(white = sum(white),
                nonwhite = sum(nonwhite),
                rich = sum(rich),
                nonrich = sum(nonrich)) %>%
      ungroup() %>%
      group_by(period) %>%
      group_split()
    
    series <- 
      reduce(map(race_split, function(x){
        tibble(race = MLID::id(as.data.frame(x), vars = c("nonwhite", "white")) %>% magrittr::extract2(1),
               income = MLID::id(as.data.frame(x), vars = c("rich", "nonrich")) %>% magrittr::extract2(1))
      }), rbind)
    
    return(mutate(series, period = factor(periods, levels = periods)))
    
  }

get_concentration <- function(partitions){
  
  working <- select(partitions, -period)
  working <- as.matrix(working)
  
  rownames(working) <- x$period
  
  working[is.na(working)] <- 0
  
  return(diverse::diversity(na.omit(working), type = c('entropy', 'herfindahl-hirschman')))
  
}

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
    
    infomap_clusters <- igraph::cluster_infomap(graph, nb.trials = 10, e.weights = E(graph)$weight, modularity = FALSE)
    
    return(infomap_clusters)
    
  }

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
                 q_leiden = modularity(as.undirected(graph), x$leiden, weights = E(graph)$weight),
                 q_infomap = modularity(as.undirected(graph), x$infomap, weights = E(graph)$weight), 
                 q_louvain = modularity(as.undirected(graph), x$louvain, weights = E(graph)$weight))
        
        return(quality)
        
      })
    
    return(qualities)
    
  }

correlate <- function(correlations, name, legend_name) {
  
  mat <- round(correlations, 2)
  
  ##
  
  get_lower_tri <- function(cormat){
    cormat[upper.tri(cormat)] <- NA
    return(cormat)
  }
  
  get_upper_tri <- function(cormat){
    cormat[lower.tri(cormat)] <- NA
    return(cormat)
  }
  
  ##
  
  upper_tri <- get_upper_tri(mat)
  melted_mat <- reshape2::melt(upper_tri, na.rm = TRUE)
  
  ##
  
  ggheatmap <- 
    ggplot(data = na.omit(melted_mat), aes(Var2, Var1, fill = value)) +
    geom_tile(color = "white") +
    scico::scale_colour_scico(palette = 'hawaii', direction = -1,
                              guide = 'none') +
    scico::scale_fill_scico(palette = 'hawaii',
                            limit = c(0.7, 1), 
                            oob = scales::squish,
                            name = legend_name,
                            guide = guide_colorbar(direction = "vertical",
                                                   barheight = unit(50, units = "mm"),
                                                   barwidth = unit(2, units = "mm"),
                                                   draw.ulim = FALSE,
                                                   title.position = 'left',
                                                   label.position = 'right',
                                                   title.hjust = 0.5,
                                                   label.hjust = 0.5)) +
    theme_minimal() +
    theme(legend.text = element_text(angle = 90),
          legend.title = element_text(angle = 90),
          axis.text.x = element_text(angle = 90, vjust = 1, 
                                     size = 8, hjust = 1),
          axis.text.y = element_text(angle = 0, vjust = 1,
                                     size = 8, hjust = 1),
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(),
          panel.grid.major = element_line(size = 0.25), 
          panel.grid.minor = element_line(size = 0.25), 
          legend.position = c(0.25, 0.75),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank()) +
    coord_fixed()
  
  ggmatrix <- 
    ggheatmap +
    geom_text(aes(Var2, Var1, label = value, colour = value), size = 3) 
  
  ggsave(ggmatrix, filename = name, height = 8, width = 8, dpi = 300)
  
  return(ggmatrix)
  
}

get_community_assortativity_1 <-
  function(edges, nodes, node_attributes){
    
    temp_edges <- 
      edges %>%
      transmute(from = target,
                to = focal, 
                weight) 
    
    temp_nodes <-
      nodes %>% 
      transmute(cbg) %>% 
      left_join(rename(node_attributes, cbg = GEOID)) %>%
      replace_na(list(d_in = 0, d_out = 0, median_income = 0, pct_nonwhite = 0, 
                      out_weighted = 0, in_weighted = 0))
    
    population <- transmute(node_attributes, cbg = GEOID, population)
    
    graph <- 
      temp_edges %>%
      graph_from_data_frame(vertices = select(temp_nodes, cbg), directed = TRUE) %>%
      set_edge_attr("weight", value = temp_edges$weight) %>%
      set_vertex_attr("income", value = temp_nodes$median_income) %>%
      set_vertex_attr("race", value = temp_nodes$pct_nonwhite) 
    
    # infomap_clusters <- igraph::cluster_infomap(graph, nb.trials = 10, e.weights = E(graph)$weight)
    infomap_clusters <- igraph::cluster_louvain(as.undirected(graph), weights = E(graph)$weight)    
    
    community_assortativity <-
      function(community_subgraph, graph){
        
        induced_community <- induced_subgraph(graph, infomap_clusters[[community_subgraph]])
        
        return(list(igraph::assortativity(induced_community, V(induced_community)$race),
                    igraph::assortativity(induced_community, V(induced_community)$income)))
        
      }
    
    values <- map(unique(infomap_clusters$membership), ~community_assortativity(.x, graph))
    
    crosswalk <-
      nodes %>% 
      mutate(membership = infomap_clusters$membership) %>%
      left_join(population) %>%
      arrange(membership, desc(population)) %>%
      group_by(membership) %>%
      slice(1) %>%
      ungroup() %>%
      transmute(membership, top = cbg) %>%
      mutate(race = map_dbl(values, ~pluck(.x, 1)),
             income = map_dbl(values, ~pluck(.x, 2)),
             period = edges$period[1])
    
    return(crosswalk)
    
  }

get_community_assortativity_2 <-
  function(edges, nodes, node_attributes){
    
    temp_edges <- 
      edges %>%
      transmute(from = target,
                to = focal, 
                weight) 
    
    temp_nodes <-
      nodes %>% 
      transmute(cbg) %>% 
      left_join(rename(node_attributes, cbg = GEOID)) %>%
      replace_na(list(d_in = 0, d_out = 0, median_income = 0, pct_nonwhite = 0, 
                      out_weighted = 0, in_weighted = 0))
    
    graph <- 
      temp_edges %>%
      graph_from_data_frame(vertices = select(temp_nodes, cbg), directed = TRUE) %>%
      set_edge_attr("weight", value = temp_edges$weight) %>%
      set_vertex_attr("income", value = temp_nodes$median_income) %>%
      set_vertex_attr("race", value = temp_nodes$pct_nonwhite) 
    
    # infomap_clusters <- igraph::cluster_infomap(graph, nb.trials = 10, e.weights = E(graph)$weight)
    infomap_clusters <- igraph::cluster_louvain(as.undirected(graph), weights = E(graph)$weight)    
    
    recoded <- 
      map_dfr(unique(baseline_clusters$membership),
              function(j){
                
                C <- baseline_clusters[[j]]
                P_prime <- infomap_clusters
                
                N_i <- map_dbl(unique(P_prime$membership),
                               function(i){
                                 
                                 C_i <- P_prime[[i]]
                                 return(sum(C_i %in% C))
                                 
                               })
                
                return(tibble(C = j, C_prime = which.max(N_i)))
                
              })
    
    community_nodes <- 
      nodes %>%
      mutate(C_prime = infomap_clusters$membership) %>%
      left_join(recoded) %>%
      drop_na() %>%
      arrange(C) %>%
      group_by(C) %>%
      add_tally() %>%
      ungroup() %>%
      filter(n > 1)
    
    community_assortativity <-
      function(community_nodes, graph){
        
        induced_community <- induced_subgraph(graph, community_nodes$cbg)
        
        return(list(igraph::assortativity(induced_community, V(induced_community)$race),
                    igraph::assortativity(induced_community, V(induced_community)$income)))
        
      }
    
    values <- map(group_split(community_nodes, C), ~community_assortativity(.x, graph))
    
    crosswalk <- 
      community_nodes %>%
      distinct(C) %>%
      mutate(race = map_dbl(values, ~pluck(.x, 1)),
             income = map_dbl(values, ~pluck(.x, 2)),
             period = edges$period[1])
    
    return(crosswalk)
    
  }

get_conductance <- 
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
    
    infomap_runs <- future_map(1:6, ~igraph::cluster_infomap(graph, nb.trials = 10, e.weights = E(graph)$weight, modularity = TRUE))
    modularities <- map_dbl(infomap_runs, ~magrittr::use_series(.x, "modularity"))
    
    infomap_clusters <- infomap_runs[[which.max(modularities)]]
    
    return(sum(crossing(infomap_clusters, graph)) / (length(crossing(infomap_clusters, graph)) - sum(crossing(infomap_clusters, graph))))
    
  }

get_xy <- function(geometry, coordinate) {
  
  xy <- st_coordinates(geometry)
  
  return(xy[, coordinate])
  
}

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
  index <- ranger("2020-01", "2021-08")
  
  ## get edge data
  edges <- 
    map_df(index, function(x) { 
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
    map_dfr(1:20, 
            function(x) {
              
              map_dfr(1:20, 
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
    write_csv(glue("data/processed/2022_05/correlations/{city}.csv"))
  
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
    write_csv(glue("data/processed/2022_05/global/{city}.csv"))
  
  local %>%
    write_csv(glue("data/processed/2022_05/local/{city}.csv"))
  
  end <- Sys.time()
  print(end - start)
  
}
