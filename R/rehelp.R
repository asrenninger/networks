#################################################
## Let's do this...
#################################################
get_distance <-
  function(edges, nodes){
    
    distances <- stplanr::od2line(edges, 
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
             assortativity_income = igraph::assortativity(graph, V(graph)$income),
             assortativity_race = igraph::assortativity(graph, V(graph)$race))
    
    graph <- simplify(graph)
    
    infomap_clusters <- igraph::cluster_infomap(as.undirected(graph), nb.trials = 10, e.weights = E(graph)$weight)
    leiden_clusters <- igraph::cluster_leiden(as.undirected(graph), resolution_parameter = 0.9, objective_function = 'modularity')

    local <- 
      tibble(GEOID = V(graph)$name,
             eigenvector = igraph::eigen_centrality(graph)$vector, 
             k_all = igraph::degree(graph, loops = FALSE),
             k_i = igraph::degree(graph, mode = "in", loops = FALSE),
             k_o = igraph::degree(graph, mode = "out", loops = FALSE),
             eccentricity_i = igraph::eccentricity(graph, mode = "in"),
             eccentricity_o = igraph::eccentricity(graph, mode = "out"),
             transitivity_l = igraph::transitivity(graph, type = "barrat"),
             infomap_1 = infomap_clusters$membership,
             infomap_2 = igraph::cluster_infomap(as.undirected(graph), e.weights = E(graph)$weight)$membership,
             infomap_3 = igraph::cluster_infomap(as.undirected(graph), e.weights = E(graph)$weight)$membership,
             infomap_4 = igraph::cluster_infomap(as.undirected(graph), e.weights = E(graph)$weight)$membership,
             leiden = leiden_clusters$membership) %>%
      left_join(weights) %>%
      mutate(period = edges$period[1])
   
    global <-
      global %>%
      mutate(Q_i = infomap_clusters$modularity,
             Q_l = leiden_clusters$quality)
    
    return(list(local, global))
      
  }

get_correlations <- 
  function(edges, nodes){
    
    square <- array(dim = c(length(ready), nrow(nodes), nrow(nodes)))
    
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
    igraph::sample_degseq(out.deg = igraph::degree(graph, mode = "in", loops = FALSE),
                          in.deg = igraph::degree(graph, mode = "out", loops = FALSE)) %>%
    set_edge_attr("weight", value = sample(E(graph)$weight))
    
  
  return(configuration_model)
  
}

get_dissimilarity <-
  function(communities, node_attributes, infomap_run){
    
    run <- as.character(infomap_run)
    
    race_split <- 
      communities %>% 
      select(GEOID, period, leiden, ends_with(run)) %>%
      rename_at(vars(ends_with(run)), ~str_remove(.x, "_.*")) %>%
      left_join(node_attributes) %>%
      group_by(period, leiden) %>%
      summarise(white = sum(white),
                nonwhite = sum(population - white),
                upper = sum(upper),
                lower = sum(lower)) %>%
      ungroup() %>%
      group_by(period) %>%
      group_split()
    
    series <- 
      reduce(map(race_split, function(x){
        tibble(race = MLID::id(as.data.frame(x), vars = c("nonwhite", "white")) %>% magrittr::extract2(1),
               income = MLID::id(as.data.frame(x), vars = c("lower", "upper")) %>% magrittr::extract2(1))
      }), rbind)
    
    return(mutate(series, period = factor(ym, levels = ym)))
    
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
    
    infomap_clusters <- igraph::cluster_infomap(as.undirected(graph), nb.trials = 10, e.weights = E(graph)$weight)
    leiden_clusters <- igraph::cluster_leiden(as.undirected(graph), resolution_parameter = 0.9, objective_function = 'modularity')
    
    partitions <- 
      tibble(GEOID = V(graph)$name,
             infomap = infomap_clusters$membership,
             leiden = leiden_clusters$membership, 
             period = edges$period[1])
    
    return(partitions)
    
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
                 q_infomap = modularity(as.undirected(graph), x$infomap, weights = E(graph)$weight))
        
        return(quality)
        
      })
    
    return(qualities)
    
  }


