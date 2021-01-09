####################################
## getting measurements
####################################

source("R/query.R")
source("R/package.R")
source("R/help.R")

## the big one
get_metrics <- function(metro, months) {
  
  print(metro)
  
  ## palette 
  pal <- scico::scico(9, palette = 'turku')
  
  ##  getting fips codes
  codes <- get_codes(metro)
  
  ## getting node data
  nodes <- get_nodes(codes)
  
  ## getting edge data
  index <- str_pad(months, side = 'left', width = 2, pad = "0")
  edges <- map_df(index, function(x) { get_edges(codes, x, nodes$cbg) %>% mutate(month = as.numeric(x)) })
  
  ## splitting the data for analysis
  ready <- 
    edges %>% 
    group_by(month) %>% 
    group_split()
  
  ## functions! 
  print("density")
  density     <- get_density(ready, nodes)
  
  print("centrality")
  centrality  <- get_centrality(ready, nodes)
  
  print("correlation")
  correlation <- get_correlation(ready, nodes, months)
  
  rownames(correlation) <- lubridate::month(1:length(months), label = TRUE, abbr = FALSE)
  colnames(correlation) <- lubridate::month(1:length(months), label = TRUE, abbr = FALSE)
 
  ## adding demography
  sf1 <- c(white = "P005003",
           black = "P005004",
           asian = "P005006",
           hispanic = "P004003")
  
  print("race")
  race <-
    map_df(codes %>% 
             str_remove_all("\'") %>% 
             str_split(", ") %>% 
             magrittr::extract2(1),
           function(x) { 
             get_decennial(geography = "block group", variables = sf1,
                           state = str_sub(x, 1, 2), county = str_sub(x, 3, 5), geometry = FALSE,
                           summary_var = "P001001") 
             
           })
  
  ## getting community size
  print("community size")
  community <- 
    centrality %>% 
    group_by(infomap, month) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    group_by(month) %>% 
    summarise(infomap = mean(n)) %>% 
    pull(infomap)
  
  ## adding dissimilarity index
  print("similarity")
  similarity <- get_dissimilarity(centrality, race)
  
  print("diversity")
  diversity <- get_diversity(centrality, race)
  
  print("combined")
  diversities <- mutate(diversity, 
                        diss = similarity,
                        density = density, 
                        infomap = community) %>% 
    mutate(city = metro)
  
  ## plotting it 
  print("viz")
  correlate(correlation, metro, glue("viz/results/correlations/correlation_{metro}.png"))
  diversify(diversities, metro, glue("viz/results/diversities/diversity_{metro}.png"))
  
  print("writing centrality")
  centrality %>% 
    mutate(city = metro) %>% 
    write_csv(glue("data/processed/centralities/centrality_{metro}.csv"))
  
  print("writing correlation")
  correlation %>% 
    get_upper_tri() %>%
    as_tibble() %>%
    rownames_to_column(var = "month") %>% 
    pivot_longer(cols = January:December) %>%
    drop_na() %>% 
    mutate(city = metro) %>%
    write_csv(glue("data/processed/correlations/correlation_{metro}.csv"))
  
  write_csv(diversities, glue("data/processed/diversities/diversity_{metro}.csv"))
  return(diversities)
  
}

get_centrality <- function(networks, nodes) {
  
  centraliser <- 
    function(network) {
      
      new_edges <- 
        network %>%
        transmute(from = target,
                  to = focal, 
                  weight) 
      
      new_nodes <- transmute(nodes, cbg)
      
      graph <- 
        new_edges %>%
        graph_from_data_frame(vertices = new_nodes, directed = TRUE) %>%
        set_edge_attr("weight", value = new_edges$weight)
      
      infomap_clusters <- cluster_infomap(graph)
      
      cents <- 
        tibble(GEOID = V(graph)$name,
               evc = evcent(graph)$vector, 
               deg = degree(graph, loops = FALSE),
               ind = degree(graph, mode = "in", loops = FALSE),
               out = degree(graph, mode = "out", loops = FALSE),
               bet = betweenness(graph, weights = E(graph)$weight, normalized = TRUE), 
               clo = closeness(graph, weights =  E(graph)$weight),
               ecc = eccentricity(graph),
               prc = page_rank(graph, directed = TRUE, weights = E(graph)$weight)$vector,
               infomap = infomap_clusters$membership) %>%
        mutate(month = unique(network$month))
      
      return(cents)
      
    }
  
  centralities <- map_df(networks, centraliser)
  return(centralities)
  
}

get_density <- function(networks, nodes) {
  
  density <- 
    map(networks, 
        function(x){
          
          new_edges <- 
            x %>%
            transmute(from = target,
                      to = focal, 
                      weight) 
          
          new_nodes <- transmute(nodes, cbg)
          
          graph <- 
            new_edges %>%
            graph_from_data_frame(vertices = new_nodes, directed = TRUE) %>%
            set_edge_attr("weight", value = new_edges$weight)
          
          return(igraph::graph.density(graph, loop = FALSE))
          
        })
  
  return(reduce(density, c))
  
}

get_dissimilarity <- function(centralities, race) {
  
  wide <- 
    race %>% 
    select(GEOID, variable, value) %>%
    pivot_wider(id_cols = GEOID, names_from = variable, values_from = value) %>%
    transmute(GEOID = GEOID,
              white = white,
              nonwhite = black + hispanic)
  
  race_split <- 
    centralities %>% 
    transmute(GEOID, month, infomap) %>%
    left_join(wide) %>%
    group_by(month, infomap) %>%
    summarise(white = sum(white),
              nonwhite = sum(nonwhite)) %>%
    ungroup() %>%
    group_by(month) %>%
    group_split()
  
  list <- 
    purrr::map(race_split, function(x){
      MLID::id(as.data.frame(x), vars = c("nonwhite", "white")) %>% magrittr::extract2(1)
    })
  
  return(reduce(list, c))
  
}

get_correlation <- function(networks, nodes, months) {

  square <- array(dim = c(length(months), nrow(nodes), nrow(nodes)))
  
  for (i in 1:length(months)) {
    
    new_edges <- 
      networks[[i]] %>%
      transmute(from = target,
                to = focal, 
                weight) %>%
      filter(weight > 50) %>%
      filter(from %in% nodes$cbg) %>%
      arrange(to, from)
    
    adjacencies <- 
      new_edges %>%
      graph_from_data_frame(vertices = nodes, directed = FALSE) %>%
      set_edge_attr("weight", value = new_edges$weight) %>% 
      as_adjacency_matrix(attr = "weight") %>% 
      as.matrix()
    
    square[i, , ] <- adjacencies
    
  }
  
  correlations <- sna::gcor(square)
  
  return(correlations)
  
}

get_diversity <- function(centralities, demographics) {
  months <- unique(centralities$month)
  
  indices <- 
    map_df(months, function(x){
      
      base <- 
        centralities %>% 
        filter(month == x) %>% 
        transmute(GEOID, month, infomap) %>%
        right_join(demographics) %>% 
        group_by(infomap, variable) %>%
        summarise(value = sum(value)) %>%
        ungroup() %>%
        filter(value > 0) %>%
        pivot_wider(names_from = variable, values_from = value) 
      
      working <- select(base, -infomap)
      working <- as.matrix(working)
      
      rownames(working) <- base$infomap
      
      working[is.na(working)] <- 0
      
      indices <- 
        diverse::diversity(working) %>%
        as_tibble() %>% 
        rownames_to_column(var = "infomap") %>% 
        select(infomap, entropy, HHI, gini.simpson, berger.parker.D) %>%
        mutate(month = x) %>% 
        clean_names()
      
      return(indices)
      
    })
  
  aggregated <- 
    indices %>%
    group_by(month) %>%
    summarise(entropy = mean(entropy),
              hhi = mean(hhi),
              gini = mean(gini_simpson),
              bp = mean(berger_parker_d))
  
  return(aggregated)
  
}

## plotting functions
diversify <- function(metrics, metro, name) {
  
  pal <- scico::scico(9, palette = 'turku')
  
  long <- 
    metrics %>% 
    transmute(`month` = lubridate::month(month, label = TRUE),
              `InfoMap community size (mean)` = infomap,
              `network density` = density) %>%
  pivot_longer(cols = `InfoMap community size (mean)`:`network density`, names_to = "variable", values_to = "value")
  
  ggplot(long,aes(x = month, y = value, colour = variable, group = variable)) +
    geom_line(size = 2) +
    scale_colour_manual(values = c(pal[3], pal[7]), guide = 'none') + 
    facet_wrap(~ variable, ncol = 1, scales = 'free_y') + 
    ylab("") +
    xlab("") +
    ggtitle(title = glue("{metro}")) +
    theme_hor() +
    ggsave(filename = name, height = 5, width = 5, dpi = 300)
  
}

correlate <- function(correlations, metro, name) {
  
  pal <- scico::scico(9, palette = 'turku')
  mat <- round(correlations, 2)
  
  ##
  
  get_lower_tri <- function(cormat){
    cormat[upper.tri(cormat)] <- NA
    return(cormat)
  }
  
  get_upper_tri <- function(cormat){
    cormat[lower.tri(cormat)]<- NA
    return(cormat)
  }
  
  ##
  
  upper_tri <- get_upper_tri(mat)
  melted_mat <- reshape2::melt(upper_tri, na.rm = TRUE)
  
  ##
  
  ggheatmap <- 
    ggplot(data = na.omit(melted_mat), aes(Var2, Var1, fill = value)) +
    geom_tile(color = "white") +
    scico::scale_colour_scico(palette = 'turku', direction = -1,
                              guide = 'none') +
    scico::scale_fill_scico(palette = 'turku',
                            limit = c(0.8, 1), 
                            oob = scales::squish,
                            name = "pearson\ncorrelation",
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
    geom_text(aes(Var2, Var1, label = value, colour = value), size = 3) +
    ggtitle(title = glue("{metro}"))
  
  ggsave(ggmatrix, filename = name, height = 8, width = 8, dpi = 300)
  
  return(ggmatrix)
  
}

get_lower_tri <- function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)] <- NA
  return(cormat)
}
