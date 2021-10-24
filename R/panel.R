####################################
## New York City
####################################

source("R/query.R")
source("R/package.R")
source("R/help.R")

## getting fips codes, either whole metro or five boros
codes <- get_codes("new york")
codes <-  "'36005', '36047', '36061', '36081', '36085'"

## getting node data
nodes <- get_nodes(codes)

## creating our index
index <- ranger("2020-01", "2021-08")

## getting edge data
edges <- map_df(index, function(x) { get_edges(codes, x, nodes$cbg) %>% mutate(year = parse_number(str_sub(x, 1, 4)),
                                                                               month = lubridate::month(parse_number(str_sub(x, 6, 7)), label = TRUE, abbr = FALSE)) })

## getting context data
shape <- 
  reduce(
    map(codes %>% 
          str_remove_all("\'") %>% 
          str_split(", ") %>% 
          magrittr::extract2(1),
        function(x) { 
          block_groups(state = str_sub(x, 1, 2), county = str_sub(x, 3, 5), class = 'sf')
        }),
    rbind)

water <- 
  reduce(
    map(codes %>% 
          str_remove_all("\'") %>% 
          str_split(", ") %>% 
          magrittr::extract2(1),
        function(x) { 
          area_water(state = str_sub(x, 1, 2), county = str_sub(x, 3, 5), class = 'sf')
        }),
    rbind)

## creating background
water <- 
  water %>% 
  st_union() %>%
  st_combine()

background <-
  shape %>% 
  st_union() %>% 
  st_combine() %>% 
  st_difference(water) %>% 
  rmapshaper::ms_simplify(0.05)

##  plotting desire lines
ym <- 
  edges %>% 
  mutate(period = glue("{month}, {year}")) %>% 
  pull(period) %>% 
  unique()

lines <- 
  stplanr::od2line(edges, 
                   nodes %>% 
                     st_as_sf(coords = c("X", "Y"), crs = 4326) %>% 
                     st_transform(4269)) %>% 
  mutate(distance = units::drop_units(units::set_units(st_length(geometry), km)))

lines_trimmed <-
  lines %>% 
  mutate(period = factor(glue("{month}, {year}"), levels = ym)) %>%
  filter(str_detect(month, "April|January|July"))

flows <- 
  ggplot() +
  geom_sf(data = background, 
          aes(), fill = NA, colour = '#000000', lwd = 0.25) +
  geom_sf(data = lines_trimmed, 
          aes(colour = cut(lines_trimmed$weight, c(0, quantile(lines_trimmed$weight,
                                                               c(.3,.4,.5,.6,.7,.8, .9, .95, 1),
                                                               na.rm = TRUE))), 
              lwd = weight, alpha = weight)) +
  scale_colour_manual(values = scico::scico(9, palette = 'hawaii'),
                      labels = as.character(quantile(lines_trimmed$weight,
                                                     c(.3,.4,.5,.6,.7,.8,.9, .95, 1),
                                                     na.rm = TRUE)),
                      name = "visits",
                      guide =   guide_legend(direction = "horizontal",
                                             keyheight = unit(2, units = "mm"),
                                             keywidth = unit(10, units = "mm"),
                                             title.position = 'top',
                                             label.position = 'bottom',
                                             title.hjust = 0.5,
                                             label.hjust = 0.75,
                                             nrow = 1,
                                             byrow = TRUE)) +
  scale_size_continuous(range = c(0.1, 1), guide = 'none') +
  scale_alpha(range = c(0.1, 0.5), guide = 'none') +
  facet_wrap(~ period, nrow = 2) +
  theme_map() +
  theme(legend.position = 'bottom')

ggsave(flows, filename = "flows_nyc.png", height = 6, width = 8, dpi = 300)

## prepare data
ready <- 
  edges %>% 
  mutate(distance = lines$distance) %>% 
  group_by(year, month) %>% 
  group_split()

## convert to matrix
square <- array(dim = c(length(ready), nrow(nodes), nrow(nodes)))

for (i in 1:length(ready)) {
  
  temp <- 
    ready[[i]] %>%
    transmute(from = target,
              to = focal, 
              weight) %>%
    filter(from %in% shape$GEOID) %>%
    arrange(to, from)
  
  adjacencies <- 
    temp %>%
    graph_from_data_frame(vertices = nodes, directed = FALSE) %>%
    set_edge_attr("weight", value = edges$weight) %>% 
    as_adjacency_matrix(attr = "weight") %>% 
    as.matrix()
  
  square[i, , ] <- adjacencies
  
}

tictoc::tic()
correlations <- sna::gcor(square)
tictoc::toc()

rownames(correlations) <- ym
colnames(correlations) <- ym

corrplot <- correlate(correlations, "correlations.png")

## checking other trends
density <- 
  map(ready, 
      function(x){
        
        edges <- 
          x %>%
          transmute(from = target,
                    to = focal, 
                    weight) 
        
        nodes <- transmute(nodes, cbg)
        
        graph <- 
          edges %>%
          graph_from_data_frame(vertices = nodes, directed = TRUE) %>%
          set_edge_attr("weight", value = edges$weight)
        
        return(igraph::graph.density(graph, loops = FALSE))
        
      })

plot(1:20, reduce(density, c))

## getting centralities
centraliser <- 
  function(x, y) {
    
    edges <- 
      x %>%
      transmute(from = target,
                to = focal, 
                weight) 
    
    nodes <- transmute(nodes, cbg)
    
    graph <- 
      edges %>%
      graph_from_data_frame(vertices = nodes, directed = TRUE) %>%
      set_edge_attr("weight", value = edges$weight) %>%
      simplify()
    
    infomap_clusters <- cluster_infomap(as.undirected(graph), e.weights = E(graph)$weight)
    leiden_clusters <- cluster_leiden(as.undirected(graph), weights = 'weight', objective_function = 'modularity')
    
    cents <- 
      tibble(GEOID = V(graph)$name,
             evc = evcent(graph)$vector, 
             deg = degree(graph, loops = FALSE),
             ind = degree(graph, mode = "in", loops = FALSE),
             out = degree(graph, mode = "out", loops = FALSE),
             bet = betweenness(graph, weights = E(graph)$weight, normalized = TRUE), 
             clo = closeness(graph, weights =  E(graph)$weight),
             eci = eccentricity(graph, mode = "in"),
             eco = eccentricity(graph, mode = "out"),
             prc = page_rank(graph, directed = TRUE, weights = E(graph)$weight)$vector,
             ass = transitivity(as.undirected(graph), type = "local"),
             inf = infomap_clusters$membership,
             lei = leiden_clusters$membership) %>%
      mutate(year = unique(x$year),
             month = unique(x$month))
    
    return(cents)
    
  }

## parallel processing
library(furrr)
plan(multisession, workers = 6)

## map with parallel processing
tictoc::tic()
centralities <- future_map_dfr(ready, centraliser)
tictoc::toc()

## adding demography
sf1 <- c(white = "P005003",
         black = "P005004",
         asian = "P005006",
         hispanic = "P004003")

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

wide <- 
  race %>% 
  select(GEOID, variable, value) %>%
  pivot_wider(id_cols = GEOID, names_from = variable, values_from = value) %>%
  transmute(GEOID = GEOID,
            white = white,
            nonwhite = black + hispanic + asian)

race_split <- 
  centralities %>% 
  filter(year == "2020") %>%
  group_by(lei) %>%
  add_tally() %>%
  ungroup() %>%
  filter(n > 1) %>%
  select(-n) %>%
  transmute(GEOID = GEOID, month = month, infomap = lei) %>%
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

## palette 
pal <- scico::scico(9, palette = 'hawaii', direction = -1)

## building a table
dissimilarity <- 
  centralities %>%
  group_by(inf, month) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(month) %>%
  summarise(`mean community size` = mean(n)) %>%
  mutate(`month` = lubridate::month(1:12, label = TRUE, abbr = FALSE),
         `network density` = reduce(density[1:12], c), 
         `dissimilarity index` = reduce(list[1:12], c)) %>%
  gt() %>% 
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_column_labels(vars(`month`))) %>% 
  data_color(columns = vars(`dissimilarity index`),
             colors = scales::col_numeric(pal, domain = NULL)) %>%
  gtsave("dissimilarity_nyc.png", expand = 10)

dissimilarity <- 
  centralities %>%
  group_by(inf, month) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(month) %>%
  summarise(`InfoMap community size (mean)` = mean(n)) %>%
  ungroup() %>%
  mutate(`month` = lubridate::month(1:12, label = TRUE),
         `network density` = reduce(density[1:12], c), 
         `dissimilarity index` = reduce(list[1:12], c)) %>% 
  pivot_longer(cols = `InfoMap community size (mean)`:`dissimilarity index`, names_to = "variable", values_to = "value") %>% 
  filter(variable != "dissimilarity index") %>%
  ggplot(aes(x = month, y = value, colour = variable, group = variable)) +
  geom_line(size = 2) +
  scale_colour_manual(values = c(pal[7], pal[3]), guide = 'none') + 
  facet_wrap(~ variable, ncol = 1, scales = 'free_y') + 
  ylab("") +
  xlab("") +
  theme_hor() +
  ggsave("trends_nyc.png", height = 5, width = 5, dpi = 300)

## looking at income and entropy
mixing <- map_df(index[c(1, 4, 7, 10, 13)], ~get_profiles("'36005', '36047', '36061', '36081', '36085'", .x, "sub_category", "restaurants")) 

grid <- 
  background %>% 
  st_make_grid(n = 100) %>% 
  as_tibble() %>%
  rownames_to_column() %>%
  rename(id = rowname) %>%
  mutate(id = case_when(nchar(id) < 2 ~ paste("000", {id}, sep = ""),
                        nchar(id) < 3 ~ paste("00", {id}, sep = ""),
                        nchar(id) < 4 ~ paste("0", {id}, sep = ""),
                        TRUE ~ paste({id}))) %>%
  st_as_sf()

plot(grid)

intersection <- st_intersection(grid, background)

dizz <- 
  grid %>% 
  filter(id %in% intersection$id) %>%
  st_union() %>% 
  st_combine()

measures <-
  mixing %>% 
  mutate(white = pct_white * log(pct_white, base = exp(1)),
         black = pct_black * log(pct_black, base = exp(1)),
         hispanic = pct_hispanic * log(pct_hispanic, base = exp(1)),
         other =  pct_other * log(pct_other, base = exp(1))) %>% 
  mutate(entropy = -1 * (white + black + hispanic + other)) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(st_crs(shape)) %>% 
  st_join(grid) %>% 
  st_drop_geometry() %>% 
  group_by(id, month, year) %>% 
  add_tally() %>% 
  summarise(entropy = mean(entropy),
            income = weighted_median) %>%
  drop_na() %>% 
  left_join(grid) %>% 
  st_as_sf() 

ggplot(measures %>%
         mutate(month = factor(glue("{month}, {year}", levels = c("January, 2020", "April, 2020", "July, 2020", "October, 2020", "January, 2021"))))) +
  geom_sf(data = background, aes(), colour = '#000000', fill = NA, size = 0.5) + 
  geom_sf(aes(fill = factor(ntile(income, 9))), colour = NA, size = 0) +
  scale_fill_manual(values = pal,
                    labels = str_sub(as.character(round(quantile(measures$income,
                                                                 c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
                                                                 na.rm = TRUE))), 1, 2),
                    name = "median income (000s)",
                    guide = 
                      guide_legend(direction = "horizontal",
                                   keyheight = unit(1.5, units = "mm"),
                                   keywidth = unit(8, units = "mm"),
                                   title.position = 'top',
                                   label.position = 'bottom',
                                   title.hjust = 0.5,
                                   label.hjust = 0,
                                   nrow = 1,
                                   byrow = TRUE)) +
  facet_wrap(~ month) +
  theme_map() +
  theme(legend.position = c(0.9, 0.2))  +
  ggsave("income_nyc.png", height = 6, width = 10, dpi = 300)

-1 * ((0.25 * log(0.25)) + (0.25 * log(0.25)) + (0.25 * log(0.25)) + (0.25 * log(0.25))) 

ggplot(measures %>%
         mutate(month = factor(glue("{month}, {year}", levels = c("January, 2020", "April, 2020", "July, 2020", "October, 2020", "January, 2021"))))) +
  geom_sf(data = background, aes(), colour = '#000000', fill = NA, size = 0.5) + 
  geom_sf(aes(fill = factor(ntile(entropy, 9))), colour = NA, size = 0) +
  scale_fill_manual(values = pal,
                    labels = as.character(round(quantile(measures$entropy,
                                                         c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
                                                         na.rm = TRUE), 2)),
                    name = "entropy (0 - 1.39)",
                    guide = 
                      guide_legend(direction = "horizontal",
                                   keyheight = unit(1.5, units = "mm"),
                                   keywidth = unit(8, units = "mm"),
                                   title.position = 'top',
                                   label.position = 'bottom',
                                   title.hjust = 0.5,
                                   label.hjust = 0,
                                   nrow = 1,
                                   byrow = TRUE)) +
  facet_wrap(~ month) +
  theme_map() +
  theme(legend.position = c(0.9, 0.2))  +
  ggsave("entropy_nyc.png", height = 6, width = 10, dpi = 300)

## ecdf
library(gganimate)

anim <- 
  ggplot(data = centralities %>% 
           mutate(month = factor(glue("{month}, {year}"), levels = ym)), 
         aes(x = ind - out, colour = month)) +
  stat_ecdf(size = 2) +
  scale_color_manual(values = scico::scico(n = 20, palette = 'hawaii'), 
                     name = 'month',
                     guide = 'none') +
  scale_x_log10() +
  ylab("") +
  xlab("degree K") +
  labs(title = "Changing Degrees", subtitle = "Cumulative probability in {current_frame}") +
  theme_ver() +
  transition_manual(month, cumulative = TRUE) +
  ease_aes() +
  enter_grow()

anim_save("ecdf_outin_nyc.gif", animation = anim, 
          height = 600, width = 800, nframes = 24, fps = 1,
          start_pause = 2, end_pause = 2)

## assortativity
income <- 
  vroom("data/census/data/cbg_b19.csv") %>% 
  filter(census_block_group %in% nodes$cbg) %>%
  select(census_block_group, B19301e1, B19301m1) %>% 
  transmute(GEOID = census_block_group,
            median_income = B19301e1)

wide <- 
  race %>% 
  select(GEOID, variable, value) %>%
  pivot_wider(id_cols = GEOID, names_from = variable, values_from = value) %>%
  transmute(GEOID = GEOID,
            white = white,
            nonwhite = black + hispanic + asian) %>%
  left_join(income) %>% 
  transmute(GEOID,
            income = median_income,
            pct_nonwhite = nonwhite / (white + nonwhite)) %>% 
  drop_na()

sorting <- 
  map_df(ready, 
      function(x){
        
        temp_edges <- 
          x %>%
          transmute(from = target,
                    to = focal, 
                    weight) 
        
        temp_nodes <-
          nodes %>% 
          transmute(cbg) %>% 
          left_join(x %>% 
                      group_by(target) %>% 
                      summarise(distance = mean(distance)) %>% 
                      rename(cbg = target)) %>% 
          left_join(wide %>%
                      rename(cbg = GEOID)) %>%
          replace_na(list(distance = 0, income = 0, pct_nonwhite = 0))
        
        graph <- 
          temp_edges %>%
          graph_from_data_frame(vertices = select(temp_nodes, cbg), directed = TRUE) %>%
          set_edge_attr("weight", value = temp_edges$weight) %>%
          set_vertex_attr("distance", value = temp_nodes$distance) %>%
          set_vertex_attr("income", value = temp_nodes$income) %>%
          set_vertex_attr("race", value = temp_nodes$pct_nonwhite)
        
        sorting <- tibble(year = x$year[1],
                          month =x$month[1],
                          degree = igraph::assortativity_degree(graph),
                          income = igraph::assortativity(graph, V(graph)$income),
                          race = igraph::assortativity(graph, V(graph)$race),
                          distance = igraph::assortativity(graph, V(graph)$distance))
        
        return(sorting)
        
      })

ggplot(sorting %>% 
         mutate(month = factor(glue("{month}, {year}"), levels = ym)) %>% 
         select(-year, -degree) %>% 
         pivot_longer(!month),
       aes(x = month, y = value, colour = name, group = name)) +
  geom_line(size = 2) +
  geom_text(data = 
              sorting %>% 
              mutate(month = factor(glue("{month}, {year}"), levels = ym)) %>% 
              select(-year, -degree) %>% 
              pivot_longer(!month) %>%
              filter(month == "December, 2020"),
            aes(x = month, y = value + 0.05, colour = name, label = name),
            fontface = 'bold') +
  labs(x = "", y = "", title = "Assortativity") +
  scale_colour_manual(values = c(pal[1], pal[5], pal[9]), guide = 'none') +
  theme_hor() +
  theme(axis.text.x = element_text(angle = 90)) +
  ggsave("assortativities.png", height = 6, width = 10, dpi = 300)


