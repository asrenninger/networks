########################################
## Testing
########################################

source("R/package.R")
source("R/help.R")

##

codes <- get_codes("philadelphia")
nodes <- get_nodes(codes)

##

month <- str_pad(1:11, width = 2, pad = "0", side = 'left')

##

edges <- map_df(month, function(x){
  get_edges(codes, x, nodes$cbg) %>%
    mutate(month = x)
}) %>%
  glimpse()

##

paths <- stplanr::od2line(edges, nodes %>% st_as_sf(coords = c("X", "Y")))

##

flows <- 
  ggplot() +
  #geom_sf(data = background, 
  #        aes(), fill = NA, colour = '#000000', lwd = 0.5) +
  geom_sf(data = paths %>% 
            mutate(label = lubridate::month(as.numeric(month), label = TRUE, abbr = FALSE)), 
          aes(colour = factor(ntile(weight, 9)), lwd = weight, alpha = weight)) +
  scale_colour_manual(values = rev(pal[seq(1, length(pal), length(pal) %/% 9)]),
                      labels = as.character(quantile(paths$weight,
                                                     c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
                                                     na.rm = TRUE)),
                      name = "visits",
                      guide = guide_discrete) +
  scale_size_continuous(range = c(0.1, 1), guide = 'none') +
  scale_alpha(range = c(0.1, 0.5), guide = 'none') +
  facet_wrap(~ label, nrow = 3) +
  labs(title = 'Origin-Destination Flows', subtitle = "Connections between neighborhoods and points of interest") +
  theme_map() +
  theme(legend.position = 'bottom')

ggsave(flows, filename = "flows_metro.png", height = 12, width = 14, dpi = 300)

##

metro <- 
  read_csv("https://raw.githubusercontent.com/asrenninger/networks/master/data/metrolist.csv", n_max = 1159, col_names = FALSE) %>%
  set_names(c('metro_fips', 'metro_name', 'county_fips', 'county_name')) 

codes <- 
  metro %>%
  filter(str_detect(str_to_lower(metro_name), "new york")) %>%
  filter(str_detect(county_fips, "36005|36081|36061|36047|36085")) %>%
  pull(county_fips) %>%
  glue_collapse(sep = "\', \'")

codes <- paste("\'", codes, "\'", sep = "")

##

nodes <- get_nodes(codes)

##

edges <- map_df(month, function(x){
  get_edges(codes, x, nodes$cbg) %>%
    mutate(month = x)
}) %>%
  glimpse()

##

paths <- stplanr::od2line(edges, nodes %>% st_as_sf(coords = c("X", "Y"), crs = 4326) %>% st_transform(4269))

##

flows <- 
  ggplot() +
  #geom_sf(data = background, 
  #        aes(), fill = NA, colour = '#000000', lwd = 0.5) +
  geom_sf(data = paths %>% 
            mutate(label = lubridate::month(as.numeric(month), label = TRUE, abbr = FALSE)), 
          aes(colour = factor(ntile(weight, 9)), lwd = weight, alpha = weight)) +
  scale_colour_manual(values = rev(pal[seq(1, length(pal), length(pal) %/% 9)]),
                      labels = as.character(quantile(paths$weight,
                                                     c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
                                                     na.rm = TRUE)),
                      name = "visits",
                      guide = guide_discrete) +
  scale_size_continuous(range = c(0.1, 1), guide = 'none') +
  scale_alpha(range = c(0.1, 0.5), guide = 'none') +
  facet_wrap(~ label, nrow = 3) +
  labs(title = 'Origin-Destination Flows', subtitle = "Connections between neighborhoods and points of interest") +
  theme_map() +
  theme(legend.position = 'bottom')

ggsave(flows, filename = "flows_metro.png", height = 11, width = 17, dpi = 300)

##

ggplot(paths %>%
         mutate(length = st_length(geometry), 
                length = units::set_units(length, km)) %>%
         mutate(length = as.numeric(length))) +
  geom_histogram(aes(x = length, y = ..density.., weight = weight), bins = 60) + 
  scale_x_log10()

##

ready <- 
  edges %>% 
  filter(focal %in% nodes$cbg & target %in% nodes$cbg) %>%
  group_by(month) %>% 
  group_split()

centraliser <- 
  function(x, y) {
    
    links <- 
      x %>%
      transmute(from = target,
                to = focal, 
                weight = weight)
    
    verts <- 
      nodes %>%
      transmute(cbg = cbg)
    
    graph <- 
      links %>%
      graph_from_data_frame(vertices = verts, directed = TRUE) %>%
      set_edge_attr("weight", value = edges$weight)
    
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
             inf = infomap_clusters$membership) %>%
      mutate(month = unique(x$month))
    
    return(cents)
    
  }

##

cents <- map_df(ready, centraliser)

##

cents %>% 
  group_by(month, inf) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(month) %>%
  summarise(mean = mean(n))

##

blocks <- 
  metro %>%
  filter(str_detect(str_to_lower(metro_name), "new york")) %>%
  filter(str_detect(county_fips, "36005|36081|36061|36047|36085")) %>%
  pull(county_fips) %>%
  map(function(x){ block_groups(str_sub(x, 1, 2), str_sub(x, 3, 5), cb = TRUE, class = 'sf') }) %>%
  reduce(rbind) 

##

vars <- c(white = "P005003",
          black = "P005004",
          asian = "P005006",
          hispanic = "P004003")

library(tidycensus)

race <- 
  metro %>%
  filter(str_detect(str_to_lower(metro_name), "new york")) %>%
  filter(str_detect(county_fips, "36005|36081|36061|36047|36085")) %>%
  pull(county_fips) %>%
  map(function(x){ get_decennial(geography = "block group", variables = vars,
                                 state = str_sub(x, 1, 2), county = str_sub(x, 3, 5), geometry = TRUE,
                                 summary_var = "P001001") }) %>%
  reduce(rbind) 

##

comms <-
  ggplot() +
  geom_sf(data = race %>%
            mutate(variable = str_to_title(variable)) %>%
            mutate(percent = 100 * (value / summary_value)), 
          aes(fill = percent), lwd = 0) + 
  geom_sf(data = 
            cents %>%
            mutate(label = lubridate::month(as.numeric(month), label = TRUE, abbr = FALSE)) %>%
            left_join(blocks) %>%
            st_as_sf() %>%
            group_by(label, inf) %>%
            summarise(), aes(), lwd = 0.5, fill = NA) +
  scale_fill_gradientn(colours = rev(pal), 
                       guide = guide_continuous) +
  facet_grid(label ~ variable) +
  #labs(title = "Communities by Month",
  #     subtitle = "Detected mobility clusters over demographic compositon") + 
  theme_map() +
  theme(legend.position = 'bottom',
        strip.text.y = element_text(angle = 270)) 

ggsave(comms, filepath = "communitiesxrace_clean.png", height = 48, width = 20, dpi = 300)

##

