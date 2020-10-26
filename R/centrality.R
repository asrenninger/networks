########################################
## Centralities
########################################

source("R/package.R")
source("R/help.R")

##

odmat <- vroom("data/processed/od_monthly.csv")
phila <- read_sf("data/processed/phila.geojson")

##

shape <- block_groups("PA", "Philadelphia", cb = TRUE, class = 'sf')

##

sginf <- 
  phila %>% 
  transmute(safegraph_place_id = safegraph_place_id,
            locale = location_name, type = top_category, naics = str_sub(naics_code, 1, 2),
            poi_cbg = GEOID) %>% 
  st_drop_geometry()

##

stamp <- c("January", "February", "March", "April", "May", "June", "July", "August") 

##

unimo <-
  odmat %>%
  left_join(sginf) %>%
  select(poi_cbg, cbg, month, visits) %>%
  group_by(poi_cbg, cbg, month) %>%
  summarise(visits = sum(visits)) 

##

edges <- 
  unimo %>%
  filter(month == 4) %>%
  transmute(from = cbg,
            to = poi_cbg, 
            weight = visits)

nodes <- 
  shape %>%
  transmute(cbg = shape$GEOID) %>%
  st_drop_geometry()

##

graph <- 
  edges %>%
  graph_from_data_frame(vertices = nodes, directed = TRUE) %>%
  set_edge_attr("weight", value = edges$weight)

##

cents <- 
  tibble(GEOID = V(graph)$name,
         evc = evcent(graph)$vector, 
         deg = degree(graph), 
         bet = betweenness(graph, weights = E(graph)$weight, normalized = TRUE), 
         clo = closeness(graph, weights =  E(graph)$weight),
         ecc = eccentricity(graph))

##

ready <- 
  unimo %>% 
  group_by(month) %>% 
  group_split()

centraliser <- 
  function(x, y) {
    
    edges <- 
      x %>%
      transmute(from = cbg,
                to = poi_cbg, 
                weight = visits)
    
    nodes <- 
      shape %>%
      transmute(cbg = shape$GEOID) %>%
      st_drop_geometry()
    
    graph <- 
      edges %>%
      graph_from_data_frame(vertices = nodes, directed = TRUE) %>%
      set_edge_attr("weight", value = edges$weight)
    
    cents <- 
      tibble(GEOID = V(graph)$name,
             evc = evcent(graph)$vector, 
             deg = degree(graph), 
             bet = betweenness(graph, weights = E(graph)$weight, normalized = TRUE), 
             clo = closeness(graph, weights =  E(graph)$weight),
             ecc = eccentricity(graph)) %>%
      mutate(month = unique(x$month))
    
    return(cents)
    
}

##

cents <- map_df(ready, centraliser)

##

library(gganimate)

##

ready <- 
  cents %>% 
  left_join(shape) %>%
  st_as_sf() %>%
  st_transform(3701) %>%
  mutate(period = month)

##

anim <- 
  ggplot(ready) +
  geom_sf(data = shape %>% st_combine() %>% st_union(),
          aes(), fill = NA, colour = pal[1], lwd = 1) +
  geom_sf(aes(fill = factor(ntile(bet, 5))), colour = NA, lwd = 0) +
  scale_fill_manual(values = pal[6:10],
                    labels = as.character(scientific(quantile(cents$bet,
                                                              c(.1,.2,.4,.6,.8),
                                                              na.rm = TRUE), digits = 4)),
                    name = "betweenness",
                    guide = guide_discrete) +
  labs(title = 'Betweenness Centrality by Month', subtitle = "{stamp[current_frame]}") +
  transition_manual(period) +
  theme_map() +
  theme(legend.position = 'bottom',
        legend.text = element_text(size = 4)) 

anim_save("tween.gif", animation = anim, 
          height = 600, width = 800, nframes = 12, fps = 1,
          start_pause = 2, end_pause = 2)

##

anim <- 
  ggplot(ready) +
  geom_sf(data = shape %>% st_combine() %>% st_union(),
          aes(), fill = NA, colour = pal[1], lwd = 1) +
  geom_sf(aes(fill = factor(ntile(evc, 5))), colour = NA, lwd = 0) +
  scale_fill_manual(values = pal[6:10],
                    labels = as.character(scientific(quantile(cents$evc,
                                                              c(.1,.2,.4,.6,.8),
                                                              na.rm = TRUE), digits = 4)),
                    name = "eigenvector",
                    guide = guide_discrete) +
  labs(title = 'Eigenvector Centrality by Month', subtitle = "{stamp[current_frame]}") +
  transition_manual(period) +
  theme_map() +
  theme(legend.position = 'bottom',
        legend.text = element_text(size = 4)) 

anim_save("eigen.gif", animation = anim, 
          height = 600, width = 800, nframes = 12, fps = 1,
          start_pause = 2, end_pause = 2)

##

anim <- 
  ggplot(ready) +
  geom_sf(data = shape %>% st_combine() %>% st_union(),
          aes(), fill = NA, colour = pal[1], lwd = 1) +
  geom_sf(aes(fill = factor(ntile(deg, 5))), colour = NA, lwd = 0) +
  scale_fill_manual(values = pal[6:10],
                    labels = as.character(scientific(quantile(cents$deg,
                                                              c(.1,.2,.4,.6,.8),
                                                              na.rm = TRUE), digits = 4)),
                    name = "degree",
                    guide = guide_discrete) +
  labs(title = 'Degree Centrality by Month', subtitle = "{stamp[current_frame]}") +
  transition_manual(period) +
  theme_map() +
  theme(legend.position = 'bottom',
        legend.text = element_text(size = 4)) 

anim_save("degri.gif", animation = anim, 
          height = 600, width = 800, nframes = 12, fps = 1,
          start_pause = 2, end_pause = 2)

##

anim <- 
  ggplot(ready) +
  geom_sf(data = shape %>% st_combine() %>% st_union(),
          aes(), fill = NA, colour = pal[1], lwd = 1) +
  geom_sf(aes(fill = factor(ntile(clo, 5))), colour = NA, lwd = 0) +
  scale_fill_manual(values = pal[6:10],
                    labels = as.character(scientific(quantile(cents$clo,
                                                              c(.1,.2,.4,.6,.8),
                                                              na.rm = TRUE), digits = 4)),
                    name = "closeness",
                    guide = guide_discrete) +
  labs(title = 'Closeness Centrality by Month', subtitle = "{stamp[current_frame]}") +
  transition_manual(period) +
  theme_map() +
  theme(legend.position = 'bottom',
        legend.text = element_text(size = 4)) 

anim_save("close.gif", animation = anim, 
          height = 600, width = 800, nframes = 12, fps = 1,
          start_pause = 2, end_pause = 2)



