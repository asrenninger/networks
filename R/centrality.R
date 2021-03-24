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
            weight = visits) %>%
  filter(from %in% shape$GEOID)

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
  filter(cbg %in% shape$GEOID) %>%
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

labels <- 
  odmat %>%
  mutate(label = lubridate::month(start, label = TRUE, abbr = FALSE)) %>%
  select(month, label) %>%
  distinct(month, .keep_all = TRUE)

ggplot() +
  geom_sf(data = race %>%
            mutate(variable = str_to_title(variable)) %>%
            mutate(percent = 100 * (value / summary_value)), 
          aes(fill = percent), lwd = 0) + 
  geom_sf(data = 
            cents %>%
            left_join(shape) %>%
            st_as_sf() %>%
            group_by(month, inf) %>%
            summarise() %>%
            left_join(labels), aes(), lwd = 1, fill = NA) +
  scale_fill_gradientn(colours = rev(pal), 
                       guide = guide_continuous) +
  facet_grid(variable ~ label) +
  #labs(title = "Communities by Month",
  #     subtitle = "Detected mobility clusters over demographic compositon") + 
  theme_map() +
  theme(legend.position = 'bottom',
        strip.text.y = element_text(angle = 270)) + 
  ggsave("communitiesxrace_clean.png", height = 8, width = 11, dpi = 300)

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

##

anim <- 
  ggplot(ready) +
  geom_sf(data = shape %>% st_combine() %>% st_union(),
          aes(), fill = NA, colour = pal[1], lwd = 1) +
  geom_sf(aes(fill = factor(ntile(prc, 5))), colour = NA, lwd = 0) +
  scale_fill_manual(values = pal[6:10],
                    labels = as.character(scientific(quantile(cents$prc,
                                                              c(.1,.2,.4,.6,.8),
                                                              na.rm = TRUE), digits = 4)),
                    name = "pagerank",
                    guide = guide_discrete) +
  labs(title = 'PageRank by Month', subtitle = "{stamp[current_frame]}") +
  transition_manual(period) +
  theme_map() +
  theme(legend.position = 'bottom',
        legend.text = element_text(size = 4)) 

anim_save("page.gif", animation = anim, 
          height = 600, width = 800, nframes = 12, fps = 1,
          start_pause = 2, end_pause = 2)

##

coord <-
  shape %>% 
  st_transform(3701) %>%
  st_centroid() %>%
  st_coordinates() %>%
  as_tibble() %>%
  bind_cols(shape)
  
dizzy <- 
  shape %>% 
  st_transform(3701) %>% 
  st_union() %>% 
  st_combine()

##

mapit <- function(id){
  
  locater <-
    ggplot() +
    geom_point(data = coord %>% 
                 filter(GEOID == id), 
               aes(x = X, y = Y), 
               size = 20, colour = '#000000') +
    geom_sf(data = dizzy, aes(), fill = NA, colour = '#000000', lwd = 5) +
    theme_map()
  
  return(locater)
  
}

spark <- function(df){
  
  sparkline <- 
    ggplot(data = df, 
           aes(x = month, y = deg)) +
    geom_line(colour = rev(pal[1:9])[df$decile[1]], size = 10) +
    theme_void()
  
  return(sparkline)
  
}  

##

plots <- 
  ready %>%
  select(GEOID, month, deg) %>%
  st_drop_geometry() %>%
  group_by(GEOID) %>%
  mutate(decile = ntile(mean(deg), 9)) %>%
  ungroup() %>%
  group_by(GEOID) %>%
  nest() %>%
  mutate(plot = map(data, spark)) %>%
  select(-data)

maps <- 
  ready %>%
  st_drop_geometry() %>%
  distinct(GEOID) %>%
  mutate(map = map(GEOID, mapit))

##

top10 <-
  ready %>%
  st_drop_geometry() %>%
  group_by(GEOID) %>%
  summarise(betweenness = mean(bet),
            closeness = mean(clo),
            degree = mean(deg)) %>%
  left_join(maps) %>%
  left_join(plots) %>%
  select(map, GEOID, betweenness, closeness, degree, plot) %>%
  arrange(desc(degree)) %>%
  slice(1:10)

top10_plots <- select(top10, GEOID, plot, map)
top10 <- select(top10, GEOID, betweenness, closeness, degree)

bot10 <-
  ready %>%
  st_drop_geometry() %>%
  group_by(GEOID) %>%
  summarise(betweenness = mean(bet),
            closeness = mean(clo, na.rm = TRUE),
            degree = mean(deg)) %>%
  left_join(maps) %>%
  left_join(plots) %>%
  select(map, GEOID, betweenness, closeness, degree, plot) %>%
  arrange(degree) %>%
  slice(1:10)

bot10_plots <- select(bot10, GEOID, plot, map)
bot10 <- select(bot10, GEOID, betweenness, closeness, degree)

##

library(gt)

##

bot10 %>% 
  mutate(ggplot = NA, ggmap = NA) %>%
  select(ggmap, GEOID, betweenness, closeness, degree, ggplot) %>%
  gt() %>% 
  tab_header(title = html("<b>Neighborhood Centrality: bottom ten</b>"),
             subtitle = md("Highest betweenness and degree centrality by block group<br><br>")) %>%
  tab_source_note(source_note = md("**Data**: SafeGraph | **Note**: Period spanning January to August 2020"))  %>% 
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_column_labels(vars(`GEOID`))) %>% 
  cols_label(ggmap = "") %>% 
  text_transform(locations = cells_body(columns = vars(`ggmap`)),
                 fn = function(x) {map(bot10_plots$map, ggplot_image, height = px(30), aspect_ratio = 1)}) %>%
  data_color(columns = vars(`betweenness`, `closeness`, `degree`),
             colors = scales::col_numeric(c(pal[6:10]), domain = NULL)) %>% 
  cols_align(align = "center",
             columns = 2:5) %>% 
  opt_table_font(font = list(c("IBM Plex Sans"))) %>% 
  tab_options(heading.title.font.size = 30,
              heading.subtitle.font.size = 15,
              heading.align = "left",
              table.border.top.color = "white",
              heading.border.bottom.color = "white",
              table.border.bottom.color = "white",
              column_labels.border.bottom.color = "grey",
              column_labels.border.bottom.width= px(1)) %>% 
  cols_label(ggplot = "trend") %>% 
  text_transform(locations = cells_body(columns = vars(`ggplot`)),
                 fn = function(x) {map(bot10_plots$plot, ggplot_image, height = px(20), aspect_ratio = 5)}) %>%
  gtsave("worst.png", expand = 10)

top10 %>% 
  mutate(ggplot = NA, ggmap = NA) %>%
  select(ggmap, GEOID, betweenness, closeness, degree, ggplot) %>%
  gt() %>% 
  tab_header(title = html("<b>Neighborhood Centrality: top ten</b>"),
             subtitle = md("Lowest betweenness and degree centrality by block group<br><br>")) %>%
  tab_source_note(source_note = md("**Data**: SafeGraph | **Note**: Period spanning January to August 2020"))  %>% 
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_column_labels(vars(`GEOID`))) %>% 
  cols_label(ggmap = "") %>% 
  text_transform(locations = cells_body(columns = vars(`ggmap`)),
                 fn = function(x) {map(top10_plots$map, ggplot_image, height = px(30), aspect_ratio = 1)}) %>%
  data_color(columns = vars(`betweenness`, `closeness`, `degree`),
             colors = scales::col_numeric(c(pal[6:10]), domain = NULL)) %>% 
  cols_align(align = "center",
             columns = 2:3) %>% 
  opt_table_font(font = list(c("IBM Plex Sans"))) %>% 
  tab_options(heading.title.font.size = 30,
              heading.subtitle.font.size = 15,
              heading.align = "left",
              table.border.top.color = "white",
              heading.border.bottom.color = "white",
              table.border.bottom.color = "white",
              column_labels.border.bottom.color = "grey",
              column_labels.border.bottom.width= px(1)) %>% 
  cols_label(ggplot = "trend") %>% 
  text_transform(locations = cells_body(columns = vars(`ggplot`)),
                 fn = function(x) {map(top10_plots$plot, ggplot_image, height = px(20), aspect_ratio = 5)}) %>%
  gtsave("best.png", expand = 10)

##

d <- 
  demos %>%
  mutate(pct = 100 * (value / summary_value)) %>%
  st_drop_geometry() %>%
  select(GEOID, variable, pct) %>%
  pivot_wider(id_cols = GEOID, names_from = variable, values_from = pct) %>%
  mutate(quintile = factor(ntile(Black, 5))) %>%
  right_join(cents) %>%
  group_by(quintile, month) %>%
  summarise(betweenness = mean(bet),
            closeness = mean(clo),
            degree = mean(deg)) %>%
  drop_na(quintile) %>%
  ggplot(aes(x = month, y = degree, colour = quintile)) +
  geom_line(size = 2) +
  scale_colour_manual(values = pal[2:6],
                      guide = guide_discrete,
                      name = "quintitle, percent black") +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8), labels = c("J", "F", "M", "A", "M", "J", "J", "A")) +
  xlab("") +
  theme_hor() +
  theme(legend.position = 'bottom')
  

b <- 
  demos %>%
  mutate(pct = 100 * (value / summary_value)) %>%
  st_drop_geometry() %>%
  select(GEOID, variable, pct) %>%
  pivot_wider(id_cols = GEOID, names_from = variable, values_from = pct) %>%
  mutate(quintile = factor(ntile(Black, 5))) %>%
  right_join(cents) %>%
  group_by(quintile, month) %>%
  summarise(betweenness = mean(bet),
            closeness = mean(clo),
            degree = mean(deg)) %>%
  drop_na(quintile) %>%
  ggplot(aes(x = month, y = betweenness, colour = quintile)) +
  geom_line(size = 2) +
  scale_colour_manual(values = pal[2:6],
                      guide = guide_discrete,
                      name = "quintile, percent black") +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8), labels = c("J", "F", "M", "A", "M", "J", "J", "A")) +
  xlab("") +
  theme_hor() +
  theme(legend.position = 'bottom')

##

library(patchwork)

##

p <- d / b + plot_layout(guides = "collect") & theme(legend.position = 'bottom')

ggsave(p, filename = "relationships.png", height = 8, width = 10)

##

par(mfrow=c(2,4))

for (i in 1:8) {
  plot(ecdf(log(cents[which(cents$month==i), ][which(cents$bet!=0), ]$deg)))
}

##

anim <- 
  ggplot(data = cents, aes(x = deg, colour = factor(month))) +
  stat_ecdf(size = 2) +
  scale_color_manual(values = pal, name = 'month', guide = guide_discrete) +
  scale_x_log10() +
  ylab("") +
  xlab("degree centrality") +
  labs(title = "Changing Degrees", subtitle = "Cumulative probability in {stamp[current_frame]}") +
  theme_ver() +
  theme(legend.position = 'bottom') +
  transition_manual(month, cumulative = TRUE) +
  ease_aes() +
  enter_grow()
 
anim_save("ecdf.gif", animation = anim, 
          height = 600, width = 800, nframes = 12, fps = 1,
          start_pause = 2, end_pause = 2)

ggplot(data = cents, aes(x = deg, colour = factor(month))) +
  stat_ecdf(size = 2, alpha = 0.75) +
  scale_color_manual(values = pal, name = 'month', guide = guide_discrete) +
  scale_x_log10() +
  ylab("cummulative probability") +
  xlab("degree centrality") +
  theme_ver() +
  theme(legend.position = 'bottom') +
  ggsave("ecdf.png", height = 6, width = 6, dpi = 300)

## 

library(gt)

##

wide <- 
  race %>% 
  select(GEOID, variable, value) %>%
  st_drop_geometry() %>% 
  pivot_wider(id_cols = GEOID, names_from = variable, values_from = value) %>%
  transmute(GEOID = GEOID,
            white = white,
            nonwhite = black + hispanic)

race_split <- 
  cents %>% 
  transmute(GEOID = GEOID, month = month, infomap = inf) %>%
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

cents %>%
  group_by(inf, month) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(month) %>%
  summarise(max = max(n),
            min = min(n),
            mean = mean(n)) %>%
  mutate(`dissimilarity index` = reduce(list, c)) %>%
  gt() %>% 
  #tab_header(title = html("<b>Community Size by Month</b>"),
  #           subtitle = md("How segregation and community bounds relate<br><br>")) %>%
  #tab_source_note(source_note = md("**Data**: SafeGraph / Census Bureau | **Note**: Period spanning January to August 2020"))  %>% 
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_column_labels(vars(`month`))) %>% 
  data_color(columns = vars(`dissimilarity index`),
             colors = scales::col_numeric(rev(pal), domain = NULL)) %>% 
  cols_align(align = "center",
             columns = 2:5) %>% 
  opt_table_font(font = list(c("IBM Plex Sans"))) %>% 
  tab_options(heading.title.font.size = 30,
              heading.subtitle.font.size = 15,
              heading.align = "left",
              table.border.top.color = "white",
              heading.border.bottom.color = "white",
              table.border.bottom.color = "white",
              column_labels.border.bottom.color = "grey",
              column_labels.border.bottom.width= px(1)) %>% 
  gtsave("dissimilarity.png", expand = 10)

##

density <- 
  purrr::map(ready, function(x){
    
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
  
  return(igraph::graph.density(graph, loop = FALSE))
  })

diameter <- 
  purrr::map(ready, function(x){
    
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
    
    return(igraph::diameter(graph))
  })

tibble(month = stamp,
       diameter = reduce(diameter, c),
       density = reduce(density, c)) %>%
  gt() %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_column_labels(vars(`month`))) %>%
  gtsave("density.png", expand = 10)

##


library(sna)

##

zeros <- array(dim = c(8, 1336, 1336))

##

for (i in 1:8) {
  
  edges <- 
    ready[[i]] %>%
    transmute(from = cbg,
              to = poi_cbg, 
              weight = visits) %>%
    filter(from %in% shape$GEOID) %>%
    arrange(to, from)
  
  adjacencies <- 
    edges %>%
    graph_from_data_frame(vertices = nodes, directed = FALSE) %>%
    set_edge_attr("weight", value = edges$weight) %>% 
    as_adjacency_matrix(attr = "weight") %>% 
    as.matrix()
  
  zeros[i, , ] <- adjacencies
  
}

##

months <- odmat %>% pull(start) %>% lubridate::month(label = TRUE, abbr = FALSE) %>% unique()

##

correlations <- sna::gcor(zeros)

mean(correlations[lower.tri(correlations)])
min(correlations[lower.tri(correlations)])
max(correlations[lower.tri(correlations)])

unique(correlations[lower.tri(correlations)])

##

rownames(correlations) <- months[1:8]
colnames(correlations) <- months[1:8]

##

correlate(correlations, "testing.png")

qap <- qaptest(list(zeros[1, , ], zeros[4, , ]), gcor, g1=1, g2=2, reps=1000)

ggplot() +
  geom_histogram(aes(qap$dist), bins = 500, fill = '#707070') +
  geom_vline(xintercept = unique(correlations[lower.tri(correlations)]), colour = pal[1], linetype = 2, size = 0.5) +
  geom_text(aes(x = 0.575, y = 100, label = "observed"), angle = 90, fontface = 'bold', colour = pal[1]) +
  xlab("permuted correlations") +
  ylab("") +
  theme_hor() +
  ggsave("qap.png", height = 4, width = 6, dpi = 300)

plot(test)

##
