########################################
## Regraphing
########################################

source("R/package.R")
source("R/help.R")

##

odmat <- vroom("data/processed/od_monthly.csv")
phila <- read_sf("data/processed/phila.geojson")

##

sginf <- 
  phila %>% 
  transmute(safegraph_place_id = safegraph_place_id,
            locale = location_name, type = top_category, naics = str_sub(naics_code, 1, 2)) %>% 
  st_drop_geometry()

##

stamp <- c("January", "February", "March", "April", "May", "June", "July", "August") 

##

joint <- 
  odmat %>% 
  filter(month == 1) %>%
  left_join(sginf) %>%
  drop_na(naics)

##

links <- transmute(joint, from = cbg, to = safegraph_place_id, weight = visits)

##

color <- 
  joint %>%
  mutate(class = case_when(str_detect(type, "Restaurants|Drinking") ~ "leisure",
                           str_detect(type, "Schools|Child") ~ "school",
                           str_detect(type, "Stores") & str_detect(type, "Food|Grocery|Liquor") ~ "grocery",
                           str_detect(type, "Stores|Dealers") & !str_detect(type, "Food|Grocery|Liquor") ~ "shopping",
                           str_detect(type, "Gasoline Stations|Automotive") ~ "automotive",
                           str_detect(type, "Real Estate") ~ "real Estate",
                           str_detect(type, "Museums|Amusement|Accommodation|Sports|Gambling") ~ "tourism", 
                           str_detect(type, "Offices|Outpatient|Nursing|Home Health|Diagnostic") & !str_detect(type, "Real Estate") ~ "healthcare",
                           str_detect(type, "Care") & str_detect(type, "Personal") ~ "pharmacy",
                           str_detect(type, "Religious") ~ "worship",
                           TRUE ~ "other")) %>%
  group_by(class) %>% 
  summarise(n = sum(visits)) %>%
  arrange(desc(n)) %>% 
  mutate(cmap = sample(pal))

cross <- 
  joint %>%
  mutate(class = case_when(str_detect(type, "Restaurants|Drinking") ~ "leisure",
                           str_detect(type, "Schools|Child") ~ "school",
                           str_detect(type, "Stores") & str_detect(type, "Food|Grocery|Liquor") ~ "grocery",
                           str_detect(type, "Stores|Dealers") & !str_detect(type, "Food|Grocery|Liquor") ~ "shopping",
                           str_detect(type, "Gasoline Stations|Automotive") ~ "automotive",
                           str_detect(type, "Real Estate") ~ "real Estate",
                           str_detect(type, "Museums|Amusement|Accommodation|Sports|Gambling") ~ "tourism", 
                           str_detect(type, "Offices|Outpatient|Nursing|Home Health|Diagnostic") & !str_detect(type, "Real Estate") ~ "healthcare",
                           str_detect(type, "Care") & str_detect(type, "Personal") ~ "pharmacy",
                           str_detect(type, "Religious") ~ "worship",
                           TRUE ~ "other")) %>%
  distinct(type, .keep_all = TRUE) %>%
  select(type, class)

##

verts <- 
  joint %>% 
  distinct(safegraph_place_id, .keep_all = TRUE) %>%
  select(-visits, -cbg) %>% 
  left_join(cross) %>% 
  left_join(color)

##

graph <- graph_from_data_frame(links, directed = TRUE)
V(graph)$type <- bipartite_mapping(graph)$type

comps <- components(graph, mode = "weak")

graph <-
  graph %>% 
  set_vertex_attr("venue", index = V(graph)[type=="TRUE"], value = verts$locale)  %>% 
  set_vertex_attr("description", index = V(graph)[type=="TRUE"], value = verts$class) %>%
  set_vertex_attr("naics", index = V(graph)[type=="TRUE"], value = verts$naics) %>%
  set_vertex_attr("cmap", index = V(graph)[type=="TRUE"], value = verts$cmap) %>%
  set_vertex_attr("venue", index = V(graph)[type=="FALSE"], value = "")  %>% 
  set_vertex_attr("description", index = V(graph)[type=="FALSE"], value = "") %>%
  set_vertex_attr("naics", index = V(graph)[type=="FALSE"], value = "") %>%
  set_vertex_attr("cmap", index = V(graph)[type=="FALSE"], value = '#000000') %>%
  set_vertex_attr("comp", value = comps$membership) %>%
  set_edge_attr("month", value = joint$month)

graph <- simplify(graph)

decom <- decompose(graph, mode = c("weak"),
                   min.vertices = 2)

decom <- decom[[1]]

##

place <- joint %>% filter(locale == "Logan Square") %>% pull(safegraph_place_id) %>% magrittr::extract(1)
place <- joint %>% filter(str_detect(locale, "Comcast") & !str_detect(locale, "XFINITY"))  %>% pull(safegraph_place_id) %>% magrittr::extract(1)

ego <- make_ego_graph(decom, order = 2, nodes = V(decom)[name==place], mode = 'all')
ego <- ego[[1]]

lay <- layout_with_fr(ego)#, weights = E(decom)$weight)
lay <- norm_coords(lay, ymin = -1, ymax = 1, xmin = -1, xmax = 1)

png(file = "test.png", width = 900, height = 900)

plot(
  main = "Logan Square",
  sub = "April",
  ego,
  layout = lay,
  vertex.size = sqrt(degree(ego)),
  vertex.label = NA, # V(ego)$venue,
  vertex.label.color = '#000000',
  edge.arrow.size = 0,
  vertex.color = V(ego)$cmap
)

dev.off()

##

map(1:8, function(x){
  joint <- 
    odmat %>% 
    filter(month == x) %>%
    left_join(sginf) %>%
    drop_na(naics)
  
  links <- transmute(joint,from = cbg, to = safegraph_place_id, weight = visits)
  
  verts <- 
    joint %>% 
    distinct(safegraph_place_id, .keep_all = TRUE) %>%
    select(-visits, -cbg) %>% 
    left_join(cross) %>% 
    left_join(color)
  
  
  graph <- graph_from_data_frame(links, directed = TRUE)
  V(graph)$type <- bipartite_mapping(graph)$type
  
  comps <- components(graph, mode = "weak")
  
  graph <-
    graph %>% 
    set_vertex_attr("venue", index = V(graph)[type=="TRUE"], value = verts$locale)  %>% 
    set_vertex_attr("description", index = V(graph)[type=="TRUE"], value = verts$class) %>%
    set_vertex_attr("naics", index = V(graph)[type=="TRUE"], value = verts$naics) %>%
    set_vertex_attr("cmap", index = V(graph)[type=="TRUE"], value = verts$cmap) %>%
    set_vertex_attr("venue", index = V(graph)[type=="FALSE"], value = "")  %>% 
    set_vertex_attr("description", index = V(graph)[type=="FALSE"], value = "") %>%
    set_vertex_attr("naics", index = V(graph)[type=="FALSE"], value = "") %>%
    set_vertex_attr("cmap", index = V(graph)[type=="FALSE"], value = '#000000') %>%
    set_vertex_attr("comp", value = comps$membership)
  
  graph <- simplify(graph)
  
  decom <- decompose(graph, mode = c("weak"),
                     min.vertices = 2)
  
  decom <- decom[[1]]
  
  place <- 
    joint %>% 
    filter(str_detect(locale, "Fairmount Park")) %>% 
    pull(safegraph_place_id) %>% 
    magrittr::extract() %>%
    unique()
  
  ego <- make_ego_graph(decom, order = 2, nodes = V(decom)[name %in% place], mode = 'all')
  ego <- ego[[1]]
  
  lay <- layout_with_fr(ego)#, weights = E(decom)$weight)
  lay <- norm_coords(lay, ymin = -1, ymax = 1, xmin = -1, xmax = 1)
  
  png(file = glue("{x}.png"), width = 900, height = 900)
  
  plot(
    main = "Fairmount Park",
    sub = glue("{stamp[x]}"),
    ego,
    layout = lay,
    vertex.size = sqrt(degree(ego)),
    vertex.label = NA, # V(ego)$venue,
    vertex.label.color = '#000000',
    edge.arrow.size = 0,
    vertex.color = V(ego)$cmap
  )
  
  dev.off()
  
})

##

library(magick)

##

list.files(path='animations/fairmount', pattern = '*.png', full.names = TRUE) %>% 
  image_read() %>% 
  image_join() %>% 
  image_animate(fps = 1) %>% 
  image_write("fairmount.gif")

##

leisure <- 
  odmat %>% 
  left_join(sginf) %>%
  left_join(cross) %>%
  filter(class == "leisure") %>%
  select(safegraph_place_id, cbg, visits, month)

##

tig <- 
  block_groups("PA", "Philadelphia", cb = TRUE, class = 'sf') %>% 2272 
  st_transform(3702)
  
jan <- 
  leisure %>%
  filter(month == 1) %>%
  filter(cbg %in% tig$GEOID) %>%
  select(-month) %>%
  pivot_wider(names_from = cbg, values_from = visits) %>%
  replace(is.na(.), 0) %>%
  as.data.frame()

rownames(jan) <- jan$safegraph_place_id
jan <- jan[, -1]
jan <- as.matrix(jan)

## Block-to-Block
t(jan) %*% jan

## Venue-to-Venue
jan %*% t(jan) 

## Create graph
net <- graph_from_adjacency_matrix(t(jan) %*% jan, mode = "undirected", diag = FALSE, weighted = TRUE)
diameter(net)

lay <- layout_with_fr(net)
lay <- norm_coords(lay, ymin = -1, ymax = 1, xmin = -1, xmax = 1)

png(file = "test.png", width = 900, height = 900)

plot(
  net,
  layout = lay, 
  vertex.size = degree(graph)/100,
  vertex.label = NA,
  edge.arrow.size = 0,
)

dev.off()

apr <- 
  leisure %>%
  filter(month == 4) %>%
  filter(cbg %in% tig$GEOID) %>%
  select(-month) %>%
  pivot_wider(names_from = cbg, values_from = visits) %>%
  replace(is.na(.), 0) %>%
  as.data.frame()

rownames(apr) <- apr$safegraph_place_id
apr <- apr[, -1]
apr <- as.matrix(apr)

## Block-to-Block
t(apr) %*% apr

## Venue-to-Venue
apr %*% t(apr)

## Create graph
net <- graph_from_adjacency_matrix(t(apr) %*% apr, mode = "undirected", diag = FALSE, weighted = TRUE)
diameter(net)


net <- delete_edges(net, which(E(net)$weight < 100))
net <- delete_vertices(net, which(degree(net) < 1))

decom <- decompose(net, mode = c("weak"),
                   min.vertices = 2)

decom <- decom[[1]]

decom <- 
  decom %>% 
  set_vertex_attr("community", value = cluster_louvain(decom)$membership) 

lay <- layout_with_fr(net)
lay <- norm_coords(lay, ymin = -1, ymax = 1, xmin = -1, xmax = 1)

png(file = "april.png", width = 900, height = 900)

plot(
  net,
  layout = lay, 
  vertex.size = degree(graph)/100,
  vertex.label = NA,
  vertex.color = V(decom)$community,
  palette = pal,
  edge.arrow.size = 0,
)

dev.off()

##

tig %>% 
  left_join(tibble(GEOID = V(decom)$name, 
                   community = V(decom)$community)) %>%
  mutate(community = factor(community)) %>%
  tm_shape() + 
  tm_fill(col = "community",
          pal = pal[2:10]) +
  tm_layout(frame.lwd = 0)

##

map(1:8, function(x){
  
  mon <- 
    leisure %>%
    filter(month == x) %>%
    filter(cbg %in% tig$GEOID) %>%
    select(-month) %>%
    pivot_wider(names_from = cbg, values_from = visits) %>%
    replace(is.na(.), 0) %>%
    as.data.frame()
  
  rownames(mon) <- mon$safegraph_place_id
  mon <- mon[, -1]
  mon <- as.matrix(mon)
  
  net <- graph_from_adjacency_matrix(t(mon) %*% mon, mode = "undirected", diag = FALSE, weighted = TRUE)
  
  net <- delete_edges(net, which(E(net)$weight < 100))
  net <- delete_vertices(net, which(degree(net) < 1))
  
  decom <- decompose(net, mode = c("weak"),
                     min.vertices = 2)
  
  decom <- decom[[1]]
  
  decom <- 
    decom %>% 
    set_vertex_attr("community", value = cluster_louvain(decom)$membership) 
  
  lay <- layout_with_fr(decom)
  lay <- norm_coords(lay, ymin = -1, ymax = 1, xmin = -1, xmax = 1)
  
  png(file = glue("{x}.png"), width = 900, height = 900)
  
  plot(
    main = "Philadelphia Patterns: shared venues between neighborhoods",
    sub = glue("{stamp[x]}"),
    decom,
    layout = lay, 
    vertex.size = degree(decom)/100,
    vertex.label = NA,
    vertex.color = V(decom)$community,
    palette = pal,
    vertex.frame.color = '#000000',
    vertex.frame.width = 0.05,
    edge.arrow.size = 0,
  )
  
  dev.off()
  
})

##

list.files(path = 'miscellany/networks/mode_one', pattern = '*.png', full.names = TRUE) %>% 
  image_read() %>% 
  image_join() %>% 
  image_animate(fps = 1) %>% 
  image_write("mode_one.gif")

##

map(1:8, function(x){
  
  mon <- 
    leisure %>%
    filter(month == x) %>%
    filter(cbg %in% tig$GEOID) %>%
    select(-month) %>%
    pivot_wider(names_from = cbg, values_from = visits) %>%
    replace(is.na(.), 0) %>%
    as.data.frame()
  
  rownames(mon) <- mon$safegraph_place_id
  mon <- mon[, -1]
  mon <- as.matrix(mon)
  
  net <- graph_from_adjacency_matrix(t(mon) %*% mon, mode = "undirected", diag = FALSE, weighted = TRUE)
  
  net <- delete_edges(net, which(E(net)$weight < 100))
  net <- delete_vertices(net, which(degree(net) < 1))
  
  decom <- decompose(net, mode = c("weak"),
                     min.vertices = 2)
  
  decom <- decom[[1]]
  
  decom <- 
    decom %>% 
    set_vertex_attr("community", value = cluster_louvain(decom)$membership) 
  
  lay <- layout_with_fr(decom)
  lay <- norm_coords(lay, ymin = -1, ymax = 1, xmin = -1, xmax = 1)
  
  png(file = glue("miscellany/animations/working/net_{x}.png"), width = 900, height = 900)
  
  plot(
    main = "Philadelphia Integration",
    sub = glue("{stamp[x]}"),
    decom,
    layout = lay, 
    vertex.size = degree(decom)/100,
    vertex.label = NA,
    vertex.color = V(decom)$community,
    palette = pal,
    vertex.frame.color = '#000000',
    vertex.frame.width = 0.05,
    edge.arrow.size = 0,
  )
  
  dev.off()
  
  map <- 
    tig %>% 
    left_join(tibble(GEOID = V(decom)$name, 
                     community = V(decom)$community)) %>%
    mutate(community = factor(community)) %>%
    tm_shape() + 
    tm_fill(col = "community",
            pal = pal[2:10]) +
    tm_layout(frame.lwd = 0,
              legend.show=FALSE)
  
  tmap_save(map, glue("miscellany/animations/working/map_{x}.png"), width = 600, height = 900, unit = 'px')
  
  list.files(path = 'miscellany/animations/working', pattern = glue("*{x}.png"), full.names = TRUE) %>% 
    image_read() %>% 
    image_join() %>% 
    image_append() %>% 
    image_write(glue("{x}.png"))
  
})

list.files(path = 'miscellany/animations/combined', pattern = "*.png", full.names = TRUE) %>% 
  image_read() %>% 
  image_join() %>% 
  image_animate(fps = 1) %>% 
  image_write(glue("combined.gif"))

show_col(pal)

##

ggplot(data = color) +
  geom_tile(aes(x = class, y = 1, fill = cmap)) +
  geom_text(aes(x = class, y = 1, label = tolower(class))) +
  geom_text(data = 
              color %>% 
              filter(class == "worship"),
            aes(x = class, y = 1, label = tolower(class)), colour = '#ffffff') +  
  scale_fill_identity() +
  coord_equal() +
  theme_void() +
  ggsave("cmap.png")

##

diameter(net)

##

