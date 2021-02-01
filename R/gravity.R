####################################
## Philadelphia, simplex
####################################

source("R/query.R")
source("R/package.R")
source("R/help.R")

## getting fips codes, metro or county
codes <- get_codes("philadelphia")
codes <- "'42101'"

## getting node data
nodes <- get_nodes(codes)

## getting edge data 
index <- str_pad(1:12, side = 'left', width = 2, pad = "0")
edges <- map_df(index, function(x) { get_edges(codes, x, nodes$cbg) %>% mutate(month = as.numeric(x)) })

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
lines <- stplanr::od2line(edges, 
                          nodes %>% 
                            st_as_sf(coords = c("X", "Y"), crs = 4326) %>% 
                            st_transform(4269))

flows <- 
  ggplot() +
  geom_sf(data = background, 
          aes(), fill = NA, colour = '#000000', lwd = 0.5) +
  geom_sf(data = lines %>%
            mutate(month = lubridate::month(month, label = TRUE, abbr = FALSE)), 
          aes(colour = factor(ntile(weight, 7)), lwd = weight, alpha = weight)) +
  scale_colour_manual(values = scico::scico(7, palette = 'tokyo'),
                      labels = as.character(quantile(lines$weight,
                                                     c(.2,.3,.4,.5,.6,.7,.8),
                                                     na.rm = TRUE)),
                      name = "visits",
                      guide = guide_discrete) +
  scale_size_continuous(range = c(0.1, 1), guide = 'none') +
  scale_alpha(range = c(0.1, 0.5), guide = 'none') +
  facet_wrap(~ month, ncol = 4) +
  theme_map() +
  theme(legend.position = 'bottom')

ggsave(flows, filename = "flows.png", height = 6, width = 8, dpi = 300)

## getting distances
distances <- 
  lines %>% 
  mutate(distance = units::drop_units(st_length(geometry))) %>% 
  st_drop_geometry() 

## getting demography
income <- 
  vroom("data/census/data/cbg_b19.csv") %>% 
  filter(census_block_group %in% shape$GEOID) %>%
  select(census_block_group, B19301e1, B19301m1) %>% 
  transmute(target = census_block_group,
            median_income = B19301e1)

education <- 
  vroom("data/census/data/cbg_b15.csv") %>% 
  filter(census_block_group %in% shape$GEOID) %>%
  select(census_block_group, B15003e1, B15003e22, B15003m1, B15003m22) %>%
  transmute(target = census_block_group, 
            college_degree = B15003e22 / B15003e1)

size <- 
  vroom("data/census/data/cbg_b25.csv") %>% 
  filter(census_block_group %in% shape$GEOID) %>%
  select(census_block_group, B25010e1) %>%
  transmute(target = census_block_group, 
            household_size = B25010e1)

population <- 
  vroom("data/census/data/cbg_b01.csv") %>% 
  filter(census_block_group %in% shape$GEOID) %>%
  select(census_block_group, B01001e1) %>%
  transmute(target = census_block_group, 
            population = B01001e1)

## poi data for the model
pois <- get_pois(codes, "01")

businesses <- 
  pois %>% 
  group_by(poi_cbg) %>%
  summarise(businesses = n()) %>%
  rename(focal = poi_cbg)

## first constraint
D_j <- distances %>% group_by(focal) %>% summarise(D_j = sum(weight))
O_i <- distances %>% group_by(target) %>% summarise(O_i = sum(weight))

## join it all together
regression <- 
  distances %>%
  filter(focal != target) %>% 
  left_join(population) %>%
  left_join(education) %>%
  left_join(income) %>%
  left_join(size) %>%
  left_join(O_i) %>%
  left_join(D_j) %>%
  left_join(businesses) %>%
  replace_na(list(businesses = 0)) %>%
  as_tibble() %>% 
  drop_na() %>%
  select(-focal, -target)

## testing on a month
temp <- filter(regression, month == 4)

gravity <- 
  glm(log(weight) ~
        log(distance) +  population +
        log(median_income) +  log(businesses + 1) + college_degree + household_size + 
        log(D_j) + log(O_i), family = poisson(link = "log"), 
      data = temp)

temp$predictions <- exp(fitted(gravity))
rsquared(temp$weight, temp$predictions)

summary(gravity)

## every month
fits <- 
  reduce(map(1:12, function(x){
    
    temp <- filter(regression, month == x)
    
    gravity <- 
      glm(log(weight) ~
            log(distance) + 
            population + college_degree + household_size + 
            log(median_income) +  log(businesses + 1) + 
            log(D_j) + log(O_i), family = poisson(link = "log"), data = temp)
    
    temp$predictions <- exp(fitted(gravity))
    
    return(rsquared(temp$weight, temp$predictions))
  }), 
  c
  )

## how does the fit change over time?
ggplot(data = tibble(period = 1:12, 
                     fit = fits), 
       aes(x = period, y = fit)) + 
  geom_step(size = 2, colour = scico::scico(palette = 'tokyo', 9)[8]) + 
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10), labels = lubridate::month(c(2, 4, 6, 8, 10), label = TRUE)) +
  xlab("") +
  ylab("") + 
  labs(title = "Variance Explained Over Time") +
  theme_hor() + 
  ggsave("rsquared_phl.png", height = 6, width = 8, dpi = 300)

## what about coefficients?
results <- 
  reduce(map(1:12, function(x){
    
    temp <- filter(regression, month == x)
    
    gravity <- 
      glm(log(weight) ~
            log(distance) + 
            population + college_degree + household_size + 
            log(median_income) +  log(businesses + 1) + 
            log(D_j) + log(O_i), family = poisson(link = "log"), data = temp)
    
    results <-  
      broom::tidy(gravity) %>% 
      mutate(month = x)
    
    return(results)
  }), 
  rbind
  )

## plot it
ggplot(data = results %>%
         filter(term != "(Intercept)") %>%
         mutate(term = str_replace_all(term, "_", " ")), 
       aes(x = month, y = estimate, colour = term)) + 
  geom_step(size = 2) + 
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10), labels = lubridate::month(c(2, 4, 6, 8, 10), label = TRUE)) +
  scale_colour_manual(values = sample(scico(palette = 'tokyo', 9)[1:8], 8)) +
  facet_wrap(~ term, scales = 'free_y', nrow = 2) + 
  xlab("") +
  ylab("") + 
  labs(title = "Coefficients Over Time") +
  theme_hor() +
  theme(legend.position = 'botoom') +
  ggsave("coefficients_phl.png", height = 6, width = 8, dpi = 300)

## adding rmse
mape <- function(observed,estimated){
  MAPE <- mean(abs(observed - estimated) / observed)
  MAPE
}

fits <- 
  reduce(map(1:12, function(x){
    
    temp <- filter(regression, month == x)
    
    gravity <- 
      glm(log(weight) ~
            log(distance) + 
            population + college_degree + household_size + 
            log(median_income) +  log(businesses + 1) + 
            log(D_j) + log(O_i), family = poisson(link = "log"), data = temp)
    
    temp$predictions <- exp(fitted(gravity))
    
    return(mape(temp$weight, temp$predictions))
  }), 
  c
  )

## how does the fit change over time?
ggplot(data = tibble(period = 1:12, 
                     fit = fits), 
       aes(x = period, y = fit)) + 
  geom_step(size = 2, colour = scico::scico(palette = 'tokyo', 9)[8]) + 
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10), labels = lubridate::month(c(2, 4, 6, 8, 10), label = TRUE)) +
  xlab("") +
  ylab("") + 
  labs(title = "Mean Absolute Percent Error Over Time") +
  theme_hor() + 
  ggsave("mape_phl.png", height = 6, width = 8, dpi = 300)

####################################
## Philadelphia, multiplex
####################################
