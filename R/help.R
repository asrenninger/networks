########################################
## Helpers
########################################

## Functions 

## Palettes

# Base
pal <- scico::scico(9, palette = 'hawaii')

## Themes

# Temporal
theme_hor <- function () {
  theme_minimal() +
    theme(plot.background = element_rect(fill = 'transparent', colour = 'transparent'),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(size = 0.1, colour = 'black'),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line.x = element_line(size = 0.5, colour = 'black'),
          axis.line.y = element_blank(),
          axis.ticks.x = element_line(size = 0.5, colour = 'black'),
          axis.ticks.y = element_line(size = 0.1, colour = 'black'),
          axis.text.x = element_text(face = 'bold'),
          axis.text.y = element_text(face = 'bold'),
          plot.title = element_text(face = 'bold', colour = 'black', hjust = 0.5),
          plot.subtitle =  element_text(face = 'plain', colour = 'black', size = 15, hjust = 0.5),
          strip.text = element_text(face = 'bold', colour = 'black'),
          plot.margin = margin(20, 20, 20, 20)
    )
}

# Nontemporal (horizontal)
theme_rot <- function () {
  theme_minimal() +
    theme(plot.background = element_rect(fill = 'transparent', colour = 'transparent'),
          panel.grid.major.x = element_line(size = 0.1, colour = 'black'),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line.y = element_line(size = 0.5, colour = 'black'),
          axis.line.x = element_blank(),
          axis.ticks.y = element_line(size = 0.5, colour = 'black'),
          axis.ticks.x = element_line(size = 0.1, colour = 'black'),
          axis.text.x = element_text(face = 'bold'),
          axis.text.y = element_text(face = 'bold'),
          plot.title = element_text(face = 'bold', colour = 'black', hjust = 0.5),
          plot.subtitle =  element_text(face = 'plain', colour = 'black', size = 15, hjust = 0.5),
          strip.text = element_text(face = 'bold', colour = 'black'),
          plot.margin = margin(20, 20, 20, 20)
    )
}

# Scatter and point
theme_ver <- function () {
  theme_minimal() +
    theme(plot.background = element_rect(fill = 'transparent', colour = 'transparent'),
          panel.grid.major.x = element_line(size = 0.1, colour = 'grey50'),
          panel.grid.major.y = element_line(size = 0.1, colour = 'grey50'),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line.y = element_line(size = 0.5, colour = 'black'),
          axis.line.x = element_blank(),
          axis.ticks.y = element_line(size = 0.5, colour = 'black'),
          axis.ticks.x = element_line(size = 0.1, colour = 'grey50'),
          axis.text.x = element_text(face = 'bold'),
          axis.text.y = element_text(face = 'bold'),
          plot.title = element_text(face = 'bold', colour = 'black', hjust = 0.5),
          plot.subtitle =  element_text(face = 'plain', colour = 'black', size = 15, hjust = 0.5),
          strip.text = element_text(face = 'bold', colour = 'black'),
          plot.margin = margin(20, 20, 20, 20)
    )
}

theme_black <- function () {
  theme_void() + 
    theme(plot.background = element_rect(fill = 'black', colour = 'black'),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.title = element_text(colour = 'black'),
          legend.text = element_text(colour = 'white'),
          plot.title = element_text(face = 'bold', colour = 'black'),
          plot.subtitle =  element_text(face = 'plain', colour = 'white', size = 15),
          panel.grid.major = element_line(size = NA), 
          panel.grid.minor = element_line(size = NA),
          legend.position = c(0.2, 0.8),
          plot.margin = margin(20, 20, 20, 20)
    )
  
}

theme_map <- function () {
  theme_void() + 
    theme(plot.background = element_rect(fill = 'transparent', colour = 'transparent'),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.title = element_text(colour = 'black'),
          legend.text = element_text(colour = 'black'),
          plot.title = element_text(face = 'bold', colour = 'black', hjust = 0.5),
          plot.subtitle =  element_text(face = 'plain', colour = 'black', size = 15, hjust = 0.5),
          panel.grid.major = element_line(size = NA), 
          panel.grid.minor = element_line(size = NA),
          legend.position = c(0.8, 0.2),
          plot.margin = margin(20, 20, 20, 20)
    )
  
}

## Guides

# Continuous
guide_continuous <- 
  guide_colorbar(direction = "horizontal",
                 barheight = unit(2, units = "mm"),
                 barwidth = unit(50, units = "mm"),
                 draw.ulim = FALSE,
                 title.position = 'top',
                 label.position = 'bottom',
                 title.hjust = 0.5,
                 label.hjust = 0.5)

# Discrete
guide_discrete <-
  guide_legend(direction = "horizontal",
               keyheight = unit(2, units = "mm"),
               keywidth = unit(10, units = "mm"),
               title.position = 'top',
               label.position = 'bottom',
               title.hjust = 0.5,
               label.hjust = 1,
               nrow = 1,
               byrow = TRUE)

## Plotting

# Correlation Plotting
correlate <- function(correlations, name) {
  
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
    scico::scale_colour_scico(palette = 'turku', direction = -1,
                       guide = 'none') +
    scico::scale_fill_scico(palette = 'turku',
                     limit = c(0.7, 1), 
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
    geom_text(aes(Var2, Var1, label = value, colour = value), size = 3) 
  
  ggsave(ggmatrix, filename = name, height = 8, width = 8, dpi = 300)
  
  return(ggmatrix)
  
}

# Discrete plotting
discreter <- function(values, breaks) {
  quantise <- ntile(values, breaks)
  factorise <- as.factor(quantise)
  
  return(factorise)
}

labeller <- function(values, max) {
  quantise <- quantile(values,
                       c(.1,.2,.3,.4,.5,.6,.7,.8,.9),
                       na.rm = TRUE)
  
  characterise <- as.character(quantise)
  miniaturise <- substr(characterise, 1, max)
  
  return(miniaturise)
}

## Wrappers

# magrittr
use <- function(label) { return(magrittr::use_series(label)) }



