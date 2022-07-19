###################################
## Loubar vs Average
###################################
# The goal:
## identify hotspots in a dataset 
##
## method 1: 
## p > mean(p) is hotspot
##
## method 2: 
## loubar approach
################################### 

## loubar
# lorenz curve
lc <- ineq::Lc(population$estimate)

# slope of the line and intercepts for the last segment
y_y <- lc$L[c(length(lc$L) - 1, length(lc$L))]
x_x <- lc$p[c(length(lc$L) - 1, length(lc$L))]

coord_1 <- c(x_x[1], y_y[1])
coord_2 <- c(x_x[2], y_y[2])

rise <- coord_2[2] - coord_1[2]
run <- coord_2[1] - coord_1[1]  

coord_1[2] = ((rise / run) * coord_1[1]) + b

b <- coord_1[2] - ((rise / run) * coord_1[1])
x_int <- (0 - b) / (rise / run)
distr <- ecdf(population$estimate)

# lorenz curve
plot(lc$p, lc$L, type = 'l', lwd = 2)
# this is the simple threshold
abline(v = distr(mean(population$estimate)), col = "blue", lwd = 2, lty = 2)
# this x intercept is the loubar value
abline(a = b, b = (rise / run), col = 'red', lwd = 2)

# get loubar value from the ecdf
get_loubar <- function(x, x_int){
  
  p_i <- which(round(cumsum(tabulate(match(sort(x), unique(x)))) / length(x), 3) == round(x_int, 3))
  sorted <- sort(unique(x))
  return(sorted[p_i[1]])
  
}

hist(population$estimate, breaks = 50)
abline(v = mean(population$estimate), col = "blue", lwd = 2, lty = 2)
abline(v = population$estimate[get_loubar(population$estimate, x_int)], col = 'red', lwd = 2)

# thresholds
avg <- mean(population$estimate)
lbr <- get_loubar(population$estimate, x_int)

# wrap it up
get_hotspots <- function(variable){
  
  lc <- ineq::Lc(variable)
  
  # slope of the line and intercepts for the last segment
  y_y <- lc$L[c(length(lc$L) - 1, length(lc$L))]
  x_x <- lc$p[c(length(lc$L) - 1, length(lc$L))]
  
  coord_1 <- c(x_x[1], y_y[1])
  coord_2 <- c(x_x[2], y_y[2])
  
  rise <- coord_2[2] - coord_1[2]
  run <- coord_2[1] - coord_1[1]  
  
  b <- coord_1[2] - ((rise / run) * coord_1[1])
  x_int <- (0 - b) / (rise / run)
  distr <- ecdf(variable)
  
  # lorenz curve
  plot(lc$p, lc$L, type = 'l', lwd = 2)
  # this is the simple threshold
  abline(v = distr(mean(variable)), col = "blue", lwd = 2, lty = 2)
  # this x intercept is the loubar value
  abline(a = b, b = (rise / run), col = 'red', lwd = 2)
  
  # get loubar value from the ecdf
  get_loubar <- function(x, x_int){
    
    p_i <- which(round(cumsum(tabulate(match(sort(x), unique(x)))) / length(x), 3) == round(x_int, 3))
    sorted <- sort(unique(x))
    return(sorted[p_i[1]])
    
  }
  
  hist(variable, breaks = 50)
  abline(v = mean(variable), col = "blue", lwd = 2, lty = 2)
  abline(v = variable[get_loubar(variable, x_int)], col = 'red', lwd = 2)
  
  # thresholds
  avg <- mean(variable)
  lbr <- get_loubar(variable, x_int)
  
  return(list(avg, lbr))
  
}
