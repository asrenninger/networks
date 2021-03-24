n <- 5:100

## Probabilities
png(file = "probabilities.png", width = 900, height = 500)

par(mfrow = c(1, 1), bg = 'white')
plot(n, (log(n) / n), type = "b", frame = FALSE, pch = 19, 
     col = "red", xlab = "", ylab = "", ylim = c(0, 0.4))

lines(n, (1 / n^1.0), pch = 18, col = "blue", type = "b", lty = 2)
lines(n, (1 / n^1.5), pch = 17, col = "green", type = "b", lty = 2)
lines(n, (1 / n^2.0), pch = 16, col = "yellow", type = "b", lty = 2)

legend("top", legend = c("log(n) / n", "1 / n ^ 1.0", "1 / n ^ 1.5", "1 / n ^ 2.0"),
       col = c("red", "blue", "green", "yellow"), lty = 1:2, cex = 0.8)

dev.off()

k <- 100

## Threshold 1
## The point at which an edge will appear
png(file = "thresholds_edges.png", width = 900, height = 500)

par(mfrow = c(1, 2))

plot(igraph::erdos.renyi.game(n = k, p.or.m = (1 / k^2.00)), 
     main = "1 / n ^ 2.00",
     layout = igraph::layout_with_fr,
     vertex.size = 1,
     vertex.label = NA,
     vertex.label.color = '#000000',
     edge.arrow.size = 0)

plot(igraph::erdos.renyi.game(n = k, p.or.m = (1 / k^1.50)), 
     main = "1 / n ^ 1.50",
     layout = igraph::layout_with_fr,
     vertex.size = 1,
     vertex.label = NA,
     vertex.label.color = '#000000',
     edge.arrow.size = 0)

mtext("Threshold for Edges", side = 1, outer = TRUE, line = -3)

dev.off()

## Threshold 2
## The point at which a component will appear
png(file = "thresholds_components.png", width = 900, height = 500)

par(mfrow = c(1, 2))

plot(igraph::erdos.renyi.game(n = k, p.or.m = (1 / k^1.50)), 
     main = "1 / n ^ 1.50",
     layout = igraph::layout_with_fr,
     vertex.size = 1,
     vertex.label = NA,
     vertex.label.color = '#000000',
     edge.arrow.size = 0)

plot(igraph::erdos.renyi.game(n = k, p.or.m = (1 / k^1.00)), 
     main = "1 / n ^ 1.00",
     layout = igraph::layout_with_fr,
     vertex.size = 1,
     vertex.label = NA,
     vertex.label.color = '#000000',
     edge.arrow.size = 0)

mtext("Threshold for Components", side = 1, outer = TRUE, line = -3)

dev.off()

## Threshold 3
## The point at which a cycle will appear
png(file = "thresholds_cycles.png", width = 900, height = 500)

par(mfrow = c(1, 2))

plot(igraph::erdos.renyi.game(n = k, p.or.m = (1 / k^1.0)), 
     main = "1 / n ^ 1.00",
     layout = igraph::layout_with_fr,
     vertex.size = 1,
     vertex.label = NA,
     vertex.label.color = '#000000',
     edge.arrow.size = 0)

plot(igraph::erdos.renyi.game(n = k, p.or.m = (1 / k^0.98)), 
     main = "1 / n ^ 0.98",
     layout = igraph::layout_with_fr,
     vertex.size = 1,
     vertex.label = NA,
     vertex.label.color = '#000000',
     edge.arrow.size = 0)

mtext("Threshold for Cycles", side = 1, outer = TRUE, line = -3)

dev.off()

## Threshold 4
## The point at which the graph will be connected
png(file = "thresholds_connection.png", width = 900, height = 500)

par(mfrow = c(1, 2))

plot(igraph::erdos.renyi.game(n = k, p.or.m = (log(k) / k^1.10)), 
     main = "log(n) / n ^ 1.10",
     layout = igraph::layout_with_fr,
     vertex.size = 1,
     vertex.label = NA,
     vertex.label.color = '#000000',
     edge.arrow.size = 0)

plot(igraph::erdos.renyi.game(n = k, p.or.m = (log(k) / k^1.00)), 
     main = "log(n) / n ^ 1.00",
     layout = igraph::layout_with_fr,
     vertex.size = 1,
     vertex.label = NA,
     vertex.label.color = '#000000',
     edge.arrow.size = 0)

mtext("Threshold for Complete Connection", side = 1, outer = TRUE, line = -3)

dev.off()

## Testing the critical level
png(file = "lln_components.png", width = 1100, height = 900)

par(mfrow = c(2, 5))
for (i in seq(10, 100, by = 10)){
  
  print(i)
  plot(igraph::erdos.renyi.game(n = i, p.or.m = (1 / i^1.50)), 
       main = paste("n =", i),
       layout = igraph::layout_with_fr,
       vertex.size = 5,
       vertex.label = NA,
       vertex.color = 'black',
       vertex.label.color = '#000000',
       edge.arrow.size = 0)
  
}

mtext("Threshold for Components at Various N", side = 1, outer = TRUE, line = -3)

dev.off()

png(file = "lln_connections.png", width = 1100, height = 900)

par(mfrow = c(2, 5))
for (i in seq(10, 100, by = 10)){
  
  print(i)
  plot(igraph::erdos.renyi.game(n = i, p.or.m = (log(i) / i^1)), 
       main = paste("n =", i),
       layout = igraph::layout_with_fr,
       vertex.size = 5,
       vertex.label = NA,
       vertex.color = 'black',
       vertex.label.color = '#000000',
       edge.arrow.size = 0)

}

mtext("Threshold for Complete Connection at Various N", side = 1, outer = TRUE, line = -3)

dev.off()

k = 1000

## degree distributions
png(file = "degree_distributions.png", width = 1100, height = 900)

par(mfrow = c(2, 2))

plot(density(igraph::degree(igraph::erdos.renyi.game(n = k, p.or.m = (log(k) / k^1.0)))), 
     main = "log(n) / n ^ 1.0", ylim = c(0, 0.15), xlim = c(0, 500))
plot(density(igraph::degree(igraph::erdos.renyi.game(n = k, p.or.m = (log(k) / k^0.8)))),
     main = "log(n) / n ^ 0.8", ylim = c(0, 0.15), xlim = c(0, 500))
plot(density(igraph::degree(igraph::erdos.renyi.game(n = k, p.or.m = (log(k) / k^0.6)))),
     main = "log(n) / n ^ 0.6", ylim = c(0, 0.15), xlim = c(0, 500))
plot(density(igraph::degree(igraph::erdos.renyi.game(n = k, p.or.m = (log(k) / k^0.4)))),
     main = "log(n) / n ^ 0.4", ylim = c(0, 0.15), xlim = c(0, 500))

dev.off()