n <- 10:1000

## Probabilities
par(mfrow = c(4, 4))

plot(n, (log(n) / n))
plot(n, (1 / n^1))
plot(n, (1 / n^2))
plot(n, (1 / n^3))
plot(n, (log(n)))
plot(n, ((log(n)^2) / n))
plot(n, (log(n) / (n)^2))

k <- 100

## Threshold 1
## The point at which an edge will appear
par(mfrow = c(1, 2))

plot(igraph::erdos.renyi.game(n = k, p.or.m = (1 / k^2.00)), 
     main = "1 / n ^ 2.00",
     layout = layout_with_fr,
     vertex.size = 1,
     vertex.label = NA,
     vertex.label.color = '#000000',
     edge.arrow.size = 0)

plot(igraph::erdos.renyi.game(n = k, p.or.m = (1 / k^1.50)), 
     main = "1 / n ^ 1.50",
     layout = layout_with_fr,
     vertex.size = 1,
     vertex.label = NA,
     vertex.label.color = '#000000',
     edge.arrow.size = 0)

mtext("Threshold for Edges", side = 1, outer = TRUE, line = -3)

## Threshold 2
## The point at which a component will appear
par(mfrow = c(1, 2))

plot(igraph::erdos.renyi.game(n = k, p.or.m = (1 / k^1.50)), 
     main = "1 / n ^ 1.50",
     layout = layout_with_fr,
     vertex.size = 1,
     vertex.label = NA,
     vertex.label.color = '#000000',
     edge.arrow.size = 0)

plot(igraph::erdos.renyi.game(n = k, p.or.m = (1 / k^1.00)), 
     main = "1 / n ^ 1.00",
     layout = layout_with_fr,
     vertex.size = 1,
     vertex.label = NA,
     vertex.label.color = '#000000',
     edge.arrow.size = 0)

mtext("Threshold for Components", side = 1, outer = TRUE, line = -3)

## Threshold 3
## The point at which a cycle will appear
par(mfrow = c(1, 2))

plot(igraph::erdos.renyi.game(n = k, p.or.m = (1 / k^1.0)), 
     main = "1 / n ^ 1.00",
     layout = layout_with_fr,
     vertex.size = 1,
     vertex.label = NA,
     vertex.label.color = '#000000',
     edge.arrow.size = 0)

plot(igraph::erdos.renyi.game(n = k, p.or.m = (1 / k^0.98)), 
     main = "1 / n ^ 0.98",
     layout = layout_with_fr,
     vertex.size = 1,
     vertex.label = NA,
     vertex.label.color = '#000000',
     edge.arrow.size = 0)

mtext("Threshold for Cycles", side = 1, outer = TRUE, line = -3)

## Threshold 4
## The point at which the graph will be connected
par(mfrow = c(1, 2))

plot(igraph::erdos.renyi.game(n = k, p.or.m = (log(k) / k^1.10)), 
     main = "log(n) / n ^ 1.10",
     layout = layout_with_fr,
     vertex.size = 1,
     vertex.label = NA,
     vertex.label.color = '#000000',
     edge.arrow.size = 0)

plot(igraph::erdos.renyi.game(n = k, p.or.m = (log(k) / k^1.00)), 
     main = "log(n) / n ^ 1.00",
     layout = layout_with_fr,
     vertex.size = 1,
     vertex.label = NA,
     vertex.label.color = '#000000',
     edge.arrow.size = 0)

mtext("Threshold for Complete Connection", side = 1, outer = TRUE, line = -3)



