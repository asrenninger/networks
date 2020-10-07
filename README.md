# networks
### network analysis in R

Here we use mobile phone logs to understand Philadelphia's socio-spatial network. The data come from a location data provider. Our edge list consists of neighborhoods and points of interest of the kind that you see on Google Maps—shops, restaurants, bars, museums and offices. Each neighborhood is a Census Block Group in Philadelphia. For a given connection, one column has a FIPS code, a second has a unique identifier for a point of interest, and a third column has the number of people that traveled from that Census Block Group to that venue. This final column can be used for weights in the network. We have this data for each month from January to August (and soon for September). There are 2.3 million connections between neighborhood and point interest—what some in transportation planning call “origin-destination pairs”—so far this year; each connection has one or more visits. The infographic below defines some of the terms that will follow below.  

![](viz/infographic.png)

**Neighborhood-Venue Flows, the complete network**

![](viz/agg_graphopt.png)

With 20,000 points of interest in Philadelphia and 1,300 Census Block Groups, the network is dense. This makes it hard to interpret the connections, so we can pull out certain venues or locales and construct *ego* networks around them, exploring how the network is changing over time. Here we use Reading Terminal Market and the Comcast Center, busy hubs of activity before the pandemic, as launching points. Neighborhoods are black and sized to their degree centrality, while businesses and other points of interested a colored by category and sized by degree.

![](viz/cmap.png)

![](viz/poi_comcast.png)

![](viz/poi_reading.png)

**Neighborhood-Neighborhood Interactions, shared points of interest**

![](viz/mode_one.gif)

The diagonal here conveys the total number of interactions that a given neighborhood has had with other neighborhoods, via a point of interest, since our dataset has two modes—one for neighborhoods and one for points of interest. We use matrix multiplication to reduce the edge list to an adjacency matrix wherein each value—each interaction between two neighborhoods—corresponds with the number of venues that residents of these two areas both visit. If people from South Philadelphia and West Philadelphia frequent a pub in Center City, perhaps because they send employees into the business district during the day those employees go attend a happy hour nearby after work, then *those two neighborhoods are linked*. We can think of this as co-presence: both communities share a second or third place (borrowing from Ray Oldenburg). This means that the diagonal is the total number of interactions from all neighborhoods in the graph, a summary of a neighborhood’s role in the network.  

**Venue-Venue Interactions, shared clientele**

![](viz/mode_two.gif)

Likewise, the diagonal here is the total number of interactions that one venue has with another venue, via the neighborhoods that it services. Again, if people from Rittenhouse Square frequent a restaurant and a grocer, *these two venues are linked by their shared clientele*. The sum of a venue’s interactions with other venues, via its clientele, is the diagonal.  
