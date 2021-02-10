# Gravity Models

Tasks covered: 
- [Building exploratory models](#models)
- [Goodness of Fit](#fits)
- [Building a better mode](#augmentations)
- [Testing by type of visit](#disaggregations)
- [Generalizability](#generalizability)

# models
We develop models where the dependent variable is a **flow** between an origin and a destination, a continuous variable representing the number of visitors passing between them. That means that in Philadelpha there are 1784896 possible flows from 1336 Census block groups, including visitors that stay to shop within their home Census block group—or loops. The first pass at a model takes all visits from all origins to all destinations (the complete graph) and fits a model based on 8 variables: 

- Population of Origin
- Number of businesses at destination
- Mean household size of origin
- Percent with college degree in origin
- Median income of origin
- Distance from origin to distination
- Total visits to destination (**Dj**)
- Total visits from origin (**Oi**)

These origin and destination visit counts constrain the model, giving information on the kinds of origins and destinations contributing to the flow in question. It essentially adds parameters to the model for how active the origin and how active the destination, without giving the details on the flow in need of prediction. **They produce almost identical model fit compared to fixed effects:** in the month of April, a fixed effects model has a mean absolute percent error of 0.3395402 and a model with origin and destination travel information has one of 0.3508448. Thus, the fixed effects model is slightly more accurate but computationally more intensive. 

| January   | April     |
|-----------|-----------|
| ![](https://raw.githubusercontent.com/asrenninger/networks/master/viz/mod1.png)     | ![](https://raw.githubusercontent.com/asrenninger/networks/master/viz/mod2.png)    |

In January, the only significant variables are distance, population, and the constraining variables—the basic components of a gravity model. By April, population is no longer significant. Notably, distance became less important in April and May when the shelter-in-place order in Philadelphia was at its most stringent. Distance became minimally important in December, an estimate that was likely influenced by holiday shopping and travel. 

![](https://raw.githubusercontent.com/asrenninger/networks/master/viz/coefficients_phl.png)

# fits
We can compare a series of measures of Goodness of Fit to understand how well the model is performing. Here we use the following on our preliminary model: 

- Mean Absolute Percent Error
- Root Mean Squared Error
- R-Squared

We see contradictory fits: the MAPE and RMSE fall in April, suggesting that these variables predicted best when the pandemic was at its worst. R-Squared, however, shows a reduction in variance explained during those same months. The metrics that align most with intuition are MAPE and RMSE, because in this year, February and January were anomolous and the constraints put on the model incorporate information on total travel in 2020. 

![](https://raw.githubusercontent.com/asrenninger/networks/master/viz/mape_phl.png)
![](https://raw.githubusercontent.com/asrenninger/networks/master/viz/rmse_phl.png)
![](https://raw.githubusercontent.com/asrenninger/networks/master/viz/rsquared_phl.png)

RMSE is more difficult to interpret and R-Square assumes linearity so we proceed with MAPE as the best measure of model fit going forward. 

# augmentations

Below is a side-by-side comparison of MAPE values for two models, one containing demographic variables for both the origin and destination, the other containing just those variables for the origins. The larger model predicts with almost identical accuracy.

| Kitchen Sink   | Lean     |
|-----------|-----------|
| ![](https://raw.githubusercontent.com/asrenninger/networks/master/viz/mape_phl_sink.png)     | ![](https://raw.githubusercontent.com/asrenninger/networks/master/viz/mape_phl_lean.png)    |

To see just how strong the simple gravity model is and just how weak the contributions of the demographic variables, below is the MAPE for a model that includes just distance, origin population and destination business count. 

![](https://raw.githubusercontent.com/asrenninger/networks/master/viz/mape_phl_simple.png)

# disaggregations
What happens when we predict for a specific category of business rather that all activity flowing from one area to another? Here we build a model to predict trips to grocery stores; we aggregate the unit of analysis to Census tracts rather than block groups to compensate for the relative lack of trips. The following shows the general direction of flows in the data.

![](https://raw.githubusercontent.com/asrenninger/networks/master/viz/winds_grocery.png)

The model is largely similar to the preliminary one. The notable exception is that the sign for median income flips during the pandemic: increased neighborhood income is associated with an increase in trips to the grocery store where the opposite was true in January and February. 

![](https://raw.githubusercontent.com/asrenninger/networks/master/viz/coefficients_phl_grocery.png)

Finally, the model fit for grocery stores is more noisy, but it still performed best during April and May according to Mean Absolute Percent Error. 

![](https://raw.githubusercontent.com/asrenninger/networks/master/viz/mape_phl_grocery.png)

## Generalizability

In this section, we separate flows by neighborhood character—either of the origin or the destination, or both. We want to see if the model fits better or worse at the extremes of income to better understand its biases. 

#### ...rich/poor
#### ...white/black
#### ...present/future


