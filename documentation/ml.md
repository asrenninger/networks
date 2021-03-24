# Machine learning
## switching from a simple gravity model

First, we train both XGBoost and Random Forest models by splitting our data into training and testing sets, running a k-fold cross-validation during training to tune the hyperparameters. The results, with reference to the original gravity model, are as follows: 

#### baseline gravity
![](https://raw.githubusercontent.com/asrenninger/networks/master/viz/gravity/errorxquantilepoisson.png)

#### random forest
![](https://raw.githubusercontent.com/asrenninger/networks/master/viz/gravity/errorxquantileforest.png)

#### XGBoost
![](https://raw.githubusercontent.com/asrenninger/networks/master/viz/gravity/errorxquantilexgboost.png)

XGBoost and Random Forest models have nearly identical results: poor compared to the gravity on low values but better on high values. They do use variables differently, as shown by variable importance plots.

#### random forest
![](https://raw.githubusercontent.com/asrenninger/networks/master/viz/gravity/forestvip.png)

#### XGBoost
![](https://raw.githubusercontent.com/asrenninger/networks/master/viz/gravity/boostvip.png)

Finally, we can map the results to see how they differ. 

#### random forest
![](https://raw.githubusercontent.com/asrenninger/networks/master/viz/gravity/foresterrormap.png)

#### XGBoost
![](https://raw.githubusercontent.com/asrenninger/networks/master/viz/gravity/boosterrormap.png)

Next we take the difference of errors (MAE) from our best model and our gravity model. The estimates are similar but there are—as we would expect from the above plots—big differences at the extremes, where we know that the machine learning approaches work better. Where the error is negative, though, is where the simple gravity model performs better. **This map shows that the random forest performs much better on some of the most busy destinations—which tend to break from any linear trends—but that the differences are neglible for most other destinations; notably, for the least busy, the linear model performs better.**  

#### Gravity error minus Random Forest error
![](https://raw.githubusercontent.com/asrenninger/networks/master/viz/gravity/poissonforestdifference.png)

Now we can make some predictions. Below we change the amount of square footage in one neighborhood and see how the predictions change. **In each area, for control, we change by the same amount, 30000 square feet.** The neighborhood we choose in Old city current contains just 22000 square feet of grocery, so adding 30000 square feet—the size of typical supermarket—is a substantial change. The one we choose in South Philadelphia has just 800 square feet. 

#### First test
| Gravity     | Random Forest |
| ----------- | ----------- |
| ![](https://raw.githubusercontent.com/asrenninger/networks/master/viz/gravity/absolute_change_1.png) | ![](https://raw.githubusercontent.com/asrenninger/networks/master/viz/gravity/absolute_change_1_forest.png) |

Not too different in that neighborhood but for the following, in Old City, the random forest is making markedly different predictions. The gravity model seems more intuitive, given that Old City is in need of local grocer but would likely not be come a hub—as you might expect at a strip mall, given our data. 

#### Second test
| Gravity     | Random Forest |
| ----------- | ----------- |
| ![](https://raw.githubusercontent.com/asrenninger/networks/master/viz/gravity/absolute_change_2.png) | ![](https://raw.githubusercontent.com/asrenninger/networks/master/viz/gravity/absolute_change_2_forest.png) |

#### Removing supply

We can test how the model performs at the extremes by removing a grocery store from a neighborhood. These maps break it out by positive or negative changes to the prediction, so the left shows flows that see less activity and and right shows flows that see more activity. The model does predict decreased travel between some neighborhoods but, problematically, it also predicts increased travel. 

![](https://raw.githubusercontent.com/asrenninger/networks/master/viz/gravity/absolute_change_3_forest.png)

