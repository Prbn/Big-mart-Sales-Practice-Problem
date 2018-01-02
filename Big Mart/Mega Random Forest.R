
# Mega RANDOM FOREST REGRESSION #
#===============================#


# Setting up training and test data
trainA <- Train.Results[c('Item_Outlet_Sales','Overall_Mean_Sales','Outlet_Mean_Sales','Item_Mean_Sales','Mlinearmodel','OptimalMlinearmodel','SVMRegression','DecisionRegression','RandomForestRegression2')] 
testA <- Test.Results[c('Overall_Mean_Sales','Outlet_Mean_Sales','Item_Mean_Sales','Mlinearmodel','OptimalMlinearmodel','SVMRegression','DecisionRegression','RandomForestRegression2')] 
traintestA <-Train.Results[c('Overall_Mean_Sales','Outlet_Mean_Sales','Item_Mean_Sales','Mlinearmodel','OptimalMlinearmodel','SVMRegression','DecisionRegression','RandomForestRegression2')] 

# Feature Scaling
# ---------------

# dataset[,] <- scale(dataset[,])
# Decision tree does not require any Fearture Scaling
# As the library used for regression already has featuring scaling built in, it does not requir feature scaling

# Regression model
# ----------------

# Using the rpart package
library(randomForest)

# Setting seed
set.seed(1234)

# Fitting Random Forest Regression to the dataset
# Using the randomForest() function under the randomForest library
# creating regressor variable to store the svr
regressor <- randomForest(x = traintestA, y = trainA$Item_Outlet_Sales, ntree = 500, nodesize = 20)
# The x argument is the independent variable and is taken in the form of a data frame.
# The y argument is the dependent variable and is taken in the form of vector
# The Ntrees is the number of trees to be made

# Info about the regressor using summary() function
summary(regressor)

# Prediction
# ----------
# Predicting a new result with linear Regression
y_pred = predict(regressor,testA)

# Saving Test results
Test.Results$MegaRandomForestRegression2 <- y_pred
Train.Results$MegaRandomForestRegression2 <- predict(regressor,traintestA)


# Saving results for submission
makecsvsubmission(Test.Results,'MegaRandomForestRegression2')

