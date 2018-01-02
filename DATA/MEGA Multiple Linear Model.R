# MEGA Multiple Linear Model
# --------------------------

# Setting up training and test data
trainA <- Train.Results[c('Item_Outlet_Sales','Overall_Mean_Sales','Outlet_Mean_Sales','Item_Mean_Sales','Mlinearmodel','OptimalMlinearmodel','SVMRegression','DecisionRegression','RandomForestRegression2')] 
testA <- Test.Results[c('Overall_Mean_Sales','Outlet_Mean_Sales','Item_Mean_Sales','Mlinearmodel','OptimalMlinearmodel','SVMRegression','DecisionRegression','RandomForestRegression2')] 
traintestA <-Train.Results[c('Overall_Mean_Sales','Outlet_Mean_Sales','Item_Mean_Sales','Mlinearmodel','OptimalMlinearmodel','SVMRegression','DecisionRegression','RandomForestRegression2')] 


# Fitting Multiple Linear Regression to the training set.
# Using the lm() function
# creating regressor variable to store the linearmodel
regressor = lm(formula = Item_Outlet_Sales ~ ., data = trainA)
# formula = Profit ~ . is same as formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State

# Information of the regressor
# Using summary function
summary(regressor)
# The function has feature to avoid dummy variable trap

# Predicting the Test set results
# Using the predict() function
# storing the prediction into a functon
y_pred <- predict(regressor,newdata = testA)
# Requires the liner model to use as a basis to predict
# newdata is the data on which the prediction is to be made

# Saving Test results
Test.Results$Megalinearmodel <- y_pred
Train.Results$Megalinearmodel <- predict(regressor,newdata = traintestA)

# Saving results for submission
makecsvsubmission(Test.Results,'Megalinearmodel')