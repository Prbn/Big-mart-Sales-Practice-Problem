# DECISION REGRESSION #
#=====================#


# Feature Scaling
# ---------------

# dataset[,] <- scale(dataset[,])
# Decision tree does not require any Fearture Scaling
# As the library used for regression already has featuring scaling built in, it does not requir feature scaling

# Regression model
# ----------------

# Using the rpart package
library(rpart)

# Fitting Decision Regression to the dataset
# Using the rpart() function under the rpart library
# creating regressor variable to store the svr
regressor <- rpart(formula = Item_Outlet_Sales ~., data = trainA, control = rpart.control(minsplit = 50))
# The formula is dependent variable expressed as a linear combination of the independent variable
# The data on which the to train needs to be specified.
# The control argument is required to set the setting, such as number of split
# 'control' takes in 'rpart.control()' function
# within 'rpart.control()' function minsplit is set to 1

# Info about the regressor using summary() function
summary(regressor)

# Prediction
# ----------
# Predicting a new result with linear Regression
y_pred = predict(regressor,testA)

# Saving Test results
Test.Results$DecisionRegression <- y_pred
Train.Results$DecisionRegression <- predict(regressor,traintestA)


# Saving results for submission
makecsvsubmission(Test.Results,'DecisionRegression')

