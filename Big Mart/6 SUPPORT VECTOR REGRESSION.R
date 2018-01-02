# SUPPORT VECTOR REGRESSION #
#===========================#

# Regression model
# ----------------

# Using the e1071 package
library(e1071)

# Fitting Support Vector Regression to the dataset
# Using the svm() function under the e1071 library
# creating regressor variable to store the svr
regressor <- svm(formula = Item_Outlet_Sales ~., data = trainA, type = 'eps-regression')

# The formula is dependent variable expressed as a linear combination of the independent variable
# The data on which the to train needs to be specified.
# The type is the the most important argument, specifies the type of model
# The type is set to eps as it is being used for regression

# Info about the regressor using summary() function
summary(regressor)

# Prediction
# ----------

# Predicting a new result with linear Regression
y_pred = predict(regressor,testA)

# Saving Test results
Test.Results$SVMRegression <- y_pred
Train.Results$SVMRegression <- predict(regressor,traintestA)

# Saving results for submission
makecsvsubmission(Test.Results,'SVMRegression')

