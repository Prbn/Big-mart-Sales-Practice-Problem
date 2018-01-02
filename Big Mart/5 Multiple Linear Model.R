### Predictive Modeling ###
#=========================#



# Multiple Linear Model
# ---------------------

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
Test.Results$Mlinearmodel <- y_pred
Train.Results$Mlinearmodel <- predict(regressor,newdata = traintestA)

# Saving results for submission
makecsvsubmission(Test.Results,'Mlinearmodel')


# Building the optimal model

# Significance level is set to 95%
# Using Backward Elimination
# Starting with all the independent variables
# 1st attempt all variables
regressor = lm(formula = Item_Outlet_Sales ~ Item_Weight + Item_Fat_Content + 
               Item_Visibility + Item_Type +Item_MRP + Outlet_Size + 
               Outlet_Location_Type + Outlet_Type + Item_Category + Outler_Age,
               data = trainA)
# Then remove the non-significant independent variable
# Using the summary function to find the non-significant independent variable
summary(regressor)

# Removing the insignificant indpendent variable
# 2nd attempt removing Item_Type
regressor = lm(formula = Item_Outlet_Sales ~ Item_Weight + Item_Fat_Content + 
               Item_Visibility + Item_MRP + Outlet_Size + 
               Outlet_Location_Type + Outlet_Type + Item_Category + Outler_Age,
               data = trainA)
summary(regressor)

# Removing the insignificant indpendent variable
# 3rd attempt removing Item_Weight
regressor = lm(formula = Item_Outlet_Sales ~ Item_Fat_Content + 
                 Item_Visibility + Item_MRP + Outlet_Size + 
                 Outlet_Location_Type + Outlet_Type + Item_Category + Outler_Age,
               data = trainA)
summary(regressor)

# Removing the insignificant indpendent variable
# 4th attempt removing Item_Category
regressor = lm(formula = Item_Outlet_Sales ~ Item_Fat_Content + 
                 Item_Visibility + Item_MRP + Outlet_Size + 
                 Outlet_Location_Type + Outlet_Type + Outler_Age,
               data = trainA)
summary(regressor)

# Removing the insignificant indpendent variable
# 5th attempt removing Outler_Age
regressor = lm(formula = Item_Outlet_Sales ~ Item_Fat_Content + 
                 Item_Visibility + Item_MRP + Outlet_Size + 
                 Outlet_Location_Type + Outlet_Type,
               data = trainA)
summary(regressor)

# Removing the insignificant indpendent variable
# 5th attempt removing Item_Visibility
regressor = lm(formula = Item_Outlet_Sales ~ Item_Fat_Content + 
                 Item_MRP + Outlet_Size + 
                 Outlet_Location_Type + Outlet_Type,
               data = trainA)
summary(regressor)

# Final regressor produced
y_pred <- predict(regressor,newdata = testA)

# Saving Test results
Test.Results$OptimalMlinearmodel <- y_pred
Train.Results$OptimalMlinearmodel <- predict(regressor,newdata = traintestA)

# Saving results for submission
makecsvsubmission(Test.Results,'OptimalMlinearmodel')


###
rm(regressor,dataset,test_set,training_set,split,y_pred)

