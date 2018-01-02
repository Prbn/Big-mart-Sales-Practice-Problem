

#tempA1011 <- Test.Results
write.csv(tempA1011, file = 'Test.Results1 b.csv', row.names = F)

# setting up new working directory for saving file using the
setwd("D:\\Work\\R\\R Projects\\Big mart Sales Practice Problem\\DATA\\Test1")


if(FALSE){
  
  
  ### TEST SECTION
  
  Experiment <- backuptrain1
  
  rm(trainA,trainA1,testA,testA1,Train.Result,Test.Results)
  # Splitting the data set
  # ----------------------
  
  # Splitting the dataset into the training set and Test set
  # Using the package caTools
  library(caTools)
  set.seed(123)
  
  split <- sample.split(Experiment$Item_Outlet_Sales, SplitRatio = 4/5)
  
  trainA1 <- subset(Experiment, split == TRUE)
  testA1 <- subset(Experiment, split == FALSE)
  
  # Removing IDs
  Experiment$Item_Identifier <- NULL
  Experiment$Outlet_Identifier <- NULL
  Experiment$Outlet_Establishment_Year <- NULL
  
  trainA <- subset(Experiment, split == TRUE)
  testA <- subset(Experiment, split == FALSE)
  
  Test.Results <- testA1
  Train.Results <- trainA1
  traintestA1 <- trainA1
  traintestA <- trainA
  # Removing Null from test set
  traintestA1$Item_Outlet_Sales <- NULL
  traintestA$Item_Outlet_Sales <- NULL
  testA1$Item_Outlet_Sales <- NULL
  testA$Item_Outlet_Sales <- NULL
  
  
  rm(split, Experiment)
  
  
  
  # Write CSV
  makecsvsubmission <- function(dataset1, string){
    Data1 <- data.frame(dataset1$Item_Identifier,dataset1$Outlet_Identifier, dataset1[string])
    colnames(Data1) <- c('Item_Identifier','Outlet_Identifier','Item_Outlet_Sales')
    string <- paste(string, ' Submission_t.csv',sep = "", collapse = NULL)
    write.csv(Data1, file = string, row.names = F)
  }
}




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


### TEST SUCESSFULL ###

