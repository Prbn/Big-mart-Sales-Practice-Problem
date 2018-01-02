
# Importing Data
#---------------

# setting up new working directory using the
setwd("D:\\Work\\R\\R Projects\\Big mart Sales Practice Problem\\DATA")

# putting the data frame into an object called stats
test.d <- read.csv("Test_u94Q5KV.csv", stringsAsFactors = F, na.strings = c('') )
train.d <- read.csv("Train_UWu5bXk.csv", stringsAsFactors = F, na.strings = c(''))

# --------------------------

# Loading packages
# -----------------------

library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm
library(lubridate) # For date and time

totalsummary <- function(data.df){
  st <- str(data.df)
  hd <- head(data.df,n=6)
  sm <- summary(data.df)
  output <- list(Structure=st , Head=hd , Summary=sm)
  return(output)
}

# Data Preperation #
# ================ #

# bind training & test data
full.d  <- bind_rows(train.d, test.d)
totalsummary(full.d)

# Variable Editing
# Most of the variable are factors so convertion most variables into factor
# Factorizing
full.d$Item_Identifier <- as.factor(full.d$Item_Identifier)
full.d$Item_Fat_Content <- as.factor(full.d$Item_Fat_Content)
full.d$Item_Type <- as.factor(full.d$Item_Type)
full.d$Outlet_Identifier <- as.factor(full.d$Outlet_Identifier)
#full.d$Outlet_Establishment_Year <- as.factor(full.d$Outlet_Establishment_Year)
full.d$Outlet_Size <- as.factor(full.d$Outlet_Size)
full.d$Outlet_Location_Type <- as.factor(full.d$Outlet_Location_Type)
full.d$Outlet_Type <- as.factor(full.d$Outlet_Type)


### MISSING DATA ###
#==================#


# Exploring the missing data
full.d[!complete.cases(full.d),]
# Number of incomplete Rows
nrow(full.d[!complete.cases(full.d),])

# Rows that are missing data of Item_weight #
#-------------------------------------------#
head(full.d[is.na(full.d$Item_Weight),])
# Number of rows, missing data of Item Weight
nrow(full.d[is.na(full.d$Item_Weight),])

# The data can be imputed from the mean of other same data of same item
# Taking care of missing data

# Replacing missing data with mean of same item identity
full.d[is.na(full.d$Item_Weight),'Item_Weight'] <- sapply(full.d[is.na(full.d$Item_Weight),'Item_Identifier'],function(x)mean(full.d[full.d$Item_Identifier==x, 'Item_Weight'], na.rm = TRUE))

# Have all been replaced?
nrow(full.d[is.na(full.d$Item_Weight),])==0
# Yes


# Rows that are missing data of Item_Fat_Content #
#------------------------------------------------#
head(full.d[is.na(full.d$Item_Fat_Content),])
# Number of rows, missing data of Item Fat Content
nrow(full.d[is.na(full.d$Item_Fat_Content),])
# No missing data

# Exploring different types of Item_Fat_Content
levels(full.d$Item_Fat_Content)
# There are actuall two types only
table(full.d$Item_Fat_Content)

# Correcting
full.d[full.d$Item_Fat_Content =='LF'| full.d$Item_Fat_Content =='low fat', 'Item_Fat_Content'] <- 'Low Fat'
full.d[full.d$Item_Fat_Content =='reg', 'Item_Fat_Content'] <- 'Regular'

# Checking
table(full.d$Item_Fat_Content)
levels(full.d$Item_Fat_Content)

# Refactorizing
full.d$Item_Fat_Content <- as.factor(as.character(full.d$Item_Fat_Content))

# Checking
table(full.d$Item_Fat_Content)
levels(full.d$Item_Fat_Content)

# Rows that are missing data of Item_Visibility #
#------------------------------------------------#
head(full.d[is.na(full.d$Item_Visibility),])
# Number of rows, missing data of Item Visibility
nrow(full.d[is.na(full.d$Item_Visibility),])
# No missing data


# Rows that are missing data of Item_Type #
#------------------------------------------------#
head(full.d[is.na(full.d$Item_Type),])
# Number of rows, missing data of Item Type
nrow(full.d[is.na(full.d$Item_Type),])
# No missing data

# Exploring different types of Item_Type
levels(full.d$Item_Type)
table(full.d$Item_Type)

# Rows that are missing data of Item_MRP #
#------------------------------------------------#
head(full.d[is.na(full.d$Item_MRP),])
# Number of rows, missing data of Item MRP
nrow(full.d[is.na(full.d$Item_MRP),])
# No missing data

# Exploring different types of Item MRP
levels(full.d$Item_MRP)
table(full.d$Item_MRP)

# Rows that are missing data of Outlet_Establishment_Year #
#------------------------------------------------#
head(full.d[is.na(full.d$Outlet_Establishment_Year),])
# Number of rows, missing data of Outlet Establishment Year
nrow(full.d[is.na(full.d$Outlet_Establishment_Year),])
# No missing data

# Exploring different types of Outlet Establishment Year
levels(full.d$Outlet_Establishment_Year)
table(full.d$Outlet_Establishment_Year)

# Rows that are missing data of Outlet_Identifier #
#------------------------------------------------#
head(full.d[is.na(full.d$Outlet_Identifier),])
# Number of rows, missing data of Outlet Size
nrow(full.d[is.na(full.d$Outlet_Identifier),])
# No missing data

# Exploring different types of Outlet Size
factor(full.d$Item_Identifier)
levels(full.d$Item_Identifier)
levels(full.d$Outlet_Identifier)
table(full.d$Outlet_Identifier)

# Rows that are missing data of Outlet_Size #
#------------------------------------------------#
head(full.d[is.na(full.d$Outlet_Size),])
# Number of rows, missing data of Outlet Size
nrow(full.d[is.na(full.d$Outlet_Size),])
# No missing data

# Exploring different types of Outlet Size
levels(full.d$Outlet_Size)
table(full.d$Outlet_Size)

# Checking Outlet sizes
sapply(full.d[is.na(full.d$Outlet_Size),'Outlet_Identifier'],function(x)sum(!is.na(full.d[full.d$Outlet_Identifier == x,'Outlet_Size'])))
# It seems the data of outlet of the store id not present.

# are present store size are all correct
Aa <- table(full.d$Outlet_Identifier,full.d$Outlet_Size)

storesize.d <- data.frame(Outlet_Identifier=levels(full.d$Outlet_Identifier))
storesize.d$sumsales <- sapply(storesize.d$Outlet_Identifier,function(x)sum(full.d[full.d$Outlet_Identifier==x,'Item_Outlet_Sales'],na.rm = TRUE))
storesize.d$Outlet_size <- sapply(storesize.d$Outlet_Identifier,function(x)unique(full.d[full.d$Outlet_Identifier==x,'Outlet_Size']))
storesize.d$Outlet_Location_Type <- sapply(storesize.d$Outlet_Identifier,function(x)unique(full.d[full.d$Outlet_Identifier==x,'Outlet_Location_Type']))
storesize.d$Outlet_Type <- sapply(storesize.d$Outlet_Identifier,function(x)unique(full.d[full.d$Outlet_Identifier==x,'Outlet_Type']))
storesize.d$Number_of_Item <- sapply(storesize.d$Outlet_Identifier,function(x)length(unique(full.d[full.d$Outlet_Identifier==x,'Item_Identifier'])))
storesize.d$Outlet_Establishment_Year <- sapply(storesize.d$Outlet_Identifier,function(x)unique(full.d[full.d$Outlet_Identifier==x,'Outlet_Establishment_Year']))
storesize.d$Age <- year(now())-storesize.d$Outlet_Establishment_Year
str(storesize.d)


# Imputing missing Outlet Size values by predictive imputation
# Predictive imputation
# Create a predicting model based on other variables.

# Creating a model predicting Outlet Size based on other variables.

# Using 'mice' package. (Multivariate Imputation by Chained Equations)
# 'rpart' (recursive partitioning for regression) can also be used.
# Steps for mice imputation
# 1. Factorize the factor variables.
# 2. Perform mice imputation.

# Setting a random Seed
set.seed(123)

# Performing mice imputation
mice_mod <- mice(storesize.d[,!names(storesize.d) %in% 'Outlet_Identifier'], method='rf') 
mice_output <- complete(mice_mod)

storesize.d$Outlet_size <- mice_output$Outlet_size

# clearing mice variables
rm(mice_mod,mice_output)

# Backing up data
Backup1 <- full.d
#full.d <- Backup1

# imputing missing values
full.d[is.na(full.d$Outlet_Size),'Outlet_Size'] <- sapply(as.character(full.d[is.na(full.d$Outlet_Size),'Outlet_Identifier']),function(x)storesize.d[storesize.d$Outlet_Identifier==x,'Outlet_size'])


# Rows that are missing data of Outlet_Location_Type #
#------------------------------------------------#
head(full.d[is.na(full.d$Outlet_Location_Type),])
# Number of rows, missing data of Outlet Location Type
nrow(full.d[is.na(full.d$Outlet_Location_Type),])
# No missing data

# Exploring different types of Outlet Location Type
levels(full.d$Outlet_Location_Type)
table(full.d$Outlet_Location_Type)

# Rows that are missing data of Outlet_Type #
#------------------------------------------------#
head(full.d[is.na(full.d$Outlet_Type),])
# Number of rows, missing data of Outlet Type
nrow(full.d[is.na(full.d$Outlet_Type),])
# No missing data

# Exploring different types of Outlet Type
levels(full.d$Outlet_Type)
table(full.d$Outlet_Identifier,full.d$Outlet_Type)

# Rows that are missing data of Item_Outlet_Sales #
#------------------------------------------------#
head(full.d[is.na(full.d$Item_Outlet_Sales),])
# Number of rows, missing data of Outlet Sales
nrow(full.d[is.na(full.d$Item_Outlet_Sales),])
# No missing data


# Feature Engineering #
#=====================#

# Checking if there is significant change in the outlet Types #
#-------------------------------------------------------------#

tapply(full.d[!is.na(full.d$Item_Outlet_Sales),'Item_Outlet_Sales'],full.d[!is.na(full.d$Item_Outlet_Sales),'Outlet_Type'],mean)

tapply(full.d[!is.na(full.d$Item_Outlet_Sales),'Item_Outlet_Sales'],full.d[!is.na(full.d$Item_Outlet_Sales),'Outlet_Type'],summary)

# Plotting a boxplot
ggplot(full.d[!is.na(full.d$Item_Outlet_Sales),], aes(x = Outlet_Type, y = Item_Outlet_Sales, fill = factor(Outlet_Type))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=mean(full.d[!is.na(full.d$Item_Outlet_Sales),'Item_Outlet_Sales'])), colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()

# As the Type of stores are different they wont be altered


# Item visibility #
#-----------------#

# Number of rows where item visibility is zero
nrow(full.d[full.d$Item_Visibility==0,])

# Imputing the zero item visibility with the overall mean item visibility 
# full.d[full.d$Item_Visibility==0,'Item_Visibility'] <- mean(full.d[!full.d$Item_Visibility==0,'Item_Visibility'])

# Imputing the zero item visibility with the mean item visibility of each store
full.d[full.d$Item_Visibility==0,'Item_Visibility'] <- sapply(full.d[full.d$Item_Visibility==0,'Outlet_Identifier'],function(x)mean(full.d[(!full.d$Item_Visibility == 0 & full.d$Outlet_Identifier == x),'Item_Visibility']))

# Checking
nrow(full.d[full.d$Item_Visibility==0,])==0


# Creating a new category based on the Item Identifier
# The Item identifier that starts with FD is for food, NC for Non Consmable and DR for Drinks
full.d[grep('^FD',as.character(full.d$Item_Identifier)),'Item_Category'] <- 'Food'
full.d[grep('^NC',as.character(full.d$Item_Identifier)),'Item_Category'] <- 'Non Consumable'
full.d[grep('^DR',as.character(full.d$Item_Identifier)),'Item_Category'] <- 'Drinks'

# Checking if all the row are filled
nrow(full.d[is.na(full.d$Item_Identifier),])==0

# Backing UP
Backup1.5 =full.d

# Determining The years of operation of a Outlet
full.d$Outler_Age <- year(now())-full.d$Outlet_Establishment_Year


# Preprocessing #
#===============#
str(full.d)
backup2 <- full.d
#full.d <- backup2

# Encoding Categorical data
full.d$Item_Fat_Content <- factor(full.d$Item_Fat_Content,levels = levels(full.d$Item_Fat_Content),labels = c(1:nlevels(full.d$Item_Fat_Content)))
full.d$Item_Type <- factor(full.d$Item_Type,levels = levels(full.d$Item_Type),labels = c(1:nlevels(full.d$Item_Type)))
#full.d$Outlet_Establishment_Year <- factor(full.d$Outlet_Establishment_Year,levels = levels(full.d$Outlet_Establishment_Year),labels = c(1:nlevels(full.d$Outlet_Establishment_Year)))
full.d$Outlet_Size <- factor(full.d$Outlet_Size,levels = levels(full.d$Outlet_Size),labels = c(1:nlevels(full.d$Outlet_Size)))
full.d$Outlet_Location_Type <- factor(full.d$Outlet_Location_Type,levels = levels(full.d$Outlet_Location_Type),labels = c(1:nlevels(full.d$Outlet_Location_Type)))
full.d$Outlet_Type <- factor(full.d$Outlet_Type,levels = levels(full.d$Outlet_Type),labels = c(1:nlevels(full.d$Outlet_Type)))
full.d$Item_Category <- factor(full.d$Item_Category,levels = levels(full.d$Item_Category),labels = c(1:nlevels(full.d$Item_Category)))


### Splitting the data back into the original test and training sets.
trainA<- full.d[!is.na(full.d$Item_Outlet_Sales),]
testA <- full.d[is.na(full.d$Item_Outlet_Sales),]

### Exporting DATA
write.csv(trainA, file = 'Train_Processed1.csv', row.names = F)
write.csv(testA, file = 'Test_Processed1.csv', row.names = F)
write.csv(full.d, file = 'full_Processed1.csv', row.names = F)

# removing the ids
full.d$Item_Identifier <- NULL
full.d$Outlet_Identifier <- NULL
full.d$Outlet_Establishment_Year <- NULL

### Splitting the data back into the original test and training sets.
trainA<- full.d[!is.na(full.d$Item_Outlet_Sales),]
testA <- full.d[is.na(full.d$Item_Outlet_Sales),]

rm(train.d,test.d)


# Baseline Model


#Test.Results <- testA

# Mean Based

Test.Results$Overall_Mean_Sales <- mean(trainA$Item_Outlet_Sales)

# Outlet Mean Sales Based

Outlet_mean_sales <- sapply(testA$Outlet_Identifier,function(x)mean(trainA[trainA$Outlet_Identifier == x,'Item_Outlet_Sales']))
Test.Results$Outlet_Mean_Sales <- Outlet_mean_sales
rm(Outlet_mean_sales)

# Item Mean Sales Based

Item_mean_sales <- sapply(testA$Item_Identifier,function(x)mean(trainA[trainA$Item_Identifier == x,'Item_Outlet_Sales']))
Test.Results$Item_Mean_Sales <- Item_mean_sales
rm(Item_mean_sales)

# Backup
write.csv(Test.Results, file = 'Test.Results1.csv', row.names = F)

# Write CSV
makecsvsubmission <- function(dataset1, string){
  Data1 <- data.frame(dataset1$Item_Identifier,dataset1$Outlet_Identifier, dataset1[string])
  colnames(Data1) <- c('Item_Identifier','Outlet_Identifier','Item_Outlet_Sales')
  string <- paste(string, ' Submission.csv',sep = "", collapse = NULL)
  write.csv(Data1, file = string, row.names = F)
}

# Overall Mean
makecsvsubmission(Test.Results,'Overall_Mean_Sales')

# Outlet Mean
makecsvsubmission(Test.Results,'Outlet_Mean_Sales')

# Item Mean
makecsvsubmission(Test.Results,'Item_Mean_Sales')

### Predictive Modeling ###
#=========================#



# Multiple Linear Model
# ---------------------

# Fitting Multiple Linear Regression to the training set.
regressor = lm(formula = Item_Outlet_Sales ~ ., data = trainA)

# Information of the regressor
# Using summary function
summary(regressor)
# The function has feature to avoid dummy variable trap

y_pred <- predict(regressor,newdata = testA)

Test.Results$Mlinearmodel <- y_pred

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

# Saving results for submission
makecsvsubmission(Test.Results,'OptimalMlinearmodel')

# SUPPORT VECTOR REGRESSION #
#===========================#

# Cleaning the Testset
# requires removal of null variable
testA1<- testA
testA1$Item_Outlet_Sales <- NULL

# Using the e1071 package
library(e1071)

# Fitting Support Vector Regression to the dataset
regressor <- svm(formula = Item_Outlet_Sales ~., data = trainA, type = 'eps-regression')

# Info about the regressor using summary() function
summary(regressor)

# Prediction
# ----------

# Predicting a new result with linear Regression
y_pred = predict(regressor,testA1)

# Saving Test results
Test.Results$SVMRegression <- y_pred

# Saving results for submission
makecsvsubmission(Test.Results,'SVMRegression')



###
# DECISION REGRESSION #
#=====================#

# Using the rpart package
library(rpart)

# Fitting Decision Regression to the dataset
regressor <- rpart(formula = Item_Outlet_Sales ~., data = trainA, control = rpart.control(minsplit = 50))

# Info about the regressor using summary() function
summary(regressor)

# Prediction
# ----------
# Predicting a new result with linear Regression
y_pred = predict(regressor,testA)

# Saving Test results
Test.Results$DecisionRegression <- y_pred

# Saving results for submission
makecsvsubmission(Test.Results,'DecisionRegression')


# RANDOM FOREST REGRESSION #
#==========================#

# Using the rpart package
library(randomForest)

# Setting seed
set.seed(1234)

# Fitting Random Forest Regression to the dataset
regressor <- randomForest(x = trainA[-9], y = trainA$Item_Outlet_Sales, ntree = 500)

# Info about the regressor using summary() function
summary(regressor)

# Prediction
# ----------
# Predicting a new result with linear Regression
y_pred = predict(regressor,testA1)

# Saving Test results
Test.Results$RandomForestRegression <- y_pred

# Saving results for submission
makecsvsubmission(Test.Results,'RandomForestRegression')


###




