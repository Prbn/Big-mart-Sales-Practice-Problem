
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
if(FALSE){
  # EXPLAINATION
  is.na(full.d$Item_Weight)
  full.d[is.na(full.d$Item_Weight)]
  full.d[is.na(full.d$Item_Weight),'Item_Weight']
  
  full.d[is.na(full.d$Item_Weight),'Item_Identifier']
  
  mean(full.d$Item_Weight, na.rm = TRUE)
  mean(full.d['Item_Weight'], na.rm = TRUE)
  mean(full.d[full.d$Item_Identifier==x, 'Item_Weight'], na.rm = TRUE)
  sapply(full.d[is.na(full.d$Item_Weight),'Item_Identifier'],function(x)mean(full.d[full.d$Item_Identifier==x, 'Item_Weight'], na.rm = TRUE))
}
# Replacing missing data with mean of same item identity
full.d[is.na(full.d$Item_Weight),'Item_Weight'] <- sapply(full.d[is.na(full.d$Item_Weight),'Item_Identifier'],function(x)mean(full.d[full.d$Item_Identifier==x, 'Item_Weight'], na.rm = TRUE))

# Have all been replaced?
nrow(full.d[is.na(full.d$Item_Weight),])==0



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

if (FALSE){
  # Explanation
  head(full.d[is.na(full.d$Outlet_Size),'Outlet_Identifier'])
  Aa <- 'OUT045'
  full.d[full.d$Outlet_Identifier == Aa,]
  full.d[full.d$Outlet_Identifier == Aa,'Outlet_Size']
  length(full.d[full.d$Outlet_Identifier == Aa,'Outlet_Size'])
  sum(!is.na(full.d[full.d$Outlet_Identifier == Aa,'Outlet_Size']))
}
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
storesize.d$Age <- 2017-storesize.d$Outlet_Establishment_Year
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
?mice()
# Excluding certain less-than-useful variables:
mice_mod <- mice(storesize.d[,!names(storesize.d) %in% 'Outlet_Identifier'], method='rf') 
# The methord here is Random Forest 
?names()
# Saving the complete output
mice_output <- complete(mice_mod)
?complete()

# Store Size
storesize.d$Outlet_size <- mice_output$Outlet_size

# clearing mice variables
rm(mice_mod,mice_output)

# Backing up data
Backup1 <- full.d
#full.d <- Backup1

# imputing missing values
full.d[is.na(full.d$Outlet_Size),'Outlet_Size'] <- sapply(as.character(full.d[is.na(full.d$Outlet_Size),'Outlet_Identifier']),function(x)storesize.d[storesize.d$Outlet_Identifier==x,'Outlet_size'])
# as.character() is used as the factors actual value may be different




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

