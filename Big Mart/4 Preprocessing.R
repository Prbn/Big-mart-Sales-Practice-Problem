
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
trainA1 <- full.d[!is.na(full.d$Item_Outlet_Sales),]
testA1 <- full.d[is.na(full.d$Item_Outlet_Sales),]



# removing the ids
full.d1 <- full.d
full.d$Item_Identifier <- NULL
full.d$Outlet_Identifier <- NULL
full.d$Outlet_Establishment_Year <- NULL



### Splitting the data back into the original test and training sets.
trainA<- full.d[!is.na(full.d$Item_Outlet_Sales),]
testA <- full.d[is.na(full.d$Item_Outlet_Sales),]

traintestA <- trainA
traintestA1 <- trainA1
# Removing Null from test set
traintestA$Item_Outlet_Sales <- NULL
traintestA1$Item_Outlet_Sales <- NULL
testA$Item_Outlet_Sales <- NULL

Test.Results <- testA1
Train.Results <- trainA1
  


# Write CSV
makecsvsubmission <- function(dataset1, string,addsting=''){
  Data1 <- data.frame(dataset1$Item_Identifier,dataset1$Outlet_Identifier, dataset1[string])
  colnames(Data1) <- c('Item_Identifier','Outlet_Identifier','Item_Outlet_Sales')
  string <- paste(string,addsting, 'Submission.csv',sep = " ", collapse = NULL)
  write.csv(Data1, file = string, row.names = F)
}

# Full data process complete 
# Backup
backup2.5 <- full.d
backuptrain1 <- trainA1
backuptest1 <- testA1
#full.d <- backup2.5

# Setting directory of processed data
setwd("D:\\Work\\R\\R Projects\\Big mart Sales Practice Problem\\Test 1")

### Exporting DATA
write.csv(trainA1, file = 'Train_Processed1.csv', row.names = F)
write.csv(testA1, file = 'Test_Processed1.csv', row.names = F)
write.csv(full.d, file = 'full_Processed1.csv', row.names = F)

