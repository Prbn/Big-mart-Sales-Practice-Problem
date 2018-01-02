
# Baseline Model


#Test.Results <- testA

# Mean Based

Test.Results$Overall_Mean_Sales <- mean(trainA1$Item_Outlet_Sales)
Train.Results$Overall_Mean_Sales <- mean(trainA1$Item_Outlet_Sales)


# Outlet Mean Sales Based

Outlet_mean_sales <- sapply(testA1$Outlet_Identifier,function(x)mean(trainA1[trainA1$Outlet_Identifier == x,'Item_Outlet_Sales']))
Test.Results$Outlet_Mean_Sales <- Outlet_mean_sales
Train.Results$Outlet_Mean_Sales <- sapply(traintestA1$Outlet_Identifier,function(x)mean(trainA1[trainA1$Outlet_Identifier == x,'Item_Outlet_Sales']))

rm(Outlet_mean_sales)

# Item Mean Sales Based

Item_mean_sales <- sapply(testA1$Item_Identifier,function(x)mean(trainA1[trainA1$Item_Identifier == x,'Item_Outlet_Sales']))
Test.Results$Item_Mean_Sales <- Item_mean_sales
Train.Results$Item_Mean_Sales <- sapply(traintestA1$Item_Identifier,function(x)mean(trainA1[trainA1$Item_Identifier == x,'Item_Outlet_Sales']))

rm(Item_mean_sales)

# Backup
write.csv(Test.Results, file = 'Test.Results1.csv', row.names = F)


# Overall Mean
makecsvsubmission(Test.Results,'Overall_Mean_Sales')

# Outlet Mean
makecsvsubmission(Test.Results,'Outlet_Mean_Sales')

# Item Mean
makecsvsubmission(Test.Results,'Item_Mean_Sales')

as.numeric()
head(Test.Results)

