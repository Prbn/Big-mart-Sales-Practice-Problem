
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
# OR
if(FALSE){
  factor_vars <- c('Item_Identifier','Item_Fat_Content','Item_Type','Outlet_Identifier','Outlet_Establishment_Year','Outlet_Size','Outlet_Location_Type','Outlet_Type')
  full.d[factor_vars] <- sapply(full.d[,factor_vars], function(x) as.factor(x))
  
  str(full.d)
  rm(factor_vars)
  # Faild only works on lists
}
