
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

# Factorizing
full.d$Item_Category <- factor(full.d$Item_Category)


# Backing UP
Backup1.5 =full.d


# Determining The years of operation of a Outlet
full.d$Outler_Age <- year(now())-full.d$Outlet_Establishment_Year

