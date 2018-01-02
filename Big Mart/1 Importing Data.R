
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
library('lubridate') # For date and time

totalsummary <- function(data.df){
  st <- str(data.df)
  hd <- head(data.df,n=6)
  sm <- summary(data.df)
  output <- list(Structure=st , Head=hd , Summary=sm)
  return(output)
}
