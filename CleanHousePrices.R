
# Author: Oliver Engist
# Corresponding dataset: https://www.kaggle.com/c/house-prices-advanced-regression-techniques
# Description: Run this script to produce a cleaned up testing and training dataset of the original 
# House Price datasets.
#--------------------------------------------------------------------------------------------------

# Read in the data:
H.train <- read.csv('~/Dropbox/PORTFOLIO/Regression/train.csv',sep=',',header = T)
H.test <- read.csv('~/Dropbox/PORTFOLIO/Regression/test.csv',sep=',',header = T)

#Function to take a data vector and replace all NA with another factor ("None" in our case). 
#If the vector is numeric, a message appears and the original vector is returned.

replaceNAWithNewFactor <- function(dataVector,newFactor){
  if(class(dataVector)!="numeric"){
    data <- as.character(dataVector)
    na.idx <- is.na(data)
    data[na.idx] <- newFactor
    return(as.factor(data))
  }else{
    print("numeric variable, no NAs replaced")
    return(dataVector)
  }
}

# Create a vector with all the variable names that should be cleaned up:
columnsToClean <- data.frame("Fence","MiscFeature","FireplaceQu","PoolQC","^Bsmt","^Garage","^Mas")
# The ^ symbol is a wildcard, so all vectors with the pattern "Bsmt" or "Garage" are considered.

# Function to apply the grep function to the vector of column names.
applyGrepToHousing <- function(pattern){
  #returns a vector of indices of the columns that have this pattern in the title.
  return(grep(pattern,colnames(H.train)))
}

# Apply the new function on the entire vector of patterns we want to consider
# Creates a vector of all column indices that have NAs and need to be cleaned.
col.idx <- unlist(apply(columnsToClean,2,applyGrepToHousing))

# Take this vector of indices and use the replaceNAWithFactor function.
# Testing and training dataset separately.
H.train[,col.idx] <- mapply(replaceNAWithNewFactor,H.train[,col.idx],"none")
H.test[,col.idx] <- mapply(replaceNAWithNewFactor,H.test[,col.idx],"none")

# Replace all missing LotFrontage NAs with the square root of the LotArea
LotFront.na <- is.na(H.train$LotFrontage)
H.train$LotFrontage[LotFront.na] <- sqrt(H.train$LotArea[LotFront.na])

#Replace the single missing Electricity entry with the most frequent one:
electr.na <- is.na(H.train$Electrical)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

H.train$Electrical[electr.na] <- Mode(H.train$Electrical)

#Resulting numbers of NA per variable:
apply(apply(H.train,2,is.na),2,sum)

# Write data to a new file:
write.table(H.train, file = "~/Dropbox/PORTFOLIO/Regression/train_clean.csv", row.names = FALSE,sep=',')
write.table(H.test, file="~/Dropbox/PORTFOLIO/Regression/test_clean.csv", row.names=FALSE,sep=",")
