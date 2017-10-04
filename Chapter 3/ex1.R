# Library --------------------------------------------------
library(AppliedPredictiveModeling)
library(caret)
library(corrplot)
library(e1071)
library(purrr)
library(ggplot2)
library(GGally)

# Chapter 3 - Exercise 1 ------------------------------------

# 3.1. The data consists of 214 glass samples labeled as one
# of seven class categories. There are nine predictors,
# including the refractive index and percentages of eight
# elements: Na, Mg, Al, Si, K, Ca, Ba, and Fe

library(mlbench)
data(Glass)
str(Glass)
subGlass <- Glass[,1:9]
# a. Using visualizations, explore the predictor vars to 
# understand their distributions as well as the relationships
# between predictors.
pairs(Glass)
# From pairs we can see that some variables have some degree
# of linear correlation, such as: Ri with Ca

correlations <- cor(subGlass)
correlations
# In correlations we see that RI has -.54 corr with Si
# and 0.81 corr with Ca

# Additionally, we can find the high degree of correlation
# with the  findCorrelation funciton
highCorr <- findCorrelation(correlations, cutoff = .5)
colnames(Glass[, highCorr])

# These tells us that Ca, Si, and RI are highly correlated.
# We do not need these three variables if we model the data
# using linear regression, since they explain one another.

# Another useful tool for visualizing limited # of predictors
ggpairs(Glass, mapping = aes(color = Type), columns = 1:5)

# b. Do there appear to be any outliers in the data? Are any
# of the predictors skewed?

# One way to look at outliers is drawing a boxplot
boxdata <- boxplot(subGlass)
# If a variable/predictor has a lot of outliers, these will
# be present as dots on top/bottom of the lines.
# Here we can see that Na, Si, K, Ca, and Ba have a good
# amount of outliers

# Additionally:
Outlier_Num <- data.frame(Element = colnames(subGlass), Outliers = rep(NA,9))

for(i in 1:9) {
  Outlier_Num[i,2]=length(which(boxdata$group ==i))
  }

Outlier_Num[order(-Outlier_Num$Outliers),]

# To calculate skewness:
map_dbl(subGlass, skewness)

# Large absolute values show vars that are highly skewed.
par(mfrow=c(2,2))
hist(Glass$K, main="Histogram of K")
hist(Glass$Ba, main="Histogram of Ba")
hist(Glass$Ca, main="Histogram of Ca")
hist(Glass$Mg, main="Histogram of Mg")

# c. Are there any relevant transformations of one or more
# predictors that might improve the classification model?

# Could attempt to transform the most skewed features.
# This can be accomplished by applying a log or a squared
# root to the skewed predictors, by using the BoxCoxTrans
# function.

# When using this function there can be no Zero values.
# As such we add a very small number to this values.

boxcox_skewness <- function(x){
  
  x[which(x==0)] = 10^-6
  
  i = BoxCoxTrans(x)  # First apply the BoxCoxTrans function
  
  j = predict(i,x) # Then predict a new set of values
  
  j = skewness(j) # Find the skewness of the transformed features
  
  return(j)
}

map_dbl(Glass[,-10],boxcox_skewness)
