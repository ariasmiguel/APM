# Library --------------------------------------------------
library(AppliedPredictiveModeling)
library(caret)
library(corrplot)
library(e1071)
library(tidyverse)
library(ggplot2)
library(GGally)
library(data.table)
library(RANN)

# Chapter 3 - Exercise 2 ------------------------------------

# 3.2 The soybean data can also be found at the UC Irvine Machine
# Learning Repository. Data were collected to predict disease
# in 638 soybeans. The 35 predictors are mostly categorical and
# include information on the environmental conditions (e.g. temp,
# precipitation) and plant conditions (e.g., left spots, mold 
# growth). The outcome labels consist of 19 distinct classes.

# The data can be loaded via:
library(mlbench)
data(Soybean)
## See ?Soybean for details
# ?Soybean

setDT(Soybean)
head(Soybean)

# A ---------
# Invsetigate the frequency distributions for the categorical predictors.
# are any of the distributions degenerate in the ways discussed earlier
# in this chapter?

# Use nearZeroVar to find the predictors that have one unique value
# or that have very few unique values relative to the number of samples
# and the ratio of the freq of the most common value to the freq
# of the second most common value is large.

Soybean[, 2:36] <- setDT(map(Soybean[, 2:36], parse_number))
test <- nearZeroVar(Soybean)
colnames(Soybean)[test]

par(mfrow = c(2,2))
hist(Soybean$leaf.mild, xlab = "leaf.mild", main = "leaf.mild")
hist(Soybean$mycelium, xlab = "Mycelium", main = "Mycelium")
hist(Soybean$sclerotia, xlab = "Sclerotia", main = "Sclerotia")

# Here we can see that these predictors have strong right skewness.

# B ----------
# Roughly 18.% of the data are missing. Are there particular predictors
# that are more likely to be missing? Is the pattern of missing data
# related to the classes?

predictors <- colnames(Soybean)
N <- map_dbl(Soybean, ~sum(is.na(.x) == TRUE))
Prob <- map_dbl(Soybean, ~round(sum(is.na(.x) == TRUE)/length(.x)*100, 2) )

B <- data.table(Pred = predictors,
                NA_vals = N,
                Prob_NA = Prob)
B[order(-Prob_NA)]

Soybean$has_na_in_sample = apply(Soybean[,-1], 1, function(x){sum(is.na(x))>0})

table(Soybean[,.(Class, has_na_in_sample)])

# This table shows that some classes have NA values for almost all records
# if not all of them. For example, `2-4-d-injury` and `cyst-nematode`.

# C -------------
# Develop a strategy for missing data, either by eliminating predictors
# or imputation

# There are multiple ways to handle missing data. One of them is
# to simply omit the values. This can be done with `na.omit`, however
# it is sometimes not recommened when doing so eliminates a big
# proportion of data (rows). Hence, we can try the other two
# methods mentioned in the chapter (k-nearest neighbor or a l-model)
# For this problem, we will use the k-nearest neighbor using
# the `impute.knn` function in the impute package. Alternatively,
# we can use the `preProcess` tool in the caret package. This
# function has three methods for imputing NA's: median, bagged
# trees and knn.

# In practice the data would first be split into a training and
# testing set and preProcessing would only be applied to the
# training set. For now, we will do it on the entire data.

# knn requires the data to be centered and sclaed.
trans <- preProcess(Soybean[,-1], 
                    method = c("center", "scale"))

Soybean_trans <- predict(trans, Soybean)
head(Soybean_trans)

# No need to do data imputation for all columns, however, to keep
# all data consistent it is the best way.

# Now we perform knn on the `phytophthora-rot` class, because
# it is the only class with values present and missing.

trans <- preProcess(Soybean_trans[Soybean_trans$Class == "phytophthora-rot", -1],
                    method = c("knnImpute"))
Soybean_trans <- predict(trans, 
                         Soybean_trans[Soybean_trans$Class == "phytophthora-rot",])

head(Soybean_trans)

# Compare with previous data
head(Soybean[Soybean$Class=="phytophthora-rot",])
