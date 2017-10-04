# Library ------------------------------------------------------------
library(AppliedPredictiveModeling)
library(caret)
library(corrplot)
library(e1071)
library(tidyverse)
library(ipred)
library(MASS)

# Chapter 4 ----------------------------------------------------------

# Data Splitting -----

# The two-class data is in the AppliedPredictiveModeling package
data(twoClassData)

# The predictors are stroed in a data frame called `predictors`.
# The outcome classes are contained in a factor vector called
# `classes`
str(predictors)
str(classes)

# The base R function sample can create simple random splits
# of the data. To create stratified random splits of the data 
# (based on the classes), the `createDataPartition` function
# in the `caret` package can be used

# Set the random number seed to reproduce results
set.seed(1)
# By default, the numbers are returned as lists. Using list = FALSE,
# a matrix of row numbers is generated. These samples are allocated 
# to the training set should be specified.
trainingRows <- createDataPartition(classes,
                                    p = .80,
                                    list = FALSE)
head(trainingRows)

# Subset the data into objects for training using integer sub-setting.
trainPredictors <- predictors[trainingRows, ]
trainClasses <- classes[trainingRows]

# Do the same for the test set using negative integers
testPredictors <- predictors[-trainingRows,]
testClasses <- classes[-trainingRows]

str(trainPredictors)
str(testPredictors)

# To generate a test set using maximum dissimilarity sampling, the
# `caret` function `maxDissim` can be used to sequentially sample
# the data.
?maxDissim

# Resampling ----------------

# The caret package has various functions for data splitting. The
# `createDataPartition` could be used by using the `times` arg to 
# generate multiple splits.
set.seed(1L)

# For illustration, generate the info needed for three resampled 
# versions of the training set.
repeatedSplits <- createDataPartition(trainClasses, p = .80,
                                      times = 3)
str(repeatedSplits)

# Similarly, the function `createResamples` (for bootstraping),
# `createFolds` (for k-old cross-validation) and `createMultiFolds`
# (for repeated cross-validation). To create indicators for 10-fold
# cross-validation,
cvSplits <- createFolds(trainClasses, k = 10,
                        returnTrain = TRUE)
str(cvSplits)

# Get the first set of row numbers from the list
fold1 <- cvSplits[[1]]

# To get the first 90% of the data (the first fold):
cvPredictors1 <- trainPredictors[fold1, ]
cvClasses1 <- trainClasses[fold1]
nrow(trainPredictors)
nrow(cvPredictors1)
