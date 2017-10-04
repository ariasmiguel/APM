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
str(fold1)

# To get the first 90% of the data (the first fold):
cvPredictors1 <- trainPredictors[fold1, ]
cvClasses1 <- trainClasses[fold1]
nrow(trainPredictors)
nrow(cvPredictors1)

# Basic Model Building in R ---------------

# We can fit a 5-nearest neighbor classification model (Fig. 4.3)
# to the training data and use it to predict the test set. 
# Functions:
# MASS::knn()
# ipred::ipredknn()
# caret::knn3(), produces class predictions and the proportions of the
# neighbors of each class.

# With knn3, we can estimate the 5-nearest neighbor model with:
trainPredictors <- as.matrix(trainPredictors)
knnFit <- knn3(x = trainPredictors, y = trainClasses, k = 5)
knnFit

# At this point, knnFit is ready to predict new samples. To assign
# new samples to classes, the `predict` method is used with the model
# object.
# The standard convention is:
testPredictions <- predict(knnFit, newdata = testPredictors,
                           type = "class")
head(testPredictions)
str(testPredictions)

# The value of the type argument varies across different modelling functions

# Determination of Tuning Parameters ---------------

# 1) The `e1071` package contains the `tune` function, which can evaluate
# four types of models across a range of parameters. 
# 2) The `errorest` function in the `ipred` package can resample single models.
# 3) The `train` function in the `caret` package has built-in modules for
# 144 models and includes capabilities for different resampling methods,
# performance measures, and algorithms for choosing the best model from
# the profile. This function also has capabilities for parallel processing
# so that the resampled model fits can be executed acrros multiple computers
# or processors.

# To tune an SVM model using the credit scoring training set samples, the
# train function can be used.
library(caret)
data(GermanCredit)

## First, remove near-zero variance predictors then get rid of a few predictors 
## that duplicate values. For example, there are two possible values for the 
## housing variable: "Rent", "Own" and "ForFree". So that we don't have linear
## dependencies, we get rid of one of the levels (e.g. "ForFree")

GermanCredit <- GermanCredit[, -nearZeroVar(GermanCredit)]
GermanCredit$CheckingAccountStatus.lt.0 <- NULL
GermanCredit$SavingsAccountBonds.lt.100 <- NULL
GermanCredit$EmploymentDuration.lt.1 <- NULL
GermanCredit$EmploymentDuration.Unemployed <- NULL
GermanCredit$Personal.Male.Married.Widowed <- NULL
GermanCredit$Property.Unknown <- NULL
GermanCredit$Housing.ForFree <- NULL

## Split the data into training (80%) and test sets (20%)
set.seed(100)
inTrain <- createDataPartition(GermanCredit$Class, p = .8)[[1]]
GermanCreditTrain <- GermanCredit[ inTrain, ]
GermanCreditTest  <- GermanCredit[-inTrain, ]

# We will use all the predictors to model the outcome:
library(doMC)
registerDoMC(4)

set.seed(1056)
svmFit <- train(Class ~ .,
                data = GermanCreditTrain,
                # The "method" argument indicates the model type.
                # See ?train for a list of available models.
                method = "svmRadial",
                preProc = c("center", "scale"), # preProcess the data
                tuneLength = 10, # Evaluates the cost values: 2^-2, 2^-1, ... 2^7
                trControl = trainControl(method = "repeatedcv",
                                         repeats = 5,
                                         classProbs = TRUE)) 

# trainControl is used to specify the repeated 10-fold cross-validation
## classProbs = TRUE was added since the text was written (Used later for "type")
svmFit

# A line plot of the average performance
plot(svmFit, scales = list(x = list(log = 2)))

# To predict new samples with this model, the predict method is called:
predictedClasses <- predict(svmFit, GermanCreditTest)
str(predictedClasses)

# Use the "type" option to get class probabilities
predictedProbs <- predict(svmFit, newdata = GermanCreditTest,
                          type = "prob")
head(predictedProbs)

# Between-Model Comparisons -------------

set.seed(1056)
logisticReg <- train(Class ~ .,
                     data = GermanCreditTrain,
                     method = "glm",
                     trControl = trainControl(method = "repeatedcv",
                                              repeats = 5))
logisticReg

# To compare the models based on their cross-validation statistics,
# the resample function can be used with models that share a common
# set of resampled data sets.
resamp <- resamples(list(SVM = svmFit, Logistic = logisticReg))
summary(resamp)

# The summary indicates that the Accuracy is better for the
# Logistic model and Cohen's Kappa is higher for the Logistic model.

# To assess possible differences between the models, the diff method
# is used:
modelDifferences <- diff(resamp)
summary(modelDifferences)

# The p-value for the model comparisons are low, which indicates
# that the models show difference.

