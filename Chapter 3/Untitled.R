# Library ------------------------------------------------------------
library(AppliedPredictiveModeling)
library(caret)
library(corrplot)
library(e1071)
library(tidyverse)

# Chapter 3 ----------------------------------------------------------

# The chapter directory of the AppliedPredictiveModeling package contains 
# specific code to reproduce the specific models used in the chapter. This
# is intended to alow the reader to see exactly how the models used here
# were created.

# To function `apropos` will search any loaded R packages for a given term.
# For example:
apropos("confusion")

# To find such a function in any pakcage, the RSiteSearch function can 
# help:
# RSiteSearch("confusion", restrict = "functions")

# The raw segmentation data set is contained in the APM package.
data("segmentationOriginal")

str(segmentationOriginal)

# The variable `Case` indicates which cells were used for the training or
# test sets. 
segData <- subset(segmentationOriginal, Case == "Train")
segData <- segmentationOriginal %>% filter(Case == "Train")

# The Class and Cell fields will be saved into sep vectors and removed
# from the main object.
cellID <- segData$Cell
class <- segData$Class
case <- segData$Case
# Now remove the columns
segData <- segData[, -(1:3)]

# The orig data contianed several "status" columns which were binary
# versions of the predictors. To remove these, we find the column
# names containing "Status" and remove them:
statusColNum <- grep("Status", names(segData))
statusColNum
segData <- segData[, -statusColNum]

# Transformations ----------------------------------------------------
# Some features exhibited significantly skewness. The skewness function
# in the e1071 package calculates the sample skewness statistic for 
# each predictor:
skewness(segData$AngleCh1)

# Since all the predictors are numeric cols, the apply function can
# be used to compute the skewness across cols.
skewValues <- apply(segData, 2, skewness)
skewValues <- map_dbl(segData, skewness)
head(skewValues)

# Basic R function `hist` or the `histogram` function in the lattice
# can be used to assess the shape of the distribution
hist(skewValues)

# A caret function, BoxCoxTrans, can find the appropriate transformation
# and apply it to the new data
Ch1AreaTrans <- BoxCoxTrans(segData$AreaCh1)
Ch1AreaTrans

# The original data
head(segData$AreaCh1)

# After transformation
predict(Ch1AreaTrans, head(segData$AreaCh1))
(819^(-.9)-1)/(-.9)

# Another caret function, preProcess, applies this transformation to a set
# of predictors. The base R function prcomp can be used to 
pcaObject <- prcomp(segData,
                    center = TRUE, scale. = TRUE)

# Calculate the cumulative percentage of variance which each component
# accounts for
percentVariance <- pcaObject$sdev^2/sum(pcaObject$sdev^2)*100
percentVariance[1:3]

# The transformed values are stored in pcaObject as a sub-object
# called x, rotation stores the variable loadings:
head(pcaObject$x[, 1:5])
head(pcaObject$rotation[, 1:3])

# The caret class preProcess has the ability to transform, center
# scale, or impute values, as well as apply the spatial sign
# transformation and feature extraction.
trans <- preProcess(segData, 
                    method = c("BoxCox", "center", "scale", "pca"))
trans

# Apply the transformations:
transformed <- predict(trans, segData)

# These values are different than the previous PCA components
# since they were transformed prior to PCA
head(transformed[, 1:5])

# Filtering --------------------------------------------------------

# To filter near-zero variance predictors (nearZeroVar)
nearZeroVar(segData)

# When predictors should be removed, a vector of integers is
# returned that indicates which columns should be removed

# Correlations:
correlations <- cor(segData)
dim(correlations)
correlations[1:4,1:4]

# To visually examine the correlation structure:
corrplot(correlations, order = "hclust")

# To filter based on correlations, the findCorrelation function
# will apply the algorithm in Sect. 3.5. For a given threshold of
# pairwise correlations, the function returns column numbers denoting
# the predictors that are recommended for deletion:
highCorr <- findCorrelation(correlations, cutoff = .75)
length(highCorr)
head(highCorr)
filteredSegData <- segData[, -highCorr]

# Creating Dummy Variables ----------------------------------------
# Using the cars data set in the caret package
data(cars)
str(cars)
carSubset <- cars %>%
  gather(Type, Car, 5:18)
carSubset <- carSubset %>% filter(Type %in% c("convertible", "coupe", "hatchback", "sedan", "wagon")) %>%
  select(Price, Mileage, Type)
carSubset$Type <- as.factor(carSubset$Type)

# To model the price as a function of mileage and type of car, we can
# use the function dummyVars to determine encodings for the predictors.
# Suppose our first model assumes that the price can be modeled as a
# simple additive function of the mileage and type:
simpleMod <- dummyVars(~Mileage + Type,
                       data = carSubset,
                       ## remove the var name from the col name
                       levelsOnly = TRUE)
simpleMod

# To generate the dummy variables for the training set or any
# new samples, the predict model is used in conjunction
# with the dummyVars object:
predict(simpleMod, head(carSubset))

withInteraction <- dummyVars(~Mileage + Type + Mileage:Type,
                             data = carSubset,
                             levelsOnly = TRUE)
withInteraction
predict(withInteraction, head(carSubset))
