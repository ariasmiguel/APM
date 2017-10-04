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

# Chapter 3 - Exercise 3 ------------------------------------
# The ability of a chemical to permeate the blood-brain barrier
# was experimentally determined for 208 compounds. 134 descriptors
# were measured for each compound.

# A --------------------
# Start R and load the data
data(BloodBrain)
head(bbbDescr)
head(logBBB)
# The numeric outcome is contained in the vector `logBBB` while
# the predictors are in the data frame `bbbDescr`

# B ---------------------
# Do any of the predictors have degenerate distributions?

# Use nearZeroVar
deg <- nearZeroVar(bbbDescr)
colnames(bbbDescr)[deg]

# There are a few columns above the nearZeroVar, could drop
# them before continuing

# Now find the skewed predictors:

map_df(bbbDescr, skewness) %>%
  gather(Predictor, Skewness, 1:length(bbbDescr)) %>%
  arrange(desc(Skewness))

# Some of the skewed predictors are the same from the
# nearZeroVar. Nonetheless, there are other variables that
# have high skewness and did not appear in the nearZeroVar test.
# We should consider centering and scaling the data, which
# can be attained with `preProcess`

trans <- preProcess(bbbDescr, method = c("center", "scale"))
scaled <- predict(trans, bbbDescr)

# C -----------------------
# Generally speaking, are there strong relationships between
# the predictor data? If so, how could correlations in the predictor
# set be reduced? Does this have a dramatic effect on the number
# of predictors available for modeling?

# First, we compute the correlations between the data
correlations <- cor(bbbDescr)
corrplot(correlations[1:25, 1:25], order = "hclust")

# Then, we can determine a correlation threshold with
# findCorrelation
highCorr <- findCorrelation(correlations, cutoff = 0.75)
colnames(bbbDescr[, highCorr])

x <- length(bbbDescr)
y <- length(highCorr)

# There's 66 vars with high correlation, in other words we can 
# drop these variables. 
x-y
# We keep 68 predictors. Still good enough

bbbDescr_filtered <- bbbDescr[, -highCorr]
