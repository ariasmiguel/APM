# Library --------------------------------------------------
library(AppliedPredictiveModeling)
library(caret)
library(corrplot)
library(e1071)
library(purrr)
library(highcharter)
library(GGally)
library(MASS)
library(data.table)

# Chapter 4 - Exercise 1 ------------------------------------
# Consider the permeability data set described in Sect. 1.4. The 
# objective for these data is to use the predictors to model
# compounds' permeability
data(permeability)
fprints <- as.data.table(fingerprints)
nrow(fprints)
ncol(fprints)
str(fprints)

comparison <- nrow(fprints)/ncol(fprints)
comparison
is.na(comparison)

# The data consists of 165 observations and 1107 predictors. Hence,
# there's a ratio of 0.15 observations for each predictor. This means
# that there's a very limited number of observations. The good thing
# is that there are no NA values in the dataset.

# A) What data splitting method(s) would you use for these data?
# Explain

str(permeability)
head(permeability)
histogram(permeability)

# The data has a strong degree of right skewness.
# Due to the low amount of observations, splitting the data is not optimal.
# Therefore, should focus on using resampling performance measures to select
# optimal tuning parameters and predictive performance. Additionally, the 
# high right skewness of the data may hinder random sampling as it wouldn't be
# representative of the overall dataset, since most points are lower than 10. 
# Therefore, we should use stratified sampling to create representative, and 
# random selections of the data.
set.seed(1L)

repeatedCV <- createMultiFolds(permeability, k = 10, times = 50)
par(mfrow = c(3,3))

for (i in 1:9){
  hist(permeability[repeatedCV[[i]]],
       main = paste("Rep ", i),
       xlab = "permeability")
}
