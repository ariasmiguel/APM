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

# Consider the music genre data set described in Sect. 1.4.
# The objective for these data is to use the predictors to 
# classify music samples into the appropriate music genre.

# # A) ------
# What data splitting method(s) would you use for these data?
# Explain.

# 1. Perform nearZeroVar to eliminate low unique value predictors
# 2. Perform a correlation matrix to eliminate predictors with
# high correlation
# 3. Center and scale the data to normalize scales.
# 4. Perform a skewness test to lower skewness.
set.seed(1L)

music <- read.csv("./Chapter 4/genresTrain.csv",
                   stringsAsFactors = FALSE)
music <- as.data.table(music)

music_hist <- music[, .(count = .N), by = GENRE][,
                        P:= round(count/sum(count)*100,2)][]
str(music_hist)

hchart(music_hist, "column", hcaes(x = music_hist$GENRE,
                                   y = music_hist$P,
                                   color = music_hist$GENRE))

# There are 12,495 samples and 191 predictors. A total of
# 65 fold greater than the number of predictors. The response
# categories are not balanced, heavy metal is the smallest segment
# accounting for only 7% and the classical category (28%).

# This means that we should split the data into training and
# test sets. Should use stratified sampling when splitting the
# data.

# For sampling, there are several techniques but some have high
# computational requirements. In this scenario, k-fold would be
# less computationally taxing than bootstraping, however,
# bootstrapping would likely provide more accurate estimates.

# # B) Using the tools described in this chapter, provide code for
# implementing your approaches

genres <- music[, !("GENRE"), with = FALSE]
nearZeroVar(genres)
correlations <- cor(genres)
corrplot(correlations[1:25,1:25], order = "hclust")

train_rows <- createDataPartition(music$GENRE, 
                                  p = .80,
                                  list = FALSE)
train_rows
train_genres <- music[train_rows,]
test_genres <- music[-train_rows,]
str(train_genres)

# Check the data distributions
hist_trainGenres <- train_genres[, .(count = .N), by = GENRE][,
  P := round(count/sum(count)*100,2)]

hchart(hist_trainGenres, "column", hcaes(x = hist_trainGenres$GENRE,
                                   y = hist_trainGenres$P,
                                   color = hist_trainGenres$GENRE))

# Resample the data
genreSplits <- createFolds(train_rows, k = 10, returnTrain = TRUE)
str(genreSplits)

resample_data <- music[genreSplits[[1]]]
graph_resample <- resample_data[, .(count = .N), by = GENRE][,
                                                             P := round(count/sum(count)*100,2)]
# Let's try again with our newley resampled data
hchart(graph_resample, "column", hcaes(x = graph_resample$GENRE,
                                         y = graph_resample$P,
                                         color = graph_resample$GENRE))

