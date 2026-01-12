library(tidyverse)
library(caret)
library(mlbench)
library(MLmetrics)


##ML workflow
# 1. split data (train 80%, test 20%)
# 2. train model
# 3. score (predict)
# 4. evaluate compare train vs test

## predict diabetes
data("PimaIndiansDiabetes")

## clean missing data
df <- PimaIndiansDiabetes %>%
  drop_na()

## split data
set.seed(42)
n <- nrow(df)
id <- sample(1:n,  size = 0.8*n)
train_df <- df[id, ]
test_df <- df[-id, ]

## train model
set.seed(42)
##k = hyper parameter

ctrl <- trainControl(method = "cv",
                     number = 5,
                     verboseIter = TRUE , #show step
                     summaryFunction = prSummary,
                     classProbs = TRUE)

k_grid <- data.frame(k = c(3,5,7,9))

## Kappa = Accuracy more the better.
model <- train(diabetes ~ .,
               data = train_df,
               method = "knn",
               metric = "Recall",
               trControl = ctrl,
               ##grid search
               tuneGrid = k_grid,
               preProcess = c("center", "scale"))

## Classification vs Regression metric
## Class: "Accuracy
## Regression: "RMSE"

## Classification Interface
## SummaryFunction
## ROC: "TwoClassSummary"
## AUC: "prSummary"

## center and scale: 
## z = (x-x_bar) / sd

## normalization: 0-1
## x_norm = (x - min_x)/(max_x - min_x) 
