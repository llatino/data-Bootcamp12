library(tidyverse)
library(caret)
library(mlbench)

data("BostonHousing")

df <- BostonHousing

#mean(complete.cases(df)) == 1

set.seed(42)
n <- nrow(df)
train_id <- sample(1:n, size=0.8*n)

train_df <- df[train_id, ]
test_df <- df[-train_id, ]

set.seed(42)

train_ctrl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5
)

model <- train(medv ~ crim + rm + age,
               data = train_df,
               method = "lm",
               trControl = train_ctrl)

p_medv <- predict(model, newdata = test_df)

train_rms <- 6.052629

test_rms <- sqrt(mean((p_medv - test_df$medv)**2))

rf_model <- train(medv ~ .,
               data = train_df,
               method = "rf",
               trControl = train_ctrl
