library(tidyverse)
library(caret)
library(dplyr)
library(mlbench)
library(MLmetrics)

#read CSV
df <- read.csv("churn.csv")
glimpse(df)


#1. Missing value?
mean(complete.cases(df))

#2. Data Type
df$churn <- as.factor(df$churn)
df$internationalplan <- as.factor(df$internationalplan)
df$voicemailplan <- as.factor(df$voicemailplan)

#3. Train-Test Split
id <- createDataPartition(y = df$churn,
                          p=0.7,
                          list=FALSE)

train_df <- df[id,]
test_df <- df[-id,]

# 4.Train Model
ctrl <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  classProbs = TRUE,
  summaryFunction = prSummary
)

#random forest
rf_model <- train(
  churn ~ .,
  data = train_df,
  method = "rf",
  metric = "AUC",
  preProcess = c("center","scale"),
  trControl = ctrl
)

#KNN
KNN_model <- train(
  churn ~ .,
  data = train_df,
  method = "knn",
  metric = "AUC",
  preProcess = c("center","scale"),
  trControl = ctrl
)

# See Result
rf_model
KNN_model
# Random Forest give better result

# Variable Important
varImp(rf_model)
# Accuracy score drop after select only imp vars

## 3. Test_Model
p <- predict(rf_model, newdata=test_df)
confusionMatrix(p, test_df$churn,
                positive = "Yes",
                mode = "prec_recall")

