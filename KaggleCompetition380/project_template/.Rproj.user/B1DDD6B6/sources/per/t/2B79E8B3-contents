rm(list = ls())
library(data.table)
library(Metrics)
library(caret)

set.seed(9001)
test <- fread("project/volume/data/interim/Stat_380_train.csv")
train <- fread("project/volume/data/interim/Stat_380_train.csv")

train_y <- train$SalePrice

test$SalePrice <- 0

avg =mean(train$SalePrice, na.rm= TRUE)

dummies <-dummyVars(SalePrice ~ ., data= train)

train <- predict(dummies, newdata = train)
train <-data.table(train)

test <- predict(dummies, newdata = test)
test <-data.table(test)

train$SalePrice<-train_y

#LotFrontageMean <- mean(train$LotFrontage, na.rm= TRUE)
#train[is.na(train$LotFrontage)]$LotFrontage <- LotFrontageMean

#OverallQualMean <- mean(train$OverallQual)
#train$OverallQual[is.na(train$OverallQual)] <- 6.11

fit <-lm(SalePrice ~., data = train)
fit
summary(fit)

test$Pred_SalePrice <- predict(fit, newdata=test)

submit <- test[,.(Pred_SalePrice)]

submit[is.na(submit$Pred_SalePrice)] <- avg
submit

fwrite(test[,.(Id,SalePrice = submit$Pred_SalePrice)],"project/volume/data/processed/lmsubmission.csv")

summary(submit)

