rm(list =ls())

library(data.table)
library(ISLR)
library(Metrics)
library(caret)
set.seed(9002)



test <- fread("project/volume/data/interim/test_file.csv")
train <- fread("project/volume/data/interim/train_file.csv")

train_y <- train$result

test$result<- 0


dummies <-dummyVars(result ~ ., data= train)

train <- predict(dummies, newdata = train)
train <-data.table(train)

test <- predict(dummies, newdata = test)
test <-data.table(test)

train$result<-train_y

#specify binomial for logistic regeression
glm_fit<- glm(result ~ ., family="binomial", data = train)
summary(glm_fit)
coef(glm_fit)


saveRDS(dummies, "./project/volume/models/result_dummies.dummies")
saveRDS(glm_fit, "./project/volume/models/glm.model")

test$pred<-predict(glm_fit, newdata = test, type = "response")
submit <- test[,.(pred)]

fwrite(test[,.(id,result = submit$pred)],"project/volume/data/processed/lrsubmission.csv")
