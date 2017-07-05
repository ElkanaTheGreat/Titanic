library(caret)
library(rpart)
library(doParallel)

set.seed(11547)

rm(list = ls())

## Configuration

data_path = paste0(getwd(), "/data/")
result_path = paste0(getwd(), "/result/")
model_path = paste0(getwd(), "/model/")

source(file = "toolchain.R")
source(file = "data_preparation.R")

## loading

train_data <- loadData(data_path, "train.csv", colClasses = c("Name" = "character",
                                                              "Ticket" = "character",
                                                              "Cabin" = "character"))
test_data <- loadData(data_path, "test.csv", colClasses = c("Name" = "character",
                                                            "Ticket" = "character",
                                                            "Cabin" = "character"))

## partiton data

inTrain <- createDataPartition(y=train_data$Survived, p=0.6, list=FALSE)
validation_data <- train_data[-inTrain, ]
train_data <- train_data[inTrain, ]

## preparation
train_data <- ConvertSurvived(train_data)
validation_data <- ConvertSurvived(validation_data)
train_data <- prepareData(train_data)
validation_data <- prepareData(validation_data)
test_data <- prepareData(test_data)

# build model

cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

set.seed(7514)

fit <- train(Survived ~ 
               Pclass + Sex + Fare + Embarked +
               Title + DiscreteFamilySize + HasCabin + IsChild,
              data = train_data[, !names(train_data) %in% c("Name", "Cabin", "Ticket", 
                                                            "PassengerId", "IsMother")],
              method = 'rf', 
              metric = 'Accuracy',
              #tuneGrid = expand.grid(.mtry = c(1:4)),
              trControl = trainControl(method="repeatedcv",
                                       number=10, 
                                       verboseIter = TRUE,
                                       repeats=5, 
                                       search='random',
                                       returnResamp = "all",
                                       classProbs = TRUE,
                                       allowParallel = TRUE))

stopCluster(cluster)

## validation of model

score <- 0
result <- validate(fit, validation_data, score)
confusionMatrix(result$pred, validation_data$Survived, positive = "Died")
varImp(fit)
##summary(fit)

## build prediction on test data

 pred <- predict(fit, newdata = test_data)
 result <- cbind(test_data[, c(1, 2)], pred)[, c(1, 3)]
 names(result) <- c("PassengerId", "Survived")

serializeResultAndModel(model_path, fit, result_path, result)