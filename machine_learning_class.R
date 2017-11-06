library(caret)
library(dplyr)
library(MASS)
library(randomForest)
library(rpart)
library(rpart.plot)
library(ggplot2)

setwd("D:/desktop")

if(!file.exists("./training.csv")) {

download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",destfile = "./training.csv")

}

if(!file.exists("./test.csv")) {
  
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",destfile = "./test.csv")

}
#Getting rid of irrelevant features

training_set_first = read.csv("./training.csv")
test_set_first = read.csv("./test.csv")

training_set = training_set_first
test_set = test_set_first

training_set = dplyr::select(training_set,-(X:new_window))
test_set = dplyr::select(test_set,-(X:new_window))

# Getting rid of features with NAs

training_set = training_set[ , colSums(is.na(training_set)) == 0]
test_set = test_set[ , colSums(is.na(test_set)) == 0]

# Getting rid of factor variables, except for the activities, as they are not present
# in the test set and after further examination,hey add no useful information


activities <- training_set$classe
clean_training = training_set[,-grep("factor",sapply(training_set,class))]

clean_training$classe <- activities


# Creating partitions for cross validation and estimation of out of sample
# error, as our test data does not include an activity label to use that
# dataset for error estimating purposes

training_obs = createDataPartition(clean_training$classe, p = 0.8,list = FALSE)
pure_training = clean_training[training_obs,]
testing_dataset = clean_training[-training_obs,]

# Model creation

model_rf = randomForest(classe ~ .,na.action = na.omit,data = pure_training)
model_rpart = train(classe ~ .,na.action = na.omit,data = pure_training,method = "rpart")

# Predicting with both models

predict_rf = predict(model_rf,newdata = testing_dataset)
predict_rpart = predict(model_rpart,newdata = testing_dataset)

# Evaluating errors

confmatrix_rf = confusionMatrix(predict_rf,reference = testing_dataset$classe)
confmatrix_rpart = confusionMatrix(predict_rpart,reference = testing_dataset$classe)


# Using chosen model to predict test cases

predict_test_cases = predict(model_rf, newdata = test_set)
                  
