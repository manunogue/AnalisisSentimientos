#
# Description: Script to adjust machine learning models:
#               - J48
#               - Support Vector Machine
#               - eXtreme Gradient Boosting
#
# Author: Manuel Noguera 

# Libraries
library(data.table)
library(caret)
library(randomForest)
library(plyr)
library(xgboost)
library(RWeka)
library(kernlab)
library(partykit)
library(unbalanced)
library(doParallel)
library(e1071) #nb

# Parallelization set up
options(mc.cores = detectCores())

colsToDel <- c("X", "completeOpinion")

# All datasets path locations
pathFiles <- list.files(path = "./Data/Train/", pattern = ".csv$", all.files = FALSE, full.names = TRUE, 
                        recursive = TRUE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

# Adjust SVM model
getModelSVM = function(train_dataset) {  
  
  train_dataset$SentimentValue <- as.factor(train_dataset$SentimentValue)
  
  control <- trainControl(method="cv", number=5, classProbs = TRUE, 
                          summaryFunction = twoClassSummary, allowParallel = TRUE)
  
  ModelResults <- caret::train(SentimentValue ~ .,
                               data=train_dataset,
                               method="svmLinear",
                               trControl=control,
                               metric="ROC")
  return(ModelResults)
}

# Adjust XGBoost model
getModelXGBoost = function(train_dataset) {  
  
  control <- trainControl(method="cv", number=5, classProbs = TRUE, 
                          summaryFunction = twoClassSummary, allowParallel = TRUE)
  
  predictors <- train_dataset[,names(train_dataset)!="SentimentValue"]
  
  for(i in 1:ncol(predictors)){
    predictors[,i] <- as.numeric(as.character(predictors[,i]))
  }
  
  label <- train_dataset$SentimentValue
  
  ModelResults <- caret::train(x=predictors,
                               y=label,
                               method="xgbTree",
                               trControl=control,
                               metric="ROC")
  return(ModelResults)
}

# Adjust J48 model
getModelJ48 = function(train_dataset) {  
  
  control <- trainControl(method="cv", number=5, classProbs = TRUE, 
                          summaryFunction = twoClassSummary, allowParallel = TRUE)
  
  
  predictors <- train_dataset[,names(train_dataset)!="SentimentValue"]
  
  for(i in 1:ncol(predictors)){
    predictors[,i] <- as.numeric(as.character(predictors[,i]))
  }
  
  label <- train_dataset$SentimentValue
  
  
  ModelResults<- caret::train(x=predictors,
                              y=label, 
                              method="J48",
                              trControl=control,
                              metric="ROC")
  return(ModelResults)
}

# Train and evaluates all the algortithms agains a given dataset
getFinalResults = function(algorithm, model, train_dataset, test_dataset, model_time){ 
  
  print(".......................................................................................")
  print(paste0("ALGORITHM: ", algorithm))
  print(paste0("TIME: ", model_time))
  
  ### TRAIN
  print("MODEL SUMMARY: ")
  print(model)
  
  confMatrix <- confusionMatrix(model)
  confTable <- (nrow(train_dataset)/100)*(confMatrix$table)
  
  print("CONFUSION MATRIX: ")
  print(confMatrix)
  
  TN <- confTable[1,1]
  FN <- confTable[1,2]
  FP <- confTable[2,1]
  TP <- confTable[2,2]
  
  accuracy <- (TP+TN)/(TP+TN+FP+FN) 
  pos_precision <- TP/(TP+FP)
  neg_precision <- TN/(TN+FN)
  specificity <- TN/(TN+FP)
  sensitivity <- TP/(TP+FN)
  
  print(paste0("TRAIN accuracy: ", accuracy))
  print(paste0("TRAIN +precision: ", pos_precision))
  print(paste0("TRAIN -precision: ", neg_precision))
  print(paste0("TRAIN specifity: ", specificity))
  print(paste0("TRAIN sensitivity: ", sensitivity))
  
  ### PREDICTION AND TEST
  label <- test_dataset$SentimentValue 
  
  test_dataset <- test_dataset[,!(colnames(test_dataset) %in% colsToDel)] 
  
  for(i in 1:ncol(test_dataset)){
    test_dataset[,i] <- as.numeric(as.character(test_dataset[,i]))
  }
  
  sentiment_prediction <- predict(model, test_dataset[,names(test_dataset)!="SentimentValue"])
  
  # Print confusion matrix test
  test_confTable <- table(sentiment_prediction, label)
  
  print("***************************************************************************************")
  print("***************************************** TEST ****************************************")
  print("***************************************************************************************")
  
  print("TEST ConfMatrix : ")
  print(test_confTable)
  
  TN <- test_confTable[1,1]
  FN <- test_confTable[1,2]
  FP <- test_confTable[2,1]
  TP <- test_confTable[2,2]
  
  accuracy <- (TP+TN)/(TP+TN+FP+FN) 
  pos_precision <- TP/(TP+FP)
  neg_precision <- TN/(TN+FN)
  specificity <- TN/(TN+FP)
  sensitivity <- TP/(TP+FN)
  
  print(paste0("TEST accuracy: ", accuracy))
  print(paste0("TEST +precision: ", pos_precision))
  print(paste0("TEST -precision: ", neg_precision))
  print(paste0("TEST specifity: ", specificity))
  print(paste0("TEST sensitivity: ", sensitivity))
}

for(file in pathFiles) { 
  
  name <- gsub(".csv","", gsub("./Data/Train/","",file))
  train_dataset <- read.csv(file)
  train_dataset <- train_dataset[,!(colnames(train_dataset) %in% colsToDel)] 
  test_dataset <- read.csv(paste0("./Data/Test/",name,".csv"))
  
  sink(paste0("./Results/", name,".txt"))
  
  print(paste0("DATASET NAME: ", name))
  print(paste0("TRAIN INSTANCES: ", nrow(train_dataset)))
  print(paste0("TEST INSTANCES: ", nrow(test_dataset)))
  
  start_time <- Sys.time() 
  svm_model <- getModelSVM(train_dataset) 
  end_time <- Sys.time()
  model_time <- end_time - start_time
  getFinalResults("SVM", svm_model, train_dataset, test_dataset, model_time)
  
  start_time <- Sys.time() 
  j48_model <- getModelJ48(train_dataset)
  end_time <- Sys.time()
  getFinalResults("J48", j48_model, train_dataset, test_dataset, end_time - start_time)
  
  start_time <- Sys.time()
  xgboost_model <- getModelXGBoost(train_dataset)
  end_time <- Sys.time()
  getFinalResults("XGBoost", xgboost_model, train_dataset, test_dataset, end_time - start_time)  
  
  sink()  
}  
