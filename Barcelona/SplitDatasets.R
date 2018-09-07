#
# Description: Script datasets into Training and test managing unbalancement
#
# Author: Manuel Noguera 

# Libraries
library(caret)
library(randomForest)
library(plyr)
library(data.table)
library(xgboost)
library(unbalanced)

# Seed used in random operations
set.seed(47259346)

# Percentage split train/test:
percentage <- 0.75

# Unbalanced ratio
imbalance <- c(10, 5, 2, 1)

# Only document term matrix is used to adjust models
colsToDel <- c("X", "id", "rating", "date", "page", "username", "quote", "opinionBody", "SentimentCoreNLP", "pos", "neg")

unigrams <- list.files(path = "./Data/Features/Unigrams", pattern = ".csv$", all.files = FALSE, full.names = TRUE, 
                       recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

bigrams <- list.files(path = "./Data/Features/Bigrams", pattern = ".csv$", all.files = FALSE, full.names = TRUE, 
                      recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
# Load all datasets (unigrams and bigrams) path
pathFiles <- c(unigrams, bigrams) 

for (file in pathFiles)
{
  dataset <- read.csv(file=file, header=TRUE, sep=",")
  dataset <- dataset[,!(colnames(dataset) %in% colsToDel)] 
  
  sampleSize <- floor(percentage * nrow(dataset))
  
  # Generate train indexes
  indexes <- sample(seq_len(nrow(dataset)), size = sampleSize)
  
  # Split into train and test
  train <- dataset[indexes,]
  test <- dataset[-indexes,]
  
  imbalanceRatio <- table(train$SentimentValue)[2]/table(train$SentimentValue)[1]
  minorityIndexes <- (1:nrow(train))[train$SentimentValue == "negative"]  
  
  if(grepl("Unigrams",file)){
    dataset_name <- gsub("./Data/Features/Unigrams/Dataset", "", gsub(".csv","",file)) 
    train_path <- paste0("./Data/Train/",dataset_name,"_Uni_IR_",0,".csv")
    test_path <- paste0("./Data/Test/",dataset_name,"_Uni_IR_",0,".csv")
  }else{
    dataset_name <- gsub("./Data/Features/Bigrams/Dataset", "", gsub(".csv","",file))
    train_path <- paste0("./Data/Train/",dataset_name,"_Bi_IR_",0,".csv")
    test_path <- paste0("./Data/Test/",dataset_name,"_Bi_IR_",0,".csv")
  }
  
  # Save train and test withour ROS
  write.csv(train, file=train_path )
  write.csv(test, file=test_path)
  
  # Balance Datasets
  for( i in imbalance)
  {
    new_train <- train
    nNewSamples <- round((1/i)*((nrow(train)-length(minorityIndexes))-length(minorityIndexes)))
    
    duplicate <- sample(minorityIndexes, nNewSamples, replace = T)
    
    for( element in duplicate)
    {
      new_train <- rbind(new_train, train[element,])
    }
    
    new_train <- new_train[sample(seq_len(nrow(new_train))),]
    
    newIR <- table(train$SentimentValue)[2]/table(train$SentimentValue)[1]    
    
    if(grepl("Unigrams",file)){
      dataset_name <- gsub("./Data/Features/Unigrams/Dataset", "", gsub(".csv","",file)) 
      train_path <- paste0("./Data/Train/",dataset_name,"_Uni_IR_",i,".csv")
      test_path <- paste0("./Data/Test/",dataset_name,"_Uni_IR_",i,".csv")
    }else{
      dataset_name <- gsub("./Data/Features/Bigrams/Dataset", "", gsub(".csv","",file))
      train_path <- paste0("./Data/Train/",dataset_name,"_Bi_IR_",i,".csv")
      test_path <- paste0("./Data/Test/",dataset_name,"_Bi_IR_",i,".csv")
    }
    
    write.csv(new_train, file=train_path )
    write.csv(test, file=test_path)
  }
}
