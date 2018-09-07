#
# Description: Script to apply data preprocessing in Madrid restaurants
#
# Author: Manuel Noguera 

# Utilities
source("../PreprocessingUtils.R")

# Load datasets
TripAdvisorHRM <- read.csv(file="./Data/Core/CoreNLP_HRM.csv", header=TRUE, sep=",")
TripAdvisorBotin <- read.csv(file="./Data/Core/CoreNLP_Botin.csv", header=TRUE, sep=",")
TripAdvisorElSur <- read.csv(file="./Data/Core/CoreNLP_ElSur.csv", header=TRUE, sep=",")
TripAdvisorMSM <- read.csv(file="./Data/Core/CoreNLP_MercadoSM.csv", header=TRUE, sep=",")
TripAdvisorTCT <- read.csv(file="./Data/Core/CoreNLP_TenConTen.csv", header=TRUE, sep=",")

# Get datasets with unigrams doc term
getDocumentTermMatrix(TripAdvisorHRM, "DatasetHRM", TRUE, "M")
getDocumentTermMatrix(TripAdvisorBotin, "DatasetBotin", TRUE, "M")
getDocumentTermMatrix(TripAdvisorElSur, "DatasetElSur", TRUE, "M")
getDocumentTermMatrix(TripAdvisorMSM, "DatasetMSM", TRUE, "M")
getDocumentTermMatrix(TripAdvisorTCT, "DatasetTCT", TRUE, "M")

# Get datasets with bigrams doc term
getDocumentTermMatrix(TripAdvisorHRM, "DatasetHRM", FALSE, "M")
getDocumentTermMatrix(TripAdvisorBotin, "DatasetBotin", FALSE, "M")
getDocumentTermMatrix(TripAdvisorElSur, "DatasetElSur", FALSE, "M")
getDocumentTermMatrix(TripAdvisorMSM, "DatasetMSM", FALSE, "M")
getDocumentTermMatrix(TripAdvisorTCT, "DatasetTCT", FALSE, "M")
