#
# Description: Script to apply data preprocessing in Barcelona restaurants
#
# Author: Manuel Noguera 

# Utilities
source("../PreprocessingUtils.R")

# Load datasets
TripAdvisorCerveceria <- read.csv(file="./Data/Core/CoreNLP_Cerveceria.csv", header=TRUE, sep=",")
TripAdvisorCondal <- read.csv(file="./Data/Core/CoreNLP_Condal.csv", header=TRUE, sep=",")
TripAdvisorHRB <- read.csv(file="./Data/Core/CoreNLP_HRB.csv", header=TRUE, sep=",")
TripAdvisorTapeo <- read.csv(file="./Data/Core/CoreNLP_Tapeo.csv", header=TRUE, sep=",")
TripAdvisorTeresa <- read.csv(file="./Data/Core/CoreNLP_Teresa.csv", header=TRUE, sep=",")

# Get datasets with unigrams doc term
getDocumentTermMatrix(TripAdvisorCerveceria, "DatasetCerveceria", TRUE, "B")
getDocumentTermMatrix(TripAdvisorCondal, "DatasetCondal", TRUE, "B")
getDocumentTermMatrix(TripAdvisorHRB, "DatasetHRB", TRUE, "B")
getDocumentTermMatrix(TripAdvisorTapeo, "DatasetTapeo", TRUE, "B")
getDocumentTermMatrix(TripAdvisorTeresa, "DatasetTeresa", TRUE, "B")

# Get datasets with bigrams doc term
getDocumentTermMatrix(TripAdvisorCerveceria, "DatasetCerveceria", FALSE, "B")
getDocumentTermMatrix(TripAdvisorCondal, "DatasetCondal", FALSE, "B")
getDocumentTermMatrix(TripAdvisorHRB, "DatasetHRB", FALSE, "B")
getDocumentTermMatrix(TripAdvisorTapeo, "DatasetTapeo", FALSE, "B")
getDocumentTermMatrix(TripAdvisorTeresa, "DatasetTeresa", FALSE, "B")

