#
# Description: Script to apply data preprocessing in Granada restaurants
#
# Author: Manuel Noguera 

# Utilities
source("../PreprocessingUtils.R")

# Load datasets
TripAdvisorBodegas <- read.csv(file="./Data/Core/CoreNLP_Bodegas.csv", header=TRUE, sep=",")
TripAdvisorCarmela <- read.csv(file="./Data/Core/CoreNLP_Carmela.csv", header=TRUE, sep=",")
TripAdvisorDiamantes <- read.csv(file="./Data/Core/CoreNLP_Diamantes.csv", header=TRUE, sep=",")
TripAdvisorEstrellas <- read.csv(file="./Data/Core/CoreNLP_Estrellas.csv", header=TRUE, sep=",")
TripAdvisorJardines <- read.csv(file="./Data/Core/CoreNLP_Jardines.csv", header=TRUE, sep=",")

# Get datasets with unigrams doc term
getDocumentTermMatrix(TripAdvisorBodegas, "DatasetBodegas", TRUE, "G")
getDocumentTermMatrix(TripAdvisorCarmela, "DatasetCarmela", TRUE, "G")
getDocumentTermMatrix(TripAdvisorDiamantes, "DatasetDiamantes", TRUE, "G")
getDocumentTermMatrix(TripAdvisorEstrellas, "DatasetEstrellas", TRUE, "G")
getDocumentTermMatrix(TripAdvisorJardines, "DatasetJardines", TRUE, "G")

# Get datasets with bigrams doc term
getDocumentTermMatrix(TripAdvisorBodegas, "DatasetBodegas", FALSE, "G")
getDocumentTermMatrix(TripAdvisorCarmela, "DatasetCarmela", FALSE, "G")
getDocumentTermMatrix(TripAdvisorDiamantes, "DatasetDiamantes", FALSE, "G")
getDocumentTermMatrix(TripAdvisorEstrellas, "DatasetEstrellas", FALSE, "G")
getDocumentTermMatrix(TripAdvisorJardines, "DatasetJardines", FALSE, "G")
