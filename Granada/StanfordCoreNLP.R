#
# Description: Script to apply Stanford Core NLP to Granada Datasets
#
# Author: Manuel Noguera 

# Libraries 
library(data.table)
library(devtools)

#Only first time to download and install the program
#devtools::install_github("statsmaths/coreNLP")
#coreNLP::downloadCoreNLP()

library(coreNLP)

initCoreNLP()

getCoreNLPSentiment = function(TripAdvisor){
  
  # Creating sentiment label
  TripAdvisor$SentimentValue <- NA
  TripAdvisor$SentimentValue <- ifelse(TripAdvisor$rating <= 2, "negative", 
                                       ifelse(TripAdvisor$rating == 3, "neutral",
                                              ifelse(TripAdvisor$rating >= 4, "positive", TripAdvisor$SentimentValue)))
  
  # Predict sentiment with coreNLP
  TripAdvisor$SentimentCoreNLP <- NA
  for(i in 1:nrow(TripAdvisor)){
    print(i)
    pos <- 0
    neg <- 0
    
    opinion <- as.character(TripAdvisor$completeOpinion[i])
    opinion.df <- getSentiment(annotateString(opinion))
    
    for(j in 1:nrow(opinion.df)){
      if(opinion.df$sentiment[j]=="Verypositive"){
        pos = pos + 2
      } else if(opinion.df$sentiment[j]=="Positive"){
        pos = pos + 1
      } else if(opinion.df$sentiment[j]=="Negative"){
        neg = neg + 1
      } else if(opinion.df$sentiment[j]=="Verynegative"){
        neg = neg + 2
      }
    }
    
    TripAdvisor$pos[i] <- pos
    TripAdvisor$neg[i] <- neg
    
  }
  
  TripAdvisor$SentimentCoreNLP <- ifelse(TripAdvisor$pos > TripAdvisor$neg, "positive", 
                                         ifelse(TripAdvisor$pos < TripAdvisor$neg, "negative", "neutral"))
  return (TripAdvisor)
}

#BODEGAS
# Read the data
my_data <- read.csv(file="./Data/TripAdvisorBodegasEng.csv", header=TRUE, sep=",")

# Change name
DataCore <- getCoreNLPSentiment(my_data)

#Save the data with sentiments
write.csv(DataCore, file="./Data/Core/CoreNLP_Bodegas.csv")
save(DataCore, file="./Data/Core/CoreNLP_Bodegas.Rdata")


# CARMELA
# Read the data
my_data <- read.csv(file="./Data/TripAdvisorCarmelaEng.csv", header=TRUE, sep=",")

# Change name
DataCore <- getCoreNLPSentiment(my_data)

#Save the data with sentiments
write.csv(DataCore, file="./Data/Core/CoreNLP_Carmela.csv")
save(DataCore, file="./Data/Core/CoreNLP_Carmela.Rdata")


# DIAMANTES
# Read the data
my_data <- read.csv(file="./Data/TripAdvisorDiamantesEng.csv", header=TRUE, sep=",")

# Change name
DataCore <- getCoreNLPSentiment(my_data)

#Save the data with sentiments
write.csv(DataCore, file="./Data/Core/CoreNLP_Diamantes.csv")
save(DataCore, file="./Data/Core/CoreNLP_Diamantes.Rdata")


# ESTRELLAS
# Read the data
my_data <- read.csv(file="./Data/TripAdvisorEstrellasEng.csv", header=TRUE, sep=",")

# Change name
DataCore <- getCoreNLPSentiment(my_data)

#Save the data with sentiments
write.csv(DataCore, file="./Data/Core/CoreNLP_Estrellas.csv")
save(DataCore, file="./Data/Core/CoreNLP_Estrellas.Rdata")


# JARDINES
# Read the data
my_data <- read.csv(file="./Data/TripAdvisorJardinesEng.csv", header=TRUE, sep=",")

# Change name
DataCore <- getCoreNLPSentiment(my_data)

#Save the data with sentiments
write.csv(DataCore, file="./Data/Core/CoreNLP_Jardines.csv")
save(DataCore, file="./Data/Core/CoreNLP_Jardines.Rdata")
