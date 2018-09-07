#
# Description: Script to apply Stanford Core NLP to Madrid Datasets
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


#HARD ROCK HOTEL
# Read the data
MyData <- read.csv(file="./Data/TripAdvisorHRMadridEng.csv", header=TRUE, sep=",")

# Change name
DataCore <- getCoreNLPSentiment(MyData)

#Save the data with sentiments
write.csv(DataCore, file="./Data/Core/CoreNLP_HRM.csv")
save(DataCore, file="./Data/Core/CoreNLP_HRM.Rdata")


# BOTIN
# Read the data
MyData <- read.csv(file="./Data/TripAdvisorBotinEng.csv", header=TRUE, sep=",")

# Change name
DataCore <- getCoreNLPSentiment(MyData)

#Save the data with sentiments
write.csv(DataCore, file="./Data/Core/CoreNLP_Botin.csv")
save(DataCore, file="./Data/Core/CoreNLP_Botin.Rdata")


# EL SUR
# Read the data
MyData <- read.csv(file="./Data/TripAdvisorElSurEng.csv", header=TRUE, sep=",")

# Change name
DataCore <- getCoreNLPSentiment(MyData)

#Save the data with sentiments
write.csv(DataCore, file="./Data/Core/CoreNLP_ElSur.csv")
save(DataCore, file="./Data/Core/CoreNLP_ElSur.Rdata")


# MERCADO DE SAN MIGUEL
# Read the data
MyData <- read.csv(file="./Data/TripAdvisorMercadoSMEng.csv", header=TRUE, sep=",")

# Change name
DataCore <- getCoreNLPSentiment(MyData)

#Save the data with sentiments
write.csv(DataCore, file="./Data/Core/CoreNLP_MercadoSM.csv")
save(DataCore, file="./Data/Core/CoreNLP_MercadoSM.Rdata")


# TEN CON TEN
# Read the data
MyData <- read.csv(file="./Data/TripAdvisorTenConTenEng.csv", header=TRUE, sep=",")

# Change name
DataCore <- getCoreNLPSentiment(MyData)

#Save the data with sentiments
write.csv(DataCore, file="./Data/Core/CoreNLP_TenConTen.csv")
save(DataCore, file="./Data/Core/CoreNLP_TenConTen.Rdata")
