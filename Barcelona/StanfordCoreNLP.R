#
# Description: Script to apply Stanford Core NLP to Barcelona Datasets
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

#CERVECERIA 
# Read the data
MyData <- read.csv(file="C:/Users/manue/Desktop/TFG/Barna/Data/TripAdvisorCerveceriaEng.csv", header=TRUE, sep=",")

# Change name
DataCore <- getCoreNLPSentiment(MyData)

#Save the data with sentiments
write.csv(DataCore, file="C:/Users/manue/Desktop/TFG/Barna/Data/Core/CoreNLP_Cerveceria.csv")
save(DataCore, file="C:/Users/manue/Desktop/TFG/Barna/Data/Core/CoreNLP_Cerveceria.Rdata")



#CONDAL
# Read the data
MyData <- read.csv(file="C:/Users/manue/Desktop/TFG/Barna/Data/TripAdvisorCondalEng.csv", header=TRUE, sep=",")

# Change name
DataCore <- getCoreNLPSentiment(MyData)

#Save the data with sentiments
write.csv(DataCore, file="C:/Users/manue/Desktop/TFG/Barna/Data/Core/CoreNLP_Condal.csv")
save(DataCore, file="C:/Users/manue/Desktop/TFG/Barna/Data/Core/CoreNLP_Condal.Rdata")



#HRBARCELONA
# Read the data
MyData <- read.csv(file="C:/Users/manue/Desktop/TFG/Barna/Data/TripAdvisorHRBarcelonaEng.csv", header=TRUE, sep=",")

# Change name
DataCore <- getCoreNLPSentiment(MyData)

#Save the data with sentiments
write.csv(DataCore, file="C:/Users/manue/Desktop/TFG/Barna/Data/Core/CoreNLP_HRB.csv")
save(DataCore, file="C:/Users/manue/Desktop/TFG/Barna/Data/Core/CoreNLP_HRB.Rdata")



#TAPEO
# Read the data
MyData <- read.csv(file="C:/Users/manue/Desktop/TFG/Barna/Data/TripAdvisorTapeoEng.csv", header=TRUE, sep=",")

# Change name
DataCore <- getCoreNLPSentiment(MyData)

#Save the data with sentiments
write.csv(DataCore, file="C:/Users/manue/Desktop/TFG/Barna/Data/Core/CoreNLP_Tapeo.csv")
save(DataCore, file="C:/Users/manue/Desktop/TFG/Barna/Data/Core/CoreNLP_Tapeo.Rdata")



#TERESA
# Read the data
MyData <- read.csv(file="C:/Users/manue/Desktop/TFG/Barna/Data/TripAdvisorTeresaEng.csv", header=TRUE, sep=",")

# Change name
DataCore <- getCoreNLPSentiment(MyData)

#Save the data with sentiments
write.csv(DataCore, file="C:/Users/manue/Desktop/TFG/Barna/Data/Core/CoreNLP_Teresa.csv")
save(DataCore, file="C:/Users/manue/Desktop/TFG/Barna/Data/Core/CoreNLP_Teresa.Rdata")
