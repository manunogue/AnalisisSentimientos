#
# Description: Script to predict polarity with lexicon dictionary 
#              provided by Bin Lui to Barcelona Datasets
#
# Author: Manuel Noguera 

# Libraries
library(tidytext)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)

# Extracts polarity with Bin Lui dictionary
getTidySentiment = function(TripAdvisor){
  
  my_data <- TripAdvisor %>%
    mutate(opinionID = row_number())
  
  # Split opinions in tokens
  tidy_opinions <- unnest_tokens(my_data, word, completeOpinion) 
  
  # Just to know
  nrc_joy <- get_sentiments("nrc") %>% 
    filter(sentiment == "joy")
  tidy_opinions %>%
    inner_join(nrc_joy) %>%
    count(word, sort = TRUE)
  
  # Get Bin Lui puntuation
  opinionsSentiment <- tidy_opinions %>%
    inner_join(get_sentiments("bing")) %>%
    count(index = opinionID, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%  
    mutate(linea = row_number())
  
  aux <- data.frame(opinionsSentiment)
  updated = TRUE
  
  while(updated){
    updated = FALSE
    for(i in 1:nrow(aux)){
      if(aux$index[i] != aux$linea[i]){
        updated = TRUE
        print(i)
        aux <- rbind( aux[1:(i-1),], c(i,0,0,i), aux[i:nrow(aux),] ) 
        aux$linea[(i+1):nrow(aux)] = aux$linea[(i+1):nrow(aux)] + 1 
      }
    }
  }
  
  opinionsSentiment <- aux  %>%
    mutate(sentiment = positive - negative)
  
  # Creating real sentiment label
  TripAdvisor$SentimentValue <- NA
  TripAdvisor$SentimentValue <- ifelse(TripAdvisor$rating <= 2, "negative", 
                                       ifelse(TripAdvisor$rating == 3, "neutral",
                                              ifelse(TripAdvisor$rating >= 4, "positive", TripAdvisor$SentimentValue)))
  
  # Creating tidy sentiment label  
  TripAdvisor$TidySentiment <- ifelse(opinionsSentiment$sentiment > 1, "positive", 
                                      ifelse(opinionsSentiment$sentiment < -1, "negative", "neutral"))
  return(list(data = TripAdvisor, sentiments = opinionsSentiment))
}

#CERVECERIA 
# Read the data
my_data <- read.csv(file="./Data/TripAdvisorCerveceriaEng.csv", stringsAsFactors = FALSE)

tidy_data <- getTidySentiment(my_data)

#Sentiment trough different restaurant opinions
ggplot(tidy_data$sentiments, aes(index, sentiment)) +
  geom_col(show.legend = FALSE, fill='red') +
  ggtitle("Cervecería Catalana Barcelona")

#Save the data with sentiments
write.csv(tidy_data$data, file="./Data/Tidy/Tidy_Cerveceria.csv")
aux = data.frame(tidy_data$data)
save(aux, file="./Data/Tidy/Tidy_Cerveceria.Rdata")


#CONDAL
# Read the data
my_data <- read.csv(file="./Data/TripAdvisorCondalEng.csv", stringsAsFactors = FALSE)

# Change name
tidy_data <- getTidySentiment(my_data)

#Sentiment trough different restaurant opinions
ggplot(tidy_data$sentiments, aes(index, sentiment)) +
  geom_col(show.legend = FALSE, fill='red') +
  ggtitle("Ciudad Condal Barcelona")

#Save the data with sentiments
write.csv(tidy_data$data, file="./Data/Tidy/Tidy_Condal.csv")
aux = data.frame(tidy_data$data)
save(aux, file="./Data/Tidy/Tidy_Condal.Rdata")


#HRBARCELONA
# Read the data
my_data <- read.csv(file="./Data/TripAdvisorHRBarcelonaEng.csv", stringsAsFactors = FALSE)

tidy_data <- getTidySentiment(my_data)

#Sentiment trough different restaurant opinions
ggplot(tidy_data$sentiments, aes(index, sentiment)) +
  geom_col(show.legend = FALSE, fill='red') +
  ggtitle("Hard Rock Café Barcelona")

#Save the data with sentiments
write.csv(tidy_data$data, file="./Data/Tidy/Tidy_HRB.csv")
aux = data.frame(tidy_data$data)
save(aux, file="./Data/Tidy/Tidy_HRB.Rdata")


#TAPEO
# Read the data
my_data <- read.csv(file="./Data/TripAdvisorTapeoEng.csv", stringsAsFactors = FALSE)

tidy_data <- getTidySentiment(my_data)

#Sentiment trough different restaurant opinions
ggplot(tidy_data$sentiments, aes(index, sentiment)) +
  geom_col(show.legend = FALSE, fill='red') +
  ggtitle("Tapeo Barcelona")

#Save the data with sentiments
write.csv(tidy_data, file="./Data/Tidy/Tidy_Tapeo.csv")
aux = data.frame(tidy_data$data)
save(aux, file="./Data/Tidy/Tidy_Tapeo.Rdata")


#TERESA
# Read the data
my_data <- read.csv(file="./Data/TripAdvisorTeresaEng.csv", stringsAsFactors = FALSE)

tidy_data <- getTidySentiment(my_data)

#Sentiment trough different restaurant opinions
ggplot(tidy_data$sentiments, aes(index, sentiment)) +
  geom_col(show.legend = FALSE, fill='red') +
  ggtitle("Restaurante Teresa Barcelona")

#Save the data with sentiments
write.csv(tidy_data, file="./Data/Tidy/Tidy_Teresa.csv")
aux = data.frame(tidy_data$data)
save(aux, file="./Data/Tidy/Tidy_Teresa.Rdata")

