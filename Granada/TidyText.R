#
# Description: Script to predict polarity with lexicon dictionary 
#              provided by Bin Lui to Granada Datasets
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

#BODEGAS
# Read the data
my_data <- read.csv(file="./Data/TripAdvisorBodegasEng.csv", stringsAsFactors = FALSE)

tidy_data <- getTidySentiment(my_data)

#Sentiment trough different restaurant opinions
ggplot(tidy_data$sentiments, aes(index, sentiment)) +
  geom_col(show.legend = FALSE, fill='red') +
  ggtitle("Bodegas castañeda Granada")

#Save the data with sentiments
write.csv(tidy_data$data, file="./Data/Tidy/Tidy_Bodegas.csv")
aux = data.frame(tidy_data$data)
save( aux, file="./Data/Tidy/Tidy_Bodegas.Rdata")


# CARMELA
# Read the data
my_data <- read.csv(file="./Data/TripAdvisorCarmelaEng.csv", stringsAsFactors = FALSE)

#Save the data with sentiments
tidy_data <- getTidySentiment(my_data)

#Sentiment trough different restaurant opinions
ggplot(tidy_data$sentiments, aes(index, sentiment)) +
  geom_col(show.legend = FALSE, fill='red') +
  ggtitle("Restaurante Carmela Granada")

#Save the data with sentiments
write.csv(tidy_data$data, file="./Data/Tidy/Tidy_Carmela.csv")
aux = data.frame(tidy_data$data)
save( aux, file="./Data/Tidy/Tidy_Carmela.Rdata")


# DIAMANTES
# Read the data
my_data <- read.csv(file="./Data/TripAdvisorDiamantesEng.csv", stringsAsFactors = FALSE)

#Save the data with sentiments
tidy_data <- getTidySentiment(my_data)

#Sentiment trough different restaurant opinions
ggplot(tidy_data$sentiments, aes(index, sentiment)) +
  geom_col(show.legend = FALSE, fill='red') +
  ggtitle("Restaurante los Diamantes Granada")

#Save the data with sentiments
write.csv(tidy_data$data, file="./Data/Tidy/Tidy_Diamantes.csv")
aux = data.frame(tidy_data$data)
save( aux, file="./Data/Tidy/Tidy_Diamantes.Rdata")


# ESTRELLAS
# Read the data
my_data <- read.csv(file="./Data/TripAdvisorEstrellasEng.csv", stringsAsFactors = FALSE)

#Save the data with sentiments
tidy_data <- getTidySentiment(my_data)

#Sentiment trough different restaurant opinions
ggplot(tidy_data$sentiments, aes(index, sentiment)) +
  geom_col(show.legend = FALSE, fill='red') +
  ggtitle("Estrellas de San Nicolas Granada")

#Save the data with sentiments
write.csv(tidy_data$data, file="./Data/Tidy/Tidy_Estrellas.csv")
aux = data.frame(tidy_data$data)
save( aux, file="./Data/Tidy/Tidy_Estrellas.Rdata")


# JARDINES
# Read the data
my_data <- read.csv(file="./Data/TripAdvisorJardinesEng.csv", stringsAsFactors = FALSE)

#Save the data with sentiments
tidy_data <- getTidySentiment(my_data)

#Sentiment trough different restaurant opinions
ggplot(tidy_data$sentiments, aes(index, sentiment)) +
  geom_col(show.legend = FALSE, fill='red') +
  ggtitle("Restaurantes Jardines Granada")

#Save the data with sentiments
write.csv(tidy_data$data, file="./Data/Tidy/Tidy_Jardines.csv")
aux = data.frame(tidy_data$data)
save( aux, file="./Data/Tidy/Tidy_Jardines.Rdata")
