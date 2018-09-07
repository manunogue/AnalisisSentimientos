#
# Description: Script to download and store resturants
#               opinions from the web Tripadvisor.com
#
# Author: Manuel Noguera 

# Libraries
library(rvest)

# Change language
Sys.setlocale("LC_TIME", "English")


# Go throw all the html nodes and save relevant information
# url: Main restaurant web
# userurl: Opinions url
# page: page identifier
getPageInfo = function(url, usersurl, page){    

    # Name
    users <- url %>%
      read_html() %>%
      html_nodes(".member_info")
    
    username <- users %>%
      html_node(".username") %>%
      html_text()
    username <- gsub("\n", "", username)
    
    # About reviews 
    reviews <- url %>%
      read_html() %>%
      html_nodes("#REVIEWS .innerBubble")
      
    rating <- reviews %>%
      html_node(".ui_bubble_rating") %>%
      html_attr("class") %>%
      gsub("ui_bubble_rating bubble_", "", .) %>%
      as.integer()
    rating <- rating/10
  
    # ID reviewrs
    id <- reviews %>%
      html_node(".quote a") %>%
      html_attr("id")
    
    quote <- reviews %>%
      html_node(".quote span") %>%
      html_text()
      
    # Date of the opinion
    date <- reviews %>%
      html_node(".rating .ratingDate") %>%
      html_attr("title") %>%
      as.Date("%d %b %Y")
            
    # Opinions
    opinionBody <- as.character(c(1:length(id)))
    
    preurl = strsplit(usersurl,"XXX")[[1]][1]
    posturl = strsplit(usersurl,"XXX")[[1]][2]
    
    for(i in 1:length(id)){
      completeReviewURL <- paste0(preurl, gsub("rn", "", id[i]),posturl)
      reviews_aux <- completeReviewURL %>%
              read_html() %>%
              html_nodes(".entry") %>%
              html_text() 
      
      opinionBody[i] <- repair_encoding(gsub(paste0('.*"review_',gsub("rn", "", id[i]),'\">\n|\n.*'), "", as.character(reviews_aux[1])), "UTF-8")
    }
    
    # Merge title and opinionBody
    completeOpinion <- paste(quote,opinionBody, sep=". ")

    # Page in TripAdvisor
    page <- rep(page, length(username))
        
    temp.TripAdvisor <- data.frame(id, completeOpinion, rating, date, page, username, quote, opinionBody, stringsAsFactors = FALSE) 
    
    return (temp.TripAdvisor)
}

#********************************************************************************************************************************
#                                               MADRID      
#********************************************************************************************************************************

# HARD ROCK HOTEL 
TripAdvisorHRMadrid<- data.frame()
totalpages <- 64
last <- 1

usersurl <- paste0("https://www.tripadvisor.co.uk/ShowUserReviews-g187514-d697590-rXXX-Hard_Rock_Cafe-Madrid.html#REVIEWS")

for(k in last:totalpages){
  print(k)
  if(k == 1) {
    url <- paste0("https://www.tripadvisor.co.uk/Restaurant_Review-g187514-d697590-Reviews-Hard_Rock_Cafe-Madrid.html#REVIEWS")
  } else {
    url <- paste0("https://www.tripadvisor.co.uk/Restaurant_Review-g187514-d697590-Reviews-or",k-1,"0-Hard_Rock_Cafe-Madrid.html")
  }
  
  TripAdvisorHRMadrid <- rbind(TripAdvisorHRMadrid, getPageInfo(url,usersurl,k))
}

write.csv(TripAdvisorHRMadrid, file="./Data/TripAdvisorHRMadridEng.csv", row.names = FALSE)
save(TripAdvisorHRMadrid, file="./Data/TripAdvisorHRMadridEng.Rdata")



# TABERNA EL SUR
TripAdvisorElSur<- data.frame()
totalpages <- 180
last <- 1
usersurl <- paste0("https://www.tripadvisor.co.uk/ShowUserReviews-g187514-d2527526-rXXX-Taberna_el_Sur-Madrid.html#REVIEWS")

for(k in last:totalpages){
  print(k)
  if(k == 1) {
    url <- paste0("https://www.tripadvisor.co.uk/Restaurant_Review-g187514-d2527526-Reviews-Taberna_el_Sur-Madrid.html#REVIEWS")
  } else {        
    url <- paste0("https://www.tripadvisor.co.uk/Restaurant_Review-g187514-d2527526-Reviews-or",k-1,"0-Taberna_el_Sur-Madrid.html")
  }
  
  TripAdvisorElSur <- rbind(TripAdvisorElSur, getPageInfo(url,usersurl,k))
}

write.csv(TripAdvisorElSur, file="./Data/TripAdvisorElSurEng.csv", row.names = FALSE)
save(TripAdvisorElSur, file="./Data/TripAdvisorElSurEng.Rdata")



# TEN CON TEN
TripAdvisorTCT<- data.frame()
totalpages <- 107
last <- 1     
usersurl <- paste0("https://www.tripadvisor.co.uk/ShowUserReviews-g187514-d2489978-rXXX-Ten_Con_Ten-Madrid.html#REVIEWS")

for(k in last:totalpages){
  print(k)
  if(k == 1) {
    url <- paste0("https://www.tripadvisor.co.uk/Restaurant_Review-g187514-d2489978-Reviews-Ten_Con_Ten-Madrid.html#REVIEWS")
  } else {        
    url <- paste0("https://www.tripadvisor.co.uk/Restaurant_Review-g187514-d2489978-Reviews-or",k-1,"0-Ten_Con_Ten-Madrid.html")
  }
  
  TripAdvisorTCT <- rbind(TripAdvisorTCT, getPageInfo(url,usersurl,k))
}

write.csv(TripAdvisorTCT, file="./Data/TripAdvisorTenConTenEng.csv", row.names = FALSE)
save(TripAdvisorTCT, file="./Data/TripAdvisorTenConTenEng.Rdata")



# MERCADO DE SAN MIGUEL
TripAdvisorSanMiguel<- data.frame()
totalpages <- 315
last <- 1           
usersurl <- paste0("https://www.tripadvisor.co.uk/ShowUserReviews-g187514-d6367286-rXXX-Mercado_de_San_Miguel-Madrid.html#REVIEWS")

for(k in last:totalpages){
  print(k)
  if(k == 1) {
    url <- paste0("https://www.tripadvisor.co.uk/Restaurant_Review-g187514-d6367286-Reviews-Mercado_de_San_Miguel-Madrid.html#REVIEWS")
  } else {         
    url <- paste0("https://www.tripadvisor.co.uk/Restaurant_Review-g187514-d6367286-Reviews-or",k-1,"0-Mercado_de_San_Miguel-Madrid.html#REVIEWS")
  }
  
  TripAdvisorSanMiguel <- rbind(TripAdvisorSanMiguel, getPageInfo(url,usersurl,k))
}

write.csv(TripAdvisorSanMiguel, file="./Data/TripAdvisorMercadoSMEng.csv", row.names = FALSE)
save(TripAdvisorSanMiguel, file="./Data/TripAdvisorMercadoSMEng.Rdata")



# BOTIN
TripAdvisorBotin <- data.frame()
totalpages <- 314
last <- 1 
usersurl <- paste0("https://www.tripadvisor.co.uk/ShowUserReviews-g187514-d1094538-rXXX-Restuarant_Botin-Madrid.html#REVIEWS")

for(k in last:totalpages){
  print(k)
  if(k == 1) {
    url <- paste0("https://www.tripadvisor.co.uk/Restaurant_Review-g187514-d1094538-Reviews-Restuarant_Botin-Madrid.html#REVIEWS")
  } else {           
    url <- paste0("https://www.tripadvisor.co.uk/Restaurant_Review-g187514-d1094538-Reviews-or",k-1,"0-Restuarant_Botin-Madrid.html#REVIEWS")
  }
  
  TripAdvisorBotin <- rbind(TripAdvisorBotin, getPageInfo(url,usersurl,k))
}

write.csv(TripAdvisorBotin, file="./Data/TripAdvisorBotinEng.csv", row.names = FALSE)
save(TripAdvisorBotin, file="./Data/TripAdvisorBotinEng.Rdata")



#********************************************************************************************************************************
#                                               GRANADA     
#********************************************************************************************************************************

# LOS DIAMANTES
TripAdvisorDiamantes <- data.frame()
totalpages <- 65
last <- 1 
usersurl <- paste0("https://www.tripadvisor.co.uk/ShowUserReviews-g187441-d900012-rXXX-Bar_Los_Diamantes-Granada_Province_of_Granada_Andalucia.html#REVIEWS")

for(k in last:totalpages){
  print(k)
  if(k == 1) {
    url <- paste0("https://www.tripadvisor.co.uk/Restaurant_Review-g187441-d900012-Reviews-Bar_Los_Diamantes-Granada_Province_of_Granada_Andalucia.html")
  } else {           
    url <- paste0("https://www.tripadvisor.co.uk/Restaurant_Review-g187441-d900012-Reviews-or",k-1,"0-Bar_Los_Diamantes-Granada_Province_of_Granada_Andalucia.html")
  }
  
  TripAdvisorDiamantes <- rbind(TripAdvisorDiamantes, getPageInfo(url,usersurl,k))
}

write.csv(TripAdvisorDiamantes, file="./Data/TripAdvisorDiamantesEng.csv", row.names = FALSE)
save(TripAdvisorDiamantes, file="./Data/TripAdvisorDiamantesEng.Rdata")



# BODEGAS CASTAÑEDA
TripAdvisorBodegas <- data.frame()
totalpages <- 78
last <- 1 
usersurl <- paste0("https://www.tripadvisor.co.uk/ShowUserReviews-g187441-d1501184-rXXX-Bodegas_Castaneda-Granada_Province_of_Granada_Andalucia.html#REVIEWS")

for(k in last:totalpages){
  print(k)
  if(k == 1) {
    url <- paste0("https://www.tripadvisor.co.uk/Restaurant_Review-g187441-d1501184-Reviews-Bodegas_Castaneda-Granada_Province_of_Granada_Andalucia.html")
  } else {           
    url <- paste0("https://www.tripadvisor.co.uk/Restaurant_Review-g187441-d1501184-Reviews-or",k-1,"0-Bodegas_Castaneda-Granada_Province_of_Granada_Andalucia.html")
  }
  
  TripAdvisorBodegas <- rbind(TripAdvisorBodegas, getPageInfo(url,usersurl,k))
}

write.csv(TripAdvisorBodegas, file="./Data/TripAdvisorBodegasEng.csv", row.names = FALSE)
save(TripAdvisorBodegas, file="./Data/TripAdvisorBodegasEng.Rdata")



# ESTRELLAS DE SAN NICOLÁS
TripAdvisorEstrellas <- data.frame()
totalpages <- 67
last <- 1 
usersurl <- paste0("https://www.tripadvisor.co.uk/ShowUserReviews-g187441-d1470075-rXXX-Restaurante_Estrellas_de_San_Nicolas-Granada_Province_of_Granada_Andalucia.html#REVIEWS")

for(k in last:totalpages){
  print(k)
  if(k == 1) {
    url <- paste0("https://www.tripadvisor.co.uk/Restaurant_Review-g187441-d1470075-Reviews-Restaurante_Estrellas_de_San_Nicolas-Granada_Province_of_Granada_Andalucia.html")
  } else {           
    url <- paste0("https://www.tripadvisor.co.uk/Restaurant_Review-g187441-d1470075-Reviews-or",k-1,"0-Restaurante_Estrellas_de_San_Nicolas-Granada_Province_of_Granada_Andalucia.html")
  }
  
  TripAdvisorEstrellas <- rbind(TripAdvisorEstrellas, getPageInfo(url,usersurl,k))
}

write.csv(TripAdvisorEstrellas, file="./Data/TripAdvisorEstrellasEng.csv", row.names = FALSE)
save(TripAdvisorEstrellas, file="./Data/TripAdvisorEstrellasEng.Rdata")



# JARDINES DE ZORAYA
TripAdvisorJardines <- data.frame()
totalpages <- 100
last <- 1 
usersurl <- paste0("https://www.tripadvisor.co.uk/ShowUserReviews-g187441-d1860381-rXXX-Jardines_de_Zoraya-Granada_Province_of_Granada_Andalucia.html#REVIEWS")

for(k in last:totalpages){
  print(k)
  if(k == 1) {
    url <- paste0("https://www.tripadvisor.co.uk/Restaurant_Review-g187441-d1860381-Reviews-Jardines_de_Zoraya-Granada_Province_of_Granada_Andalucia.html")
  } else {           
    url <- paste0("https://www.tripadvisor.co.uk/Restaurant_Review-g187441-d1860381-Reviews-or",k-1,"0-Jardines_de_Zoraya-Granada_Province_of_Granada_Andalucia.html")
  }
  
  TripAdvisorJardines <- rbind(TripAdvisorJardines, getPageInfo(url,usersurl,k))
}

write.csv(TripAdvisorJardines, file="./Data/TripAdvisorJardinesEng.csv", row.names = FALSE)
save(TripAdvisorJardines, file="./Data/TripAdvisorJardinesEng.Rdata")



# RESTAURANTE CARMELA
TripAdvisorCarmela <- data.frame()
totalpages <- 53
last <- 1 
usersurl <- paste0("https://www.tripadvisor.co.uk/ShowUserReviews-g187441-d3693887-rXXX-Carmela_Restaurante-Granada_Province_of_Granada_Andalucia.html#REVIEWS")

for(k in last:totalpages){
  print(k)
  if(k == 1) {
    url <- paste0("https://www.tripadvisor.co.uk/Restaurant_Review-g187441-d3693887-Reviews-Carmela_Restaurante-Granada_Province_of_Granada_Andalucia.html")
  } else {           
    url <- paste0("https://www.tripadvisor.co.uk/Restaurant_Review-g187441-d3693887-Reviews-or",k-1,"0-Carmela_Restaurante-Granada_Province_of_Granada_Andalucia.html")
  }
  
  TripAdvisorCarmela <- rbind(TripAdvisorCarmela, getPageInfo(url,usersurl,k))
}

write.csv(TripAdvisorCarmela, file="./Data/TripAdvisorCarmelaEng.csv", row.names = FALSE)
save(TripAdvisorCarmela, file="./Data/TripAdvisorCarmelaEng.Rdata")


#********************************************************************************************************************************
#                                               BARCELONA    
#********************************************************************************************************************************

# HARD ROCK CAFE BARCELONA
TripAdvisorHRBarcelona <- data.frame()
totalpages <- 173
last <- 1 
  usersurl <- paste0("https://www.tripadvisor.co.uk/ShowUserReviews-g187497-d913500-rXXX-Hard_Rock_Cafe-Barcelona_Catalonia.html#REVIEWS")

for(k in last:totalpages){
  print(k)
  if(k == 1) {
    url <- paste0("https://www.tripadvisor.co.uk/Restaurant_Review-g187497-d913500-Reviews-Hard_Rock_Cafe-Barcelona_Catalonia.html")
  } else {           
    url <- paste0("https://www.tripadvisor.co.uk/Restaurant_Review-g187497-d913500-Reviews-or",k-1,"0-Hard_Rock_Cafe-Barcelona_Catalonia.html")
  }
  
  TripAdvisorHRBarcelona<- rbind(TripAdvisorHRBarcelona, getPageInfo(url,usersurl,k))
}

write.csv(TripAdvisorHRBarcelona, file="./Data/TripAdvisorHRBarcelonaEng.csv", row.names = FALSE)
save(TripAdvisorHRBarcelona, file="./Data/TripAdvisorHRBarcelonaEng.Rdata")



# CERVECERÍA CATALANA
TripAdvisorCerveceria <- data.frame()
totalpages <- 480
last <- 1 
usersurl <- paste0("https://www.tripadvisor.co.uk/ShowUserReviews-g187497-d782944-rXXX-Cerveceria_Catalana-Barcelona_Catalonia.html#REVIEWS")

for(k in last:totalpages){
  print(k)
  if(k == 1) {
    url <- paste0("https://www.tripadvisor.co.uk/Restaurant_Review-g187497-d782944-Reviews-Cerveceria_Catalana-Barcelona_Catalonia.html")
  } else {           
    url <- paste0("https://www.tripadvisor.co.uk/Restaurant_Review-g187497-d782944-Reviews-or",k-1,"0-Cerveceria_Catalana-Barcelona_Catalonia.html")
  }
  
  TripAdvisorCerveceria<- rbind(TripAdvisorCerveceria, getPageInfo(url,usersurl,k))
}

write.csv(TripAdvisorCerveceria, file="./Data/TripAdvisorCerveceriaEng.csv", row.names = FALSE)
save(TripAdvisorCerveceria, file="./Data/TripAdvisorCerveceriaEng.Rdata")



# RESTAURANTE CIUDAD CONDAL
TripAdvisorCondal <- data.frame()
totalpages <- 389
last <- 1 
usersurl <- paste0("https://www.tripadvisor.co.uk/ShowUserReviews-g187497-d1059712-rXXX-Ciudad_Condal_Restaurant-Barcelona_Catalonia.html#REVIEWS")

for(k in last:totalpages){
  print(k)
  if(k == 1) {
    url <- paste0("https://www.tripadvisor.co.uk/Restaurant_Review-g187497-d1059712-Reviews-Ciudad_Condal_Restaurant-Barcelona_Catalonia.html")
  } else {           
    url <- paste0("https://www.tripadvisor.co.uk/Restaurant_Review-g187497-d1059712-Reviews-or",k-1,"0-Ciudad_Condal_Restaurant-Barcelona_Catalonia.html")
  }
  
  TripAdvisorCondal<- rbind(TripAdvisorCondal, getPageInfo(url,usersurl,k))
}

write.csv(TripAdvisorCondal, file="./Data/TripAdvisorCondalEng.csv", row.names = FALSE)
save(TripAdvisorCondal, file="./Data/TripAdvisorCondalEng.Rdata")



# RESTAURANTE TERESA CARLES
TripAdvisorTeresa <- data.frame()
totalpages <- 177
last <- 1 
usersurl <- paste0("https://www.tripadvisor.co.uk/ShowUserReviews-g187497-d2075668-rXXX-Teresa_Carles-Barcelona_Catalonia.html#REVIEWS")

for(k in last:totalpages){
  print(k)
  if(k == 1) {
    url <- paste0("https://www.tripadvisor.co.uk/Restaurant_Review-g187497-d2075668-Reviews-Teresa_Carles-Barcelona_Catalonia.html")
  } else {           
    url <- paste0("https://www.tripadvisor.co.uk/Restaurant_Review-g187497-d2075668-Reviews-or",k-1,"0-Teresa_Carles-Barcelona_Catalonia.html")
  }
  
  TripAdvisorTeresa<- rbind(TripAdvisorTeresa, getPageInfo(url,usersurl,k))
}

write.csv(TripAdvisorTeresa, file="./Data/TripAdvisorTeresaEng.csv", row.names = FALSE)
save(TripAdvisorTeresa, file="./Data/TripAdvisorTeresaEng.Rdata")



# RESTAURANTE TAPEO
TripAdvisorTapeo <- data.frame()
totalpages <- 161
last <- 1 
usersurl <- paste0("https://www.tripadvisor.co.uk/ShowUserReviews-g187497-d1863141-rXXX-Tapeo-Barcelona_Catalonia.html#REVIEWS")

for(k in last:totalpages){
  print(k)
  if(k == 1) {
    url <- paste0("https://www.tripadvisor.co.uk/Restaurant_Review-g187497-d1863141-Reviews-Tapeo-Barcelona_Catalonia.html")
  } else {           
    url <- paste0("https://www.tripadvisor.co.uk/Restaurant_Review-g187497-d1863141-Reviews-or",k-1,"0-Tapeo-Barcelona_Catalonia.html")
  }
  
  TripAdvisorTapeo<- rbind(TripAdvisorTapeo, getPageInfo(url,usersurl,k))
}

write.csv(TripAdvisorTapeo, file="./Data/TripAdvisorTapeoEng.csv", row.names = FALSE)
save(TripAdvisorTapeo, file="./Data/TripAdvisorTapeoEng.Rdata")
