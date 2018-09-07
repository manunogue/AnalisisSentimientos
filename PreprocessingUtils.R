#
# Description: Script which contains preprocessing utilities:
#                 - Feature Selection
#                 - Stemming and Stop Words
#                 - Tokenizing: Unigrams and Bigrams
#                 - Document Term Matrix
#
# Author: Manuel Noguera 

# Libraries
library(tm)
library(SnowballC)
library(data.table)
require(textreuse)
library(ngram)
library(RWeka)

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

# Given an opinions dataset calculates the global word frequency  
calculateWordFrequency <- function(documentVector, sparsity= .999, unigrams= TRUE) {
  
  # Construct the document Corpus
  documents <- VCorpus(VectorSource(documentVector))
  documents <- tm_map(documents, content_transformer(tolower))
  documents <- tm_map(documents, removePunctuation)
  documents <- tm_map(documents, removeWords, stopwords("english")) 

  # Construct term frequency matrix
  if (unigrams){
    tempDTF <- DocumentTermMatrix(documents, control = list(stopwords = stopwords("SMART"), stemming=TRUE, removePunctuation = TRUE, removeNumbers = TRUE))
  } 
  else { 
    tempDTF <- DocumentTermMatrix(documents, control = list(tokenize = BigramTokenizer, removePunctuation = TRUE, removeNumbers = TRUE,
                                                            stopwords = stopwords("SMART"), stemming = TRUE))    
  }
  DTF <- as.matrix(removeSparseTerms(tempDTF, sparsity))
  
  # Construct word frequency df
  wordFreq <- colSums(DTF)
  wordFreq <- data.frame(word = names(wordFreq), freq = wordFreq)
  rownames(wordFreq) <- NULL
  
  return(wordFreq)
}

# Given an opinions dataset calculates the global inverse word frequency  
calculateInverseWordFrequency <- function(documentVector, sparsity= .999, unigrams= TRUE) {
  
  # Construct the document Corpus
  documents <- VCorpus(VectorSource(documentVector))
  documents <- tm_map(documents, content_transformer(tolower))
  documents <- tm_map(documents, removePunctuation)
  documents <- tm_map(documents, removeWords, stopwords("english")) 
  
  # Construct inverse term frequency matrix
  if (unigrams){
    tempITF <- DocumentTermMatrix(documents, control = list(stopwords = stopwords("SMART"), stemming=TRUE, removePunctuation = TRUE, removeNumbers = TRUE, weighting = function(x) weightTfIdf(x, normalize = FALSE)))
  }
  else {
    tempITF <- DocumentTermMatrix(documents, control = list(tokenize = BigramTokenizer, stopwords = stopwords("SMART"), stemming=TRUE, removePunctuation = TRUE,
                                                             removeNumbers = TRUE, weighting = function(x) weightTfIdf(x, normalize = FALSE)))
  }
  
  # Remove sparse terms
  tempITF <- as.matrix(removeSparseTerms(tempITF, sparsity))
  docTerm <- as.data.frame(tempITF)
  
  #Construct Inverse word frequency
  wordInvFreq <- colMeans(tempITF)
  wordInvFreq <- data.frame(word = names(wordInvFreq), freq = wordInvFreq)
  rownames(wordInvFreq) <- NULL
  
  return(list(InvFreq = wordInvFreq, Terms = docTerm))
}

# Given an opinions dataset calculate term frequency inverse document frequency  
getWordTFIDF <- function(dataSet, unigram) {
  
  alpha <- 150 #smoothing term
  
  # WORD TERM FREQUENCY
  wordFreqPos <- as.data.table( calculateWordFrequency(dataSet$completeOpinion[dataSet$SentimentValue == "positive"], unigrams = unigram))
  wordFreqNeg <- as.data.table( calculateWordFrequency(dataSet$completeOpinion[dataSet$SentimentValue == "negative"], unigrams = unigram))
  
  wordFreqPos <- wordFreqPos[order(freq, decreasing = TRUE),]
  wordFreqNeg <- wordFreqNeg[order(freq, decreasing = TRUE),]
  
  # NSDI
  wordFreq <- merge(wordFreqNeg, wordFreqPos, by = "word", all = T)
  
  wordFreq$freq.x[is.na(wordFreq$freq.x)] <- 0
  wordFreq$freq.y[is.na(wordFreq$freq.y)] <- 0
  wordFreq$difference <- abs(wordFreq$freq.x - wordFreq$freq.y)
  
  wordFreq$ndsi <- abs(wordFreq$freq.x - wordFreq$freq.y)/(wordFreq$freq.x - wordFreq$freq.y + 2*alpha)
  
  # WORD TERM INVERSE FREQUENCY (tfidf)
  tfidfPos <- as.data.table( calculateInverseWordFrequency(dataSet$completeOpinion[dataSet$SentimentValue == "positive"], unigrams = unigram)$InvFreq)
  tfidfNeg <- as.data.table( calculateInverseWordFrequency(dataSet$completeOpinion[dataSet$SentimentValue == "negative"], unigrams = unigram)$InvFreq)
  
  tfidfPos <- tfidfPos[order(freq, decreasing = TRUE),]
  tfidfNeg <- tfidfNeg[order(freq, decreasing = TRUE),]
  
  # NSDI
  tfidf <- merge( tfidfNeg, tfidfPos, by = "word", all = TRUE)
  
  tfidf$freq.x[is.na(tfidf$freq.x)] <- 0
  tfidf$freq.y[is.na(tfidf$freq.y)] <- 0
  tfidf$diff <- abs(tfidf$freq.x - tfidf$freq.y)
  
  tfidf$ndsi <- abs(tfidf$freq.x - tfidf$freq.y)/(tfidf$freq.x + tfidf$freq.y + 2*alpha)
  
  # Merge FREQ and TFIDF
  wordNeg <- merge(tfidfNeg, wordFreqNeg, by="word", all = TRUE)
  wordPos <- merge(tfidfPos, wordFreqPos, by="word", all = TRUE)
  
  setnames(wordNeg, old=c("freq.x", "freq.y"), new=c("tfidf", "freq"))
  setnames(wordPos, old=c("freq.x", "freq.y"), new=c("tfidf", "freq"))
  
  return(list(wordNeg = wordNeg, wordPos = wordPos, tfidf = tfidf, wordFreq = wordFreq))
}

# Extract most relevant words from a dataset using tfidf
# unigrams: indicates wheter using unigrams or bigrams
getWordSelection <- function(dataSet, unigrams) {
  
  wordNeg <- getWordTFIDF(dataSet, unigram=unigrams)$wordNeg
  wordPos <- getWordTFIDF(dataSet, unigram=unigrams)$wordPos
  
  if(!unigrams){ 
    wordNeg <- wordNeg[-deleteSTOPWords(wordNeg),]
    wordPos <- wordPos[-deleteSTOPWords(wordPos),]
  }
  
  # Order and select most 500 popular words
  wordNeg <- wordNeg[order(tfidf, decreasing = TRUE),]
  wordNeg500 <- wordNeg[1:500,]
  wordPos <- wordPos[order(tfidf, decreasing = TRUE),]
  wordPos500 <- wordPos[1:500,]
  
  wordSelection <- merge(wordNeg500, wordPos500, by = "word", all = TRUE)
  setnames(wordSelection, old=c("tfidf.x", "freq.x", "tfidf.y", "freq.y"), new=c("tfidfNeg", "freqNeg", "tfidfPos", "freqPos"))
  
  wordSelection <- wordSelection[order(tfidfNeg, decreasing = TRUE),]
  wordSelectionVector <- wordSelection$word                
  
  wordNegSelect <- wordNeg[!(wordNeg$word %in% wordPos$word),]
  wordPosSelect <- wordPos[!(wordPos$word %in% wordNeg$word),]
  wordCommonPosNeg <- wordNeg[(wordNeg$word %in% wordPos$word),]
  
  return (list (wordSelectionVector = wordSelectionVector, wordCommonPosNeg = wordCommonPosNeg))
}

# Applies feature selection and construct resulting document term matrix 
# unigrams: indicates wheter using unigrams or bigrams
# location: indicates which country is using the script
getDocumentTermMatrix <- function(dataSet, name, unigrams, location) {
  
  wordSelectionVector <- getWordSelection(dataSet, unigrams)$wordSelectionVector
   
  dataSetPosNeg <- dataSet[dataSet$SentimentValue != "neutral",]
  dataSetFeatures <- calculateInverseWordFrequency(dataSetPosNeg$completeOpinion, unigrams=unigrams)$Terms
  dataSetFeatures <- ifelse(dataSetFeatures > 0, 1, 0)
  dataSetFeatures <- dataSetFeatures[, colnames(dataSetFeatures) %in% wordSelectionVector]
  
  # Preparing the final set
  dataSetAndFeatures <- cbind(dataSetPosNeg, dataSetFeatures)
  dataSetAndFeatures$pos <- NULL
  dataSetAndFeatures$neg <- NULL
  
  # Delete Neutral opinions
  dataSetAndFeatures <- dataSetAndFeatures[dataSetAndFeatures$SentimentValue!="neutral",]
  dataSetAndFeatures <- dataSetAndFeatures[dataSetAndFeatures$SentimentCoreNLP!="neutral",]
  
  if(location == "M"){
    path = "./Madrid"  
  }else if(location == "G"){
    path = "./Granada"
  }else{
    path = "./Barcelona"
  }
  
  if(unigrams){
    path = paste(path, "/Data/Features/Unigrams/", sep = "")
  } else {
    path = paste(path, "/Data/Features/Bigrams/", sep = "")
  }
  
  save(dataSetAndFeatures, file= paste(path, name, ".Rdata", sep = ""))
  write.csv(dataSetAndFeatures, file= paste(path, name, ".csv", sep = ""), row.names = FALSE)
  
  # Not matching
  dataSetAndFeatures_NOTMATCHING <- dataSetAndFeatures[!(dataSetAndFeatures$SentimentValue=="positive" & dataSetAndFeatures$SentimentCoreNLP=="negative"),]
  dataSetAndFeatures_NOTMATCHING <- dataSetAndFeatures_NOTMATCHING[!(dataSetAndFeatures_NOTMATCHING$SentimentValue=="negative" & dataSetAndFeatures_NOTMATCHING$SentimentCoreNLP=="positive"),]
  
  save(dataSetAndFeatures_NOTMATCHING, file= paste(path, "NotMatching/", name, ".Rdata", sep = ""))
  write.csv(dataSetAndFeatures_NOTMATCHING, file= paste(path, "NotMatching/", name, ".csv", sep = ""), row.names = FALSE)

}

# Delete STOPWORDS bi-grams
deleteSTOPWords <- function(wordSent){
  numRows <- nrow(wordSent)
  delRows <- c()
  for(i in 1:numRows){
    if(sapply(strsplit(as.character(wordSent$word[i]), " "), length) == 2){
      word1 <- strsplit(as.character(wordSent$word[i]), " ")[[1]][1]
      word2 <- strsplit(as.character(wordSent$word[i]), " ")[[1]][2]
      if(word1 == "" | word2 == ""){
        delRows <- c(delRows, i)
      } else if(sum(grepl(word1, stopwords("SMART"))) > 0  & sum(grepl(word2, stopwords("SMART"))) > 0){
        delRows <- c(delRows, i)
      }
    }
    else{
      delRows <- c(delRows, i)
    }
    
  }
  return(delRows)
}
