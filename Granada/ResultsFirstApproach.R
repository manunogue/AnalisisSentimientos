library(data.table)

#BODEGAS
# Read the data
MyData <- read.csv(file="./Data/Core/CoreNLP_Bodegas.csv", header=TRUE, sep=",")

# Analysing SentimentValue vs. SentimentCoreNLP (TABLE)
table(MyData$SentimentCoreNLP, MyData$SentimentValue)
100*(table(MyData$SentimentCoreNLP, MyData$SentimentValue)/(nrow(MyData)))

# Read the data
MyData <- read.csv(file="./Data/Tidy/Tidy_Bodegas.csv", header=TRUE, sep=",")

# Analysing SentimentValue vs. SentimentTidy (TABLE)
table(MyData$TidySentiment, MyData$SentimentValue)
100*(table(MyData$TidySentiment, MyData$SentimentValue)/(nrow(MyData)))


#CARMELA
# Read the data
MyData <- read.csv(file="./Data/Core/CoreNLP_Carmela.csv", header=TRUE, sep=",")

# Analysing SentimentValue vs. SentimentCoreNLP (TABLE)
table(MyData$SentimentCoreNLP, MyData$SentimentValue)
100*(table(MyData$SentimentCoreNLP, MyData$SentimentValue)/(nrow(MyData)))

# Read the data
MyData <- read.csv(file="./Data/Tidy/Tidy_Carmela.csv", header=TRUE, sep=",")

# Analysing SentimentValue vs. SentimentTidy (TABLE)
table(MyData$TidySentiment, MyData$SentimentValue)
100*(table(MyData$TidySentiment, MyData$SentimentValue)/(nrow(MyData)))


#DIAMANTES
# Read the data
MyData <- read.csv(file="./Data/Core/CoreNLP_Diamantes.csv", header=TRUE, sep=",")

# Analysing SentimentValue vs. SentimentCoreNLP (TABLE)
table(MyData$SentimentCoreNLP, MyData$SentimentValue)
100*(table(MyData$SentimentCoreNLP, MyData$SentimentValue)/(nrow(MyData)))

# Read the data
MyData <- read.csv(file="./Data/Tidy/Tidy_Diamantes.csv", header=TRUE, sep=",")

# Analysing SentimentValue vs. SentimentTidy (TABLE)
table(MyData$TidySentiment, MyData$SentimentValue)
100*(table(MyData$TidySentiment, MyData$SentimentValue)/(nrow(MyData)))


#ESTRELLAS
# Read the data
MyData <- read.csv(file="./Data/Core/CoreNLP_Estrellas.csv", header=TRUE, sep=",")

# Analysing SentimentValue vs. SentimentCoreNLP (TABLE)
table(MyData$SentimentCoreNLP, MyData$SentimentValue)
100*(table(MyData$SentimentCoreNLP, MyData$SentimentValue)/(nrow(MyData)))

# Read the data
MyData <- read.csv(file="./Data/Tidy/Tidy_Estrellas.csv", header=TRUE, sep=",")

# Analysing SentimentValue vs. SentimentTidy (TABLE)
table(MyData$TidySentiment, MyData$SentimentValue)
100*(table(MyData$TidySentiment, MyData$SentimentValue)/(nrow(MyData)))


#JARDINES
# Read the data
MyData <- read.csv(file="./Data/Core/CoreNLP_Jardines.csv", header=TRUE, sep=",")

# Analysing SentimentValue vs. SentimentCoreNLP (TABLE)
table(MyData$SentimentCoreNLP, MyData$SentimentValue)
100*(table(MyData$SentimentCoreNLP, MyData$SentimentValue)/(nrow(MyData)))

# Read the data
MyData <- read.csv(file="./Data/Tidy/Tidy_Jardines.csv", header=TRUE, sep=",")

# Analysing SentimentValue vs. SentimentTidy (TABLE)
table(MyData$TidySentiment, MyData$SentimentValue)
100*(table(MyData$TidySentiment, MyData$SentimentValue)/(nrow(MyData)))