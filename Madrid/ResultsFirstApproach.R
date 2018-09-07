library(data.table)

#HARD ROCK HOTEL
# Read the data
MyData <- read.csv(file="./Data/Core/CoreNLP_HRM.csv", header=TRUE, sep=",")

# Analysing SentimentValue vs. SentimentCoreNLP (TABLE)
table(MyData$SentimentCoreNLP, MyData$SentimentValue)
100*(table(MyData$SentimentCoreNLP, MyData$SentimentValue)/(nrow(MyData)))


# Read the data
MyData <- read.csv(file="./Data/Tidy/Tidy_HRM.csv", header=TRUE, sep=",")

# Analysing SentimentValue vs. SentimentTidy (TABLE)
table(MyData$TidySentiment, MyData$SentimentValue)
100*(table(MyData$TidySentiment, MyData$SentimentValue)/(nrow(MyData)))


#BOTIN
# Read the data
MyData <- read.csv(file="./Data/Core/CoreNLP_Botin.csv", header=TRUE, sep=",")

# Analysing SentimentValue vs. SentimentCoreNLP (TABLE)
table(MyData$SentimentCoreNLP, MyData$SentimentValue)
100*(table(MyData$SentimentCoreNLP, MyData$SentimentValue)/(nrow(MyData)))

# Read the data
MyData <- read.csv(file="./Data/Tidy/Tidy_Botin.csv", header=TRUE, sep=",")

# Analysing SentimentValue vs. SentimentTidy (TABLE)
table(MyData$TidySentiment, MyData$SentimentValue)
100*(table(MyData$TidySentiment, MyData$SentimentValue)/(nrow(MyData)))


#EL SUR
# Read the data
MyData <- read.csv(file="./Data/Core/CoreNLP_ElSur.csv", header=TRUE, sep=",")

# Analysing SentimentValue vs. SentimentCoreNLP (TABLE)
table(MyData$SentimentCoreNLP, MyData$SentimentValue)
100*(table(MyData$SentimentCoreNLP, MyData$SentimentValue)/(nrow(MyData)))

# Read the data
MyData <- read.csv(file="./Data/Tidy/Tidy_ElSur.csv", header=TRUE, sep=",")

# Analysing SentimentValue vs. SentimentTidy (TABLE)
table(MyData$TidySentiment, MyData$SentimentValue)
100*(table(MyData$TidySentiment, MyData$SentimentValue)/(nrow(MyData)))


#MERCADO SAN MIGUEL
# Read the data
MyData <- read.csv(file="./Data/Core/CoreNLP_MercadoSM.csv", header=TRUE, sep=",")

# Analysing SentimentValue vs. SentimentCoreNLP (TABLE)
table(MyData$SentimentCoreNLP, MyData$SentimentValue)
100*(table(MyData$SentimentCoreNLP, MyData$SentimentValue)/(nrow(MyData)))

# Read the data
MyData <- read.csv(file="./Data/Tidy/Tidy_MercadoSM.csv", header=TRUE, sep=",")

# Analysing SentimentValue vs. SentimentTidy (TABLE)
table(MyData$TidySentiment, MyData$SentimentValue)
100*(table(MyData$TidySentiment, MyData$SentimentValue)/(nrow(MyData)))


#TEN CON TEN
# Read the data
MyData <- read.csv(file="./Data/Core/CoreNLP_TenConTen.csv", header=TRUE, sep=",")

# Analysing SentimentValue vs. SentimentCoreNLP (TABLE)
table(MyData$SentimentCoreNLP, MyData$SentimentValue)
100*(table(MyData$SentimentCoreNLP, MyData$SentimentValue)/(nrow(MyData)))

# Read the data
MyData <- read.csv(file="./Data/Tidy/Tidy_TenConTen.csv", header=TRUE, sep=",")

# Analysing SentimentValue vs. SentimentTidy (TABLE)
table(MyData$TidySentiment, MyData$SentimentValue)
100*(table(MyData$TidySentiment, MyData$SentimentValue)/(nrow(MyData)))
