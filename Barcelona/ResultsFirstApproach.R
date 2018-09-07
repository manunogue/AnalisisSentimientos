
library(data.table)

#CERVECERIA
# Read the data
MyData <- read.csv(file="./Data/Core/CoreNLP_Cerveceria.csv", header=TRUE, sep=",")

# Analysing SentimentValue vs. SentimentCoreNLP (TABLE)
table(MyData$SentimentCoreNLP, MyData$SentimentValue)
100*(table(MyData$SentimentCoreNLP, MyData$SentimentValue)/(nrow(MyData)))

# Read the data
MyData <- read.csv(file="./Data/Tidy/Tidy_Cerveceria.csv", header=TRUE, sep=",")

# Analysing SentimentValue vs. SentimentTidy (TABLE)
table(MyData$TidySentiment, MyData$SentimentValue)
100*(table(MyData$TidySentiment, MyData$SentimentValue)/(nrow(MyData)))


#CONDAL
# Read the data
MyData <- read.csv(file="./Data/Core/CoreNLP_Condal.csv", header=TRUE, sep=",")

# Analysing SentimentValue vs. SentimentCoreNLP (TABLE)
table(MyData$SentimentCoreNLP, MyData$SentimentValue)
100*(table(MyData$SentimentCoreNLP, MyData$SentimentValue)/(nrow(MyData)))

# Read the data
MyData <- read.csv(file="./Data/Tidy/Tidy_Condal.csv", header=TRUE, sep=",")

# Analysing SentimentValue vs. SentimentTidy (TABLE)
table(MyData$TidySentiment, MyData$SentimentValue)
100*(table(MyData$TidySentiment, MyData$SentimentValue)/(nrow(MyData)))


#HRBARCELONA
# Read the data
MyData <- read.csv(file="./Data/Core/CoreNLP_HRB.csv", header=TRUE, sep=",")

# Analysing SentimentValue vs. SentimentCoreNLP (TABLE)
table(MyData$SentimentCoreNLP, MyData$SentimentValue)
100*(table(MyData$SentimentCoreNLP, MyData$SentimentValue)/(nrow(MyData)))

# Read the data
MyData <- read.csv(file="./Data/Tidy/Tidy_HRB.csv", header=TRUE, sep=",")

# Analysing SentimentValue vs. SentimentTidy (TABLE)
table(MyData$TidySentiment, MyData$SentimentValue)
100*(table(MyData$TidySentiment, MyData$SentimentValue)/(nrow(MyData)))


#TAPEO
# Read the data
MyData <- read.csv(file="./Data/Core/CoreNLP_Tapeo.csv", header=TRUE, sep=",")

# Analysing SentimentValue vs. SentimentCoreNLP (TABLE)
table(MyData$SentimentCoreNLP, MyData$SentimentValue)
100*(table(MyData$SentimentCoreNLP, MyData$SentimentValue)/(nrow(MyData)))

# Read the data
MyData <- read.csv(file="./Data/Tidy/Tidy_Tapeo.csv", header=TRUE, sep=",")

# Analysing SentimentValue vs. SentimentTidy (TABLE)
table(MyData$TidySentiment, MyData$SentimentValue)
100*(table(MyData$TidySentiment, MyData$SentimentValue)/(nrow(MyData)))


#TERESA
# Read the data
MyData <- read.csv(file="./Data/Core/CoreNLP_Teresa.csv", header=TRUE, sep=",")

# Analysing SentimentValue vs. SentimentCoreNLP (TABLE)
table(MyData$SentimentCoreNLP, MyData$SentimentValue)
100*(table(MyData$SentimentCoreNLP, MyData$SentimentValue)/(nrow(MyData)))

# Read the data
MyData <- read.csv(file="./Data/Tidy/Tidy_Teresa.csv", header=TRUE, sep=",")

# Analysing SentimentValue vs. SentimentTidy (TABLE)
table(MyData$TidySentiment, MyData$SentimentValue)
100*(table(MyData$TidySentiment, MyData$SentimentValue)/(nrow(MyData)))
