library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("rstudioapi")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load datasets
filePath <- paste(getwd(), "/data", sep = "")
files <- as.character((list.files(path = filePath)))
bibleData <- unname(sapply(paste(filePath,.Platform$file.sep,files,sep=""), readLines))

bibleCorpus <- Corpus(VectorSource(bibleData))
#inspect(bibleCorpus)

# Remove special characters
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
bibleCorpus1 <- tm_map(bibleCorpus, toSpace, "/")
bibleCorpus2 <- tm_map(bibleCorpus1, toSpace, "@")
bibleCorpus3 <- tm_map(bibleCorpus2, toSpace, "\\|")

# Convert the text to lower case
bibleCorpus4 <- tm_map(bibleCorpus3, content_transformer(tolower))

# Remove numbers
bibleCorpus5 <- tm_map(bibleCorpus4, removeNumbers)

# Remove english common stopwords
bibleCorpus6 <- tm_map(bibleCorpus5, removeWords, stopwords("english"))

# Remove your own stop word
# specify your stopwords as a character vector
bibleCorpus7 <- tm_map(bibleCorpus6, removeWords, c("said", "will")) 

# Remove punctuations
bibleCorpus8 <- tm_map(bibleCorpus7, removePunctuation)

# Eliminate extra white spaces
bibleCorpus9 <- tm_map(bibleCorpus8, stripWhitespace)

# Text stemming
bibleCorpus10 <- tm_map(bibleCorpus9, stemDocument)

# Generate matrix
dtm <- TermDocumentMatrix(bibleCorpus10)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# Create word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))