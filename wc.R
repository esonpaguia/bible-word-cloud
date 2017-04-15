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
bibleCorpus <- tm_map(bibleCorpus, toSpace, "/")
bibleCorpus <- tm_map(bibleCorpus, toSpace, "@")
bibleCorpus <- tm_map(bibleCorpus, toSpace, "\\|")

# Convert the text to lower case
bibleCorpus <- tm_map(bibleCorpus, content_transformer(tolower))

# Remove numbers
bibleCorpus <- tm_map(bibleCorpus, removeNumbers)

# Remove english common stopwords
bibleCorpus <- tm_map(bibleCorpus, removeWords, stopwords("english"))

# Remove your own stop word
# specify your stopwords as a character vector
# bibleCorpus <- tm_map(bibleCorpus, removeWords, c("said", "will")) 

# Remove punctuations
bibleCorpus <- tm_map(bibleCorpus, removePunctuation)

# Eliminate extra white spaces
bibleCorpus <- tm_map(bibleCorpus, stripWhitespace)

# Text stemming
bibleCorpus <- tm_map(bibleCorpus, stemDocument)

# Generate matrix
dtm <- TermDocumentMatrix(bibleCorpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# Create word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))