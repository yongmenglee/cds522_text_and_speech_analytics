# CREATE R PROJECT
# Get current working directory
getwd()

# Load package
library(tm)


# **************************************************
# LOADING DATA INTO R

# Create Corpus from .txt
docs <- Corpus(DirSource("C:/Users/Darren Lee/Jupyter_Notebook/MS_DSA/CDS522_2021/CDS522_R/data"))

# View Corpus Information
docs
summary(docs)

cat("Welcome to this simple text mining project")
require(tm)

# Inspect a particular document (e.g. the 1st doc)
writeLines(as.character(docs[[1]]))


# **************************************************
# TEXT PRE-PROCESSING

# checkout tm package transformation functions
getTransformations()

# Create toSpace content transformer
toSpace <- content_transformer(
  function(x, pattern) {
    return (gsub(pattern, " ", x))
  }
)

# select a doc
docIndex <- 3

# before transformation
writeLines(as.character(docs[[docIndex]]))

# eliminate hyphen using toSpace content transformation
docs <- tm_map(docs, toSpace, "-")
writeLines(as.character(docs[[docIndex]]))

# apply removePunctuation
docs <- tm_map(docs, removePunctuation)
writeLines(as.character(docs[[docIndex]]))

# convert corpus to lower case
docs <- tm_map(docs, content_transformer(tolower))
writeLines(as.character(docs[[docIndex]]))

# remove digits in corpus
docs <- tm_map(docs, removeNumbers)
writeLines(as.character(docs[[docIndex]]))

# remove stopwords using the standard list in tm
docs <- tm_map(docs, removeWords, stopwords("english"))
writeLines(as.character(docs[[docIndex]]))

# remove whitespace optional to remove extra whitespace
docs <- tm_map(docs, stripWhitespace)
writeLines(as.character(docs[[docIndex]]))


# **************************************************
# TEXT NORMALIZATION
library(SnowballC)

# duplicate object for testing
docs.stem <- docs

# stem the corpus
docs.stem <- tm_map(docs.stem, stemDocument)
writeLines(as.character(docs.stem[[2]]))


# Lemmatization
# load textstem package
library(textstem)

# duplicate object for testing
docs.lemma <- docs

# lemmatize the corpus (require textstem)
docs.lemma <- tm_map(docs.lemma, lemmatize_strings)
writeLines(as.character(docs.lemma[[2]]))


# **************************************************
# TEXT REPRESENTATION
# Document Term Matrix: DTM

# Create Document Term Matrix
dtm <- DocumentTermMatrix(docs.lemma)

# View summary of document term matrix
dtm

# Inspect document term matrix
inspect(dtm)

# Inspect document term matrix by specifying rows and columns
inspect(dtm[1:5, 11:20])


# **************************************************
# TEXT MINING

# get frequency of each word
freq <- colSums(as.matrix(dtm))

# check dimension of frequency
length(freq)
# freq

# Create sort order
ord <- order(freq, decreasing = TRUE)

# Inspect most frequently occurring terms
freq[head(ord)]

# Inspect least frequently occurring terms
freq[tail(ord)]


# Create document term matrix with term reduction
# - Include only words that occur in 2-8 documents. 
# - enforce lower & upper limit to the length of words (4-20 characters)
dtm.tr <- DocumentTermMatrix(
  docs.lemma, control = list(
    wordLengths = c(4,20), bounds = list(global = c(2,8))
  )
)

dtm.tr
dtm

# Find frequent terms
findFreqTerms(dtm.tr, lowfreq = 5)


# Find terms correlation
findAssocs(dtm.tr, "read", 0.5)

trydata <- c(
  "", "word1", "word1 word2", "word1 word2 word3", 
  "word1 word2 word3 word4", "word1 word2 word3 word4 word5"
)

trydtm <- DocumentTermMatrix(VCorpus(VectorSource(trydata)))
trydtm

as.matrix(trydtm)

findAssocs(trydtm, "word1", 0.0)


# **************************************************

freq.tr <- colSums(as.matrix(dtm.tr))
# freq.tr <- freq


# **************************************************
# SIMPLE GRAPHICS

# Plot simple frequency histogram

# Create a data frame (consists of name of the column)
wordfreq <- data.frame(
  term = names(freq.tr), occurences = freq.tr
)

# load ggplot2 package
library(ggplot2)

#invoke ggplot(plot only terms more than 3 times, label x and y-axis using aes)
phisto<-ggplot(subset(wordfreq, freq.tr>3), aes(term, occurences))
#set the height of the bar using stat="bin" or "identity" ("identify" means the height is based on the data value mapped to y-axis)
phisto<-phisto + geom_bar(stat="identity")
#specify that the x-axis text is at 45 degree angle and horizontally justified
phisto<-phisto + theme(axis.text.x=element_text(angle=45, hjust=1))
#display histogram
phisto


# Create Word Cloud

# load wordcloud package
library(wordcloud)

# setting the seed before each plot to ensure consistent look for clouds
set.seed(32)

# limit words by specifying min frequency
wordcloud(names(freq.tr), freq.tr, min.freq = 3, scale = c(3.5, 0.25))

# limit words by specifying min frequency (with color)
wordcloud(names(freq.tr), freq.tr, min.freq = 3, scale = c(3.5, 0.5), colors = brewer.pal(6, "Dark2"))
