## PRELIMINARIES

#check working directory
getwd()

#set working directory 


## 1. LOAD PACKAGE

#load tm package 
library(tm) 

library(RWeka)



## 2. LOAD DATA


#read data from csv into dataframe 
# complaintData <- read.csv("C:/r/simpleTextMining/data/complaintFinance/consumer_complaint_200.csv")
complaintData <- read.csv("data/complaintFinance/consumer_complaint_200.csv")

#use head function to inspect first 6 rows of data
head(complaintData)


#read data from dataframe into VCorpus 
#Use xxx$Issue to read data from Issue column
issueData <- VCorpus(VectorSource(complaintData$Issue))

str(issueData)

## 3. VIEW DATA

#view summary of corpus information

summary(issueData)
 
#inspect data for all records
inspect(issueData)

#inspect a particular document (e.g. the 1st doc)
writeLines(as.character(issueData[[1]]))



## 3. TEXT CLEANING 


#remove stopwords using the standard list in tm
issueData <- tm_map(issueData, removeWords, stopwords("english"))

#apply removePunctuation in corpus
issueData <- tm_map(issueData, removePunctuation)

#convert corpus to lower case
issueData <- tm_map(issueData, content_transformer(tolower))

#remove digits in corpus
issueData <- tm_map(issueData, removeNumbers)

#remove whitespace (optional to remove extra whitespace)
issueData <- tm_map(issueData, stripWhitespace)


## 4. NGRAM TOKENIZATION (using RWEKA Package)

#create Trigram Tokenizer
TrigramTokenizer <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3))

#create term document matrix using ngram tokenizer  
tdmIssue <- TermDocumentMatrix(issueData, control = list(tokenize = TrigramTokenizer)) # create tdm from n-grams

#inspect summary of term document matrix
tdmIssue

#check freqent terms by setting the minimum number of occurence 
findFreqTerms(tdmIssue, lowfreq = 5)


#get frequent trigram terms (e.g. 5)
tdmIssue.freqtrigram <- findFreqTerms(tdmIssue,lowfreq = 5)       


tdmIssue.freqtrigram



##8. GRAPH PLOTTING FOR NGRAM 

IssueTrigramfreq <- rowSums(as.matrix(tdmIssue[tdmIssue.freqtrigram,]))
IssueTrigramfreq <- data.frame(word=names(IssueTrigramfreq),frequency=IssueTrigramfreq)

#check the first n items 
head(IssueTrigramfreq)


#create the graph plotting fucntion 

plotthegraph <- function(data,title,num){
  df <- data[order(-data$frequency),][1:num,]
  barplot(df[1:num,]$freq, las = 2, names.arg = df[1:num,]$word,
          col ="darkblue", main = title,
          ylab = "Frequencies",cex.axis =0.8)
}
par(mar=c(10,4,4,2))

#plot the graph for Trigram 
plotthegraph(IssueTrigramfreq,"Trigrams",20)


