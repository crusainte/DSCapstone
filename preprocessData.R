library(tm)
library(SnowballC)
library(stringi)

#Preprocess to remove non ASCII characters

#Create Corpus
reviewCorpus <- Corpus(VectorSource(lowstarreview$text))
#reviewCorpus <- Corpus(VectorSource(sample))

#save(reviewCorpus,file="work_data//originalCorpus.RData")

#Function for removing non ascii characters
removeNonASCII <- function(x) 
{
    iconv(x, "latin1", "ASCII", sub="")
}   

#Clean Corpus
reviewCorpus <- tm_map(reviewCorpus, content_transformer(removeNonASCII))
reviewCorpus <- tm_map(reviewCorpus, content_transformer(tolower))
reviewCorpus <- tm_map(reviewCorpus, removePunctuation)
reviewCorpus <- tm_map(reviewCorpus, removeNumbers)
reviewCorpus <- tm_map(reviewCorpus, removeWords, stopwords("english"))
reviewCorpus <- tm_map(reviewCorpus, stripWhitespace)

corpusDictionary <- reviewCorpus
save(corpusDictionary,file="work_data//corpusDictionary.RData")

reviewCorpus <- tm_map(reviewCorpus, stemDocument)

#Create DTM
reviewDtm <- DocumentTermMatrix(reviewCorpus, control = list(minWordLength = 3))

save(reviewDtm,file="work_data//reviewDtm.RData")

#completed <- stemCompletion(colnames(reviewDtm), corpusDictionary)
#completed <- Corpus(VectorSource(completed))
#completed <- DocumentTermMatrix(completed, control = list(minWordLength = 3))

#save(completed,file="work_data//completed.RData")

#DTM Exploration

#Look at top frequency terms
findFreqTerms(reviewDtm, lowfreq=35000)

#Remove Sparse Terms
reviewDtmCompact <- removeSparseTerms(reviewDtm, sparse=0.88)
save(reviewDtmCompact,file="work_data//reviewDtmCompact.RData")