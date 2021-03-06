library(tm)
library(SnowballC)
library(stringi)
library(slam)

#Preprocess to remove non ASCII characters

#Create Corpus
reviewCorpus <- Corpus(VectorSource(lowstarreview$text))

#save(reviewCorpus,file="work_data//originalCorpus.RData")

#Function for removing non ascii characters
removeNonASCII <- function(x) 
{
    iconv(x, "latin1", "ASCII", sub="")
}   

#Clean Corpus
reviewCorpus <- tm_map(reviewCorpus, content_transformer(removeNonASCII))
reviewCorpus <- tm_map(reviewCorpus, content_transformer(tolower))
#Create DTM
reviewDtm <- DocumentTermMatrix(reviewCorpus, control = list(stemming = TRUE, stopwords = TRUE,
                                                             removeNumbers = TRUE, removePunctuation = TRUE, minDocFreq=2, minWordLength=3))
save(reviewDtm,file="work_data//reviewDtm.RData")

#Remove Sparse Terms
reviewDtmCompact <- removeSparseTerms(reviewDtm, sparse=0.9)

#Find rows with empty documents
rowTotals <- apply(reviewDtmCompact , 1, sum)

#remove empty documents from the existing corpus and build a new one
reviewDtmCompact   <- reviewDtmCompact[rowTotals> 0, ]
save(reviewDtmCompact,file="work_data//reviewDtmCompact.RData")