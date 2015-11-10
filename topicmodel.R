library(tm)
library(SnowballC)
library(topicmodels)
library(Rmpfr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(LDAvis)
library(stringi)
library(stringr)

#Function to calculate harmonic mean
harmonicMean <- function(logLikelihoods, precision = 2000L) {
    llMed <- median(logLikelihoods)
    as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                         prec = precision) + llMed))))
}
#Function to transform LDA model into JSON for display
topicmodels_json_ldavis <- function(fitted, corpus, doc_term){
    # Required packages
    library(topicmodels)
    library(dplyr)
    library(stringi)
    library(tm)
    library(LDAvis)
    
    # Find required quantities
    phi <- posterior(fitted)$terms %>% as.matrix
    theta <- posterior(fitted)$topics %>% as.matrix
    vocab <- colnames(phi)
    doc_length <- vector()
    for (i in 1:length(corpus)) {
        temp <- paste(corpus[[i]]$content, collapse = ' ')
        doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
    }
    temp_frequency <- inspect(doc_term)
    freq_matrix <- data.frame(ST = colnames(temp_frequency),
                              Freq = colSums(temp_frequency))
    rm(temp_frequency)
    
    # Convert to json
    json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                   vocab = vocab,
                                   doc.length = doc_length,
                                   term.frequency = freq_matrix$Freq)
    
    return(json_lda)
}

# Due to the vast amount of reviews, I will only run this for 100 iterations
topicNum <- seq(5, 20, 1)
burnin <- 100
iter <- 100
keep <- 50
system.time(fitted_many <- lapply(topicNum, function(k) LDA(reviewDtmCompact, k = k,
                                                                     method = "Gibbs",control = list(burnin = burnin,
                                                                                                     iter = iter, keep = keep) )))
save(fitted_many,file="work_data//fitted_many2.RData")

# extract logliks from each topic
logLiks_many <- lapply(fitted_many, function(L)  L@logLiks[-c(1:(burnin/keep))])

# compute harmonic means
hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))

ldaplot <- ggplot(data.frame(topicNum, hm_many), aes(x=topicNum, y=hm_many)) + geom_path(lwd=1.5) +
    theme(text = element_text(family= NULL),
          axis.title.y=element_text(vjust=1, size=16),
          axis.title.x=element_text(vjust=-.5, size=16),
          axis.text=element_text(size=16),
          plot.title=element_text(size=20)) +
    xlab('Number of Topics') +
    ylab('Harmonic Mean') +
    ggtitle(expression(atop("LDA Analysis of Negative Restaurant Reviews")))

# Obtain the 20 topic model for usage
model<-fitted_many[[16]]
save(model,file="work_data//model.RData")

# Preliminary look at review terms
review.terms <- as.data.frame(terms(model, 30), stringsAsFactors = FALSE)
review.terms[1:5]

topicTerms <- gather(review.terms, ReviewTopic)
# Rank the topics
topicTerms <- cbind(topicTerms, Rank = rep(1:30))
# Filter the top 3 terms for each topic
topTerms <- filter(topicTerms, Rank < 4)
topTerms <- mutate(topTerms, ReviewTopic = word(ReviewTopic, 2))
topTerms$ReviewTopic <- as.numeric(topTerms$ReviewTopic)
topicLabel <- data.frame()
# Combine the top 3 terms in each topic as label
for (i in 1:20){
    z <- filter(topTerms, ReviewTopic == i)
    l <- as.data.frame(paste(z[1,2], z[2,2], z[3,2], sep = " " ), stringsAsFactors = FALSE)
    topicLabel <- rbind(topicLabel, l)
    
}
colnames(topicLabel) <- c("Label")

# Convert reviewDtmCompact back to corpus for LDAvis json creation
dtm2list <- apply(reviewDtmCompact, 1, function(x) {
    paste(rep(names(x), x), collapse=" ")
})
reviewCorpus <- VCorpus(VectorSource(dtm2list))

# Generate JSON to be passed into serVis to display the LDA visualization
ldavis_json<-topicmodels_json_ldavis(model,reviewCorpus,reviewDtmCompact)

serVis(ldavis_json)

