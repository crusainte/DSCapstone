library(tm)
library(SnowballC)
library(topicmodels)

#Determining optimum number of topics k using harmonic means

#Function to determine harmonicMean
harmonicMean <- function(logLikelihoods, precision = 2000L) {
    llMed <- median(logLikelihoods)
    as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                         prec = precision) + llMed))))
}

seqk <- seq(5, 30, 1)
burnin <- 1000
iter <- 1000
keep <- 50
system.time(fitted_many <- lapply(seqk, function(k) LDA(reviewDtmCompact, k = k,
                                                                     method = "Gibbs",control = list(burnin = burnin,
                                                                                                     iter = iter, keep = keep) )))



model<-LDA(reviewDtmCompact,5)
save(model,file="work_data//model.RData")

# Convert reviewDtmCompact back to corpus for LDAvis json creation
dtm2list <- apply(reviewDtmCompact, 1, function(x) {
    paste(rep(names(x), x), collapse=" ")
})
myCorp <- VCorpus(VectorSource(dtm2list))

ldavis_json<-topicmodels_json_ldavis(model,myCorp,reviewDtmCompact)

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