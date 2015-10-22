#Preparations for LDA topic modelling

#Convert to Matrix
reviewMatrix <- as.matrix(reviewDtmCompact)

wordFrequency <- sort(colSums(reviewMatrix), decreasing=TRUE)
wordTerms <- names(wordFrequency)

reviewPlotData <- data.frame(word=wordTerms, freq=wordFrequency)

#Try to plot graph of word vs freq
ggplot(reviewPlotData, aes(x = word, y = freq)) +
    geom_tile(colour = "white") +
    ylab("") +
    theme(panel.background = element_blank()) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank());