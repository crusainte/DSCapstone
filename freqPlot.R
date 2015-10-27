#Preparations for LDA topic modelling
library(tm)
library(reshape2)
library(ggplot2)

#Convert to Matrix
reviewMatrix <- as.matrix(reviewDtmCompact)

#reviewMelt <- melt(reviewMatrix, value.name ="count")

wordFrequency <- sort(colSums(reviewMatrix), decreasing=TRUE)
wordTerms <- names(wordFrequency)

reviewPlotData <- data.frame(word=wordTerms, freq=unname(wordFrequency))

#Try to plot graph of word vs freq
ggplot(data = reviewPlotData[1:10,], aes(x = reorder(word, -freq), y = freq)) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    geom_bar(stat="identity") +
    ggtitle(" ") +
    xlab("Word") +
    ylab("Freq")