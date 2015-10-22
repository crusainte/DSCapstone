library(wordcloud);
m = as.matrix(reviewDtm);
v = sort(colSums(m), decreasing=TRUE);
myNames = names(v);
d = data.frame(word=myNames, freq=v);
wordcloud(d$word, colors=c(3,4), random.color=FALSE, d$freq, min.freq=35000);
rm(m)
rm(v)
rm(myNames)
rm(d)