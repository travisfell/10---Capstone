# CapstoneExplorationModeling.R
# This script contains code to prep the environment, load in 
# objects from week one, and conduct the exporation and modeling
# required for week two. 


# first, let's set the working directory
setwd("C:/Users/fellt/Desktop/Data Science/Coursera Data Science Specialization/10 - Capstone")

# load up some libraries we'll probably need
library(tm)
# load libraries to assist with multi threaded processing
library(parallel, quietly=T)
library(doParallel, quietly=T)
# turn on parallel processing to help improve performance
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)
library(NLP)
library(ggplot2)
library(data.table)

# load in Vcorpus and TDM if necessary

# save the usVcorp corpus 
#save(usVcorp, file = "usVcorp.Rda")
# reload the usVcorp corpus
#load("usVcorp.Rda")

# save the usVcorpTDM TDM 
#save(usVcorpTDM, file = "usVcorpTDM.Rda")
# reload the usVcorpTDM TDM
#load("usVcorpTDM.Rda")


#exploratory analysis
# how often do words appear?
# how often to groups of words appear?
# expectations: Standard English sentence construction: noun-verb, lots of political topics
# 

#inspect(usVcorpTDM)

#what are most and least frequent terms?
findFreqTerms(usVcorpTDM, 1000)
dim(usVcorpTDM)
usVcorpTDM.common <- removeSparseTerms(usVcorpTDM, .999)
dim(usVcorpTDM.common)
freq <- rowSums(as.matrix((usVcorpTDM.common)))
ord <- order(freq)
freq[head(ord)]
wordFreq <- freq[tail(ord, n = 20)]

#transform to data frame
wordFreq <- as.data.frame(wordFreq)
wordFreq <- setDT(wordFreq, keep.rownames = TRUE)

# save the usVcorpTDM.common TDM 
#save(usVcorpTDM.common, file = "usVcorpTDM.common.Rda")
# reload the usVcorpTDM TDM
#load("usVcorpTDM.common.Rda")

# plot word frequencies

# plot 20 most frequent words
g <- ggplot(wordFreq, aes(rn, wordFreq))
g <- g + geom_bar(stat = "identity")
g <- g + ggtitle("Word Frequency")
g <- g + ylab("Frequency")
g <- g + xlab ("Words")
g <- g + theme(axis.text.x = element_text(angle = 45))
g

# find n-grams (2 and 3 word) and plot
# see http://tm.r-forge.r-project.org/faq.html#Bigrams 

BigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

usVcorpTDM.bigrams <- TermDocumentMatrix(usVcorp, control = list(tokenize = BigramTokenizer))
#dim(usVcorpTDM.bigrams)
usVcorpTDM.bigrams.common <- removeSparseTerms(usVcorpTDM.bigrams, .999)
#dim(usVcorpTDM.bigrams.common)
freq.bigram <- rowSums(as.matrix((usVcorpTDM.bigrams.common)))
ord <- order(freq.bigram)
freq.bigram[head(ord)]
bigramFreq <- freq.bigram[tail(ord, n = 20)]

#transform data
bigramFreq <- as.data.frame(bigramFreq)
bigramFreq <- setDT(bigramFreq, keep.rownames = TRUE)

# plot 20 most frequent bigrams
b <- ggplot(bigramFreq, aes(rn, bigramFreq))
b <- b + geom_bar(stat = "identity")
b <- b + ggtitle("Bigram Frequency")
b <- b + ylab("Frequency")
b <- b + xlab ("Bigrams")
b <- b + theme(axis.text.x = element_text(angle = 45))
b

# trigrams
TrigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
#START HERE
usVcorpTDM.trigrams <- TermDocumentMatrix(usVcorp, control = list(tokenize = TrigramTokenizer))
#dim(usVcorpTDM.trigrams)
usVcorpTDM.trigrams.common <- removeSparseTerms(usVcorpTDM.trigrams, .005)
#dim(usVcorpTDM.trigrams.common)
freq.trigram <- rowSums(as.matrix((usVcorpTDM.trigrams)))
ord <- order(freq.trigram)
freq.trigram[head(ord)]
trigramFreq <- freq.trigram[tail(ord, n = 20)]

#transform data
trigramFreq <- as.data.frame(trigramFreq)
trigramFreq <- setDT(trigramFreq, keep.rownames = TRUE)

# plot 20 most frequent bigrams
t <- ggplot(trigramFreq, aes(rn, trigramFreq))
t <- t + geom_bar(stat = "identity")
t <- t + ggtitle("Trigram Frequency")
t <- t + ylab("Frequency")
t <- t + xlab ("Trigrams")
t <- t + theme(axis.text.x = element_text(angle = 45))
t
