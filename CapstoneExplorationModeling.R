# CapstoneExplorationModeling.R
# This script contains code to prep the environment, load in 
# objects from week one, and conduct the exporation and modeling
# required for week two. 


# first, let's set the working directory
setwd("C:/Users/fellt/Desktop/Data Science/Coursera Data Science Specialization/10 - Capstone")

# load up some libraries we'll probably need
library(tm)
library(SnowballC)
library(tau)
# load libraries to assist with multi threaded processing
  library(parallel, quietly=T)
  library(doParallel, quietly=T)
  # turn on parallel processing to help improve performance
  cluster <- makeCluster(detectCores() - 1)
  registerDoParallel(cluster)
library(NLP)
library(RTextTools)
library(ggplot2)
library(data.table)
library(Rtools)

# load in usVcorpTDM if needed
#load("usVcorpTDM.Rda")

#exploratory analysis
# how often do words appear?
# how often to groups of words appear?
# expectations: Standard English sentence construction: noun-verb, lots of political topics

# inspect Term Document Matrix 
inspect(usVcorpTDM)
terms <- Terms(usVcorpTDM)
length(terms)
unique(Encoding(terms)) # ensure no UTF-8 or non-ASCII text

#what are most and least frequent terms?
findFreqTerms(usVcorpTDM, 1000)
dim(usVcorpTDM)
usVcorpTDM.common <- removeSparseTerms(usVcorpTDM, .999)
dim(usVcorpTDM.common)
freq <- rowSums(as.matrix((usVcorpTDM.common)))
ord <- order(freq)
freq[head(ord)]
freq[tail(ord, n = 30)]
wordFreq <- freq[tail(ord, n = 30)]
commonTerms <- Terms(usVcorpTDM.common)
length(commonTerms)
commonTerms #see terms
#transform to data for plotting
wordFreq <- as.data.frame(wordFreq)
wordFreq <- setDT(wordFreq, keep.rownames = TRUE)
wordFreq <- wordFreq[order(wordFreq, decreasing = TRUE),]


# plot word frequencies

# plot 30 most frequent words
g <- ggplot(wordFreq, 
            aes(reorder(rn, wordFreq), wordFreq))
g <- g + geom_bar(stat = "identity")
g <- g + coord_flip()
g <- g + ggtitle("Word Frequency")
g <- g + ylab("Frequency")
g <- g + xlab ("Words")
#g <- g + theme(axis.text.x = element_text(angle = 45))
g

# find n-grams (2 and 3 word) and plot

# OPTION 1: Bigrams using NLP pkg, see http://tm.r-forge.r-project.org/faq.html#Bigrams 
BigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

usVcorpTDM.bigrams <- TermDocumentMatrix(usVcorp, control = list(tokenize = BigramTokenizer))
dim(usVcorpTDM.bigrams)
usVcorpTDM.bigrams.common <- removeSparseTerms(usVcorpTDM.bigrams, .9999)
dim(usVcorpTDM.bigrams.common)
Terms(usVcorpTDM.bigrams.common)
freq.bigram <- rowSums(as.matrix((usVcorpTDM.bigrams.common)))
ord <- order(freq.bigram)
freq.bigram[head(ord)]
freq.bigram[tail(ord, n = 30)]
bigramFreq <- freq.bigram[tail(ord, n = 30)]

#transform data for plotting
bigramFreq <- as.data.frame(bigramFreq)
bigramFreq <- setDT(bigramFreq, keep.rownames = TRUE)
bigramFreq <- bigramFreq[order(bigramFreq, decreasing = TRUE),]

# plot 20 most frequent bigrams
b <- ggplot(bigramFreq, 
            aes(reorder(rn, bigramFreq), bigramFreq))
b <- b + geom_bar(stat = "identity")
b <- b + coord_flip()
b <- b + ggtitle("Bigram Frequency")
b <- b + ylab("Frequency")
b <- b + xlab ("Bigrams")
#b <- b + theme(axis.text.x = element_text(angle = 45))
b

# OPTION 2: Bigrams using RTextTools
bigram <- create_matrix(usVcorp, ngramLength = 2)
bigram <- create_matrix(as.character(usAllSmall$V1), ngramLength = 2)

usVcorpDTM <- DocumentTermMatrix(usVcorp)
dim(usVcorpDTM)
Terms(usVcorpDTM)
usVcorpDTM.common <- removeSparseTerms(usVcorpDTM, .999)
dim(usVcorpDTM.common)
Terms(usVcorpDTM.common)
class(usVcorpDTM.common)


# OPTION 3: Bigrams using tau
tokenize_bigrams <- function(x, n=2) return(rownames(as.data.frame(unclass(textcnt(x,method="string",n=n)))))
bigram.matrix <- DocumentTermMatrix(usVcorp,control=list(tokenize=tokenize_bigrams))


# trigrams
TrigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)

usVcorpTDM.trigrams <- TermDocumentMatrix(usVcorp, control = list(tokenize = TrigramTokenizer))
dim(usVcorpTDM.trigrams)
usVcorpTDM.trigrams.common <- removeSparseTerms(usVcorpTDM.trigrams, .9999)
dim(usVcorpTDM.trigrams.common)
freq.trigram <- rowSums(as.matrix((usVcorpTDM.trigrams.common)))
ord <- order(freq.trigram)
freq.trigram[head(ord)]
freq.trigram[tail(ord, n = 30)]
trigramFreq <- freq.trigram[tail(ord, n = 30)]


#transform data
trigramFreq <- as.data.frame(trigramFreq)
trigramFreq <- setDT(trigramFreq, keep.rownames = TRUE)
trigramFreq <- trigramFreq[order(trigramFreq, decreasing = TRUE),]
#trigramFreq <- transform(trigramFreq, rn = reorder(rn, order(trigramFreq, decreasing = TRUE)))

# plot 30 most frequent trigrams
t <- ggplot(trigramFreq, 
    aes(reorder(rn, trigramFreq), trigramFreq))
t <- t + geom_bar(stat = "identity")
t <- t + coord_flip()
t <- t + ggtitle("Trigram Frequency")
t <- t + ylab("Frequency")
t <- t + xlab ("Trigrams")
#t <- t + theme(axis.text.x = element_text(angle = 45))
t


# for non-matching user entered words, try this link: https://github.com/first20hours/google-10000-english 