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
length(freq)
ord <- order(freq)
freq[head(ord)]
freq[tail(ord)]


# save the usVcorpTDM.common TDM 
#save(usVcorpTDM.common, file = "usVcorpTDM.common.Rda")
# reload the usVcorpTDM TDM
#load("usVcorpTDM.common.Rda")

# plot word frequencies


# find n-grams (2 and 3 word) and plot
# see http://tm.r-forge.r-project.org/faq.html#Bigrams 

BigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

usVcorpTDM.bigrams <- TermDocumentMatrix(usVcorp, control = list(tokenize = BigramTokenizer))
dim(usVcorpTDM.bigrams)
usVcorpTDM.bigrams.common <- removeSparseTerms(usVcorpTDM.bigrams, .999)
dim(usVcorpTDM.bigrams.common)
freq.bigram <- rowSums(as.matrix((usVcorpTDM.bigrams.common)))
length(freq.bigram)
ord <- order(freq.bigram )
freq.bigram[head(ord)]
freq.bigram[tail(ord)]
