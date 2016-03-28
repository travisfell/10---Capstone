# CapstoneExplorationModeling.R
# This script contains code to prep the environment, load in 
# objects from week one, and conduct the exporation and modeling
# required for week two. 

# I. Prep the Environment
# first, let's set the working directory
setwd("C:/Users/fellt/Desktop/Data Science/Coursera Data Science Specialization/10 - Capstone")

# load up some libraries we'll probably need
library(tm)
library(stringi)
library(Matrix)
library(data.table)
library(quanteda)
# load libraries to assist with multi threaded processing
  library(parallel, quietly=T)
  library(doParallel, quietly=T)
  # turn on parallel processing to help improve performance
  cluster <- makeCluster(detectCores() - 1)
  registerDoParallel(cluster)
library(ggplot2)
set.seed(1003)

# load in n-grams objects if needed
# load("unigrams.Rda")
# load("bigrams.Rda")
# load("trigrams.Rda")

# If needed, find and strip sparse terms here
#sparsethreshold <- round(ndoc(unigrams) * (1 - 0.999))
#unigramTrim <- trim(unigrams, minDoc = sparsethreshold)
#nfeature(unigramTrim)

  
# II. Find and sort terms matrices by frequency
unifreq <- colSums(unigrams) # may also try coercion to data frame or matrix per the help text
unifreq <- unifreq[order(unifreq, decreasing = TRUE)]
bifreq <- colSums(bigrams)
bifreq <- bifreq[order(bifreq, decreasing = TRUE)] 
trifreq <- colSums(trigrams)
trifreq <- trifreq[order(trifreq, decreasing = TRUE)]

# remember, bigrams and trigrams hae "_" separator. To remove '_' characters from bigrams and trigrams for matching: 
# names(bifreq) <- gsub("_", " ", names(bifreq))


# III. Experiment with the model

# create Maximum Likelihood Estimate matrix for known strings in corpus:
# text 1: "etc. noel had in mind a girl" 
# text 2: "just purchase the product and buy any or all of the flavors"
# text 3: "this time with the left over egg whites i made single serve mini pavlova's"

# loop through string to find all bigrams therein and calculate probabilities
# calculate P(word 2 follows word 1): find count of all word1-word2 bigrams/divide by count of all bigrams
allFeatures <- nfeature(bigrams)
w1 <- "the"
w2 <- "guy"
trainbigram <- paste(w1, w2, sep = "_")
gramfreq <- length(grep(trainbigram, names(bifreq)))
prob <- gramfreq/allFeatures
# apply log tranformation to prevent numerical underflow
# add all log probs then use exp fxn to come back to linear space
prob <- log10(prob)
# START HERE
# include "backoff" or missing terms code here to assign probability
# include trigram match code
# create a function, use loop or apply to parse a string word by word, then return total probability
# test against quiz 2 answers

# create training, development and test sets using 80/10/10 split



