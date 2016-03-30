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
#load markovchain and dependencies
  library(expm)
  library(igraph)
  library(matlab)
  library(markovchain)
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

# remember, bigrams and trigrams have "_" separator. To remove '_' characters from bigrams and trigrams for matching: 
# names(bifreq) <- gsub("_", " ", names(bifreq))


# III. Experiment with the model

# A. create Maximum Likelihood Estimate matrix for known strings in corpus:
# text 1: "etc. noel had in mind a girl" 
# text 2: "just purchase the product and buy any or all of the flavors"
# text 3: "this time with the left over egg whites i made single serve mini pavlova's"

# loop through string to find all bigrams therein and calculate probabilities
# calculate P(word 2 follows word 1): find count of all word1-word2 bigrams/divide by count of all word1-wordX bigrams

w1 <- "a" #x
w2 <- "girl" #y
trainbigram <- paste(w1, w2, sep = "_")
gramfreq <- length(grep(trainbigram, names(bifreq)))
gramall <- length(grep(w1, names(bifreq)))
prob <- gramfreq/gramall
# apply log tranformation to prevent numerical underflow
# add all log probs then use exp fxn to come back to linear space
prob <- log10(prob)

# cumulatively store probabilities

# at end of input string, add existing prob to highest prob of bigrams starting w/
# last word of input string

# if not matched, go back to the first bigram and find 2nd most likely match, and rebuild

# keep working through all possible matches to the bigram list, this should cumulatively increase the probability




# B. experimenting with the markovchain package to predict words from quiz 2
# need to find the cumulative probability for each bigram in the given sentence 
# then estimate the prob of the last term

#define states and probabilities: bigrams and their probabilities
bigramstates <- names(bifreq)

# how to calculate the prob of bigram X given every other bigram, AKA transition probability?
# match last word to first word of 

weatherStates = c("sunny", "cloudy", "rain")
byRow = TRUE
weatherMatrix = matrix(data = c(0.70, 0.2,0.1,
                                0.3,0.4, 0.3,
                                0.2,0.45,0.35), byrow = byRow, nrow = 3,
                       
                       dimnames = list(weatherStates, weatherStates))

mcWeather = new("markovchain", states = weatherStates, byrow = byRow,
                transitionMatrix = weatherMatrix, name = "Weather")



# include "backoff" or missing terms code here to assign probability
# include trigram match code
# create a function, use loop or apply to parse a string word by word, then return total probability
# test against quiz 2 answers

# create training, development and test sets using 80/10/10 split



