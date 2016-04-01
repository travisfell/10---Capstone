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
options(scipen=999) # turn off scientific notation

right = function (string, char){
  substr(string,nchar(string)-(char-1),nchar(string))
}

# load in n-grams objects if needed
 load("unigrams.Rda")
 load("bigrams.Rda")
 load("trigrams.Rda")
 load("fourgrams.Rda")

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
fourfreq <- colSums(fourgrams)
fourfreq <- fourfreq[order(fourfreq, decreasing = TRUE)]

# remember, bigrams and trigrams have "_" separator. To remove '_' characters from bigrams and trigrams for matching: 
# names(bifreq) <- gsub("_", " ", names(bifreq))

#convert frequencies to probabilities
unifreq <- unifreq/length(unifreq)
bifreq <- bifreq/length(bifreq)
trifreq <- trifreq/length(trifreq)
fourfreq <- fourfreq/length(fourfreq)

# III. Experiment with the model

# A. create Maximum Likelihood Estimate matrix

# 1. match input trigram to first three words of fourgram list
# 1a. if match present, return list with frequency 
# 2. match last two terms of input trigram to trigram list
# 2a. if match present, return list with frequency
# 3. match last term of input trigram to bigram list
# 3a. if match present, return list with frequency
# 4. show top 10 unigrams by frequency

# see this discussion: https://www.coursera.org/learn/data-science-project/module/VNKmf/discussions/HmPU3OvyEeWfwAohgaM63Q
# also, read up on interpolation and Kneser - Ney smoothing (account for context)

inputTrigram <- "^of_Adam_Sandler_" # be sure to include trailing underbar
fourfreqmatch <- fourfreq[grep(inputTrigram, names(fourfreq), ignore.case = TRUE)]
space1 <- regexpr(pattern ='_',inputTrigram)[1] #find location of first space
inputBigram <- paste("^", substring(inputTrigram, space1 + 1, nchar(inputTrigram)), sep = "")
trifreqmatch <- trifreq[grep(inputBigram, names(trifreq), ignore.case = TRUE)]
space2 <- regexpr(pattern ='_',inputBigram)[1] 
inputUnigram <- paste("^", substring(inputBigram, space2 + 1, nchar(inputBigram)), sep = "")
bifreqmatch <- bifreq[grep(inputUnigram, names(bifreq), ignore.case = TRUE)]

quiztext <- c("movies", "novels", "pictures", "stories")
matchfreq <- optionmatch(inputTrigram, fourfreqmatch, quiztext)
matchfreq <- c(matchfreq, optionmatch(inputUnigram, bifreqmatch, quiztext))
matchfreq <- c(matchfreq, optionmatch(inputUnigram, unifreq, quiztext))
matchfreq <- matchfreq[matchfreq > 0] #drop NoMatch
matchfreq <- tapply(unlist(matchfreq), names(unlist(matchfreq)), sum) #group by term and sum
matchfreq <- matchfreq[order(matchfreq, decreasing = TRUE)] # order by probability
matchfreq

optionmatch <- function(inputNgram, freqVector, options) {
  # match quiz options to model results; do not use for unigrams?
  #parse out last word of ngram after last "_" character, retain frequency
  ngramlength <- nchar(gsub("\\^", "", inputNgram)) #parse out leading caret if present
  names(freqVector) <- right(names(freqVector), nchar(names(freqVector)) - ngramlength)
  # stem the quiz options for better matching to ngram lists
  options <- wordstem(options)
  # match quiztext to freqmatch, capture matches and prob or display "no matches"
  matchidx <- match(options, names(freqVector), nomatch = 0)
  if (sum(matchidx) > 0) {
    freqVector[matchidx] #return matching terms and frequencies
    }
    else { # return blank vector value to inform user of no matches
      nomatch <- as.vector(0)
      names(nomatch) <- "NoMatch"
      nomatch
    }
}



# loop through string to find all bigrams therein and calculate probabilities
# calculate P(word 2 follows word 1): find count of all word1-word2 bigrams/divide by count of all word1-wordX bigrams
w1 <- "a" #x
w2 <- "girl" #y
trainbigram <- paste(w1, w2, sep = "_")
gramfreq <- length(grep(trainbigram, names(trifreq)))
gramall <- length(grep(trainbigram, names(trifreq)))
prob <- gramfreq/gramall
# apply log tranformation to prevent numerical underflow
# add all log probs then use exp fxn to come back to linear space
prob <- log10(prob)





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



