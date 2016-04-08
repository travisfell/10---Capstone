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

test <- unlist(strsplit("playing_the_violin", "_"))
test[length(test)]

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
uniprob <- unifreq/sum(unifreq)
biprob <- bifreq/sum(bifreq)
triprob <- trifreq/sum(trifreq)
fourprob <- fourfreq/sum(fourfreq)

save(uniprob, file = "uniprob.Rda")
save(unifreq, file = "unifreq.Rda")
save(bifreq, file = "bifreq.Rda")
save(trifreq, file = "trifreq.Rda")
save(fourfreq, file = "fourfreq.Rda")

# III. Experiment with the model

# A. create Maximum Likelihood Estimate matrix
# see this discussion: https://www.coursera.org/learn/data-science-project/module/VNKmf/discussions/HmPU3OvyEeWfwAohgaM63Q

inputTrigram <- "^of_Adam_Sandler_" # be sure to include trailing underbar
fourprobmatch <- fourprob[grep(inputTrigram, names(fourprob), ignore.case = TRUE)]
space1 <- regexpr(pattern ='_',inputTrigram)[1] #find location of first space
inputBigram <- paste("^", substring(inputTrigram, space1 + 1, nchar(inputTrigram)), sep = "")
triprobmatch <- triprob[grep(inputBigram, names(triprob), ignore.case = TRUE)]
space2 <- regexpr(pattern ='_',inputBigram)[1] 
inputUnigram <- paste("^", substring(inputBigram, space2 + 1, nchar(inputBigram)), sep = "")
biprobmatch <- biprob[grep(inputUnigram, names(biprob), ignore.case = TRUE)]

quiztext <- c("movies", "novels", "pictures", "stories")
matchprob <- optionmatch(inputTrigram, fourprobmatch, quiztext)
matchprob <- c(matchprob, optionmatch(inputUnigram, biprobmatch, quiztext))
matchprob <- c(matchprob, optionmatch(inputUnigram, uniprob, quiztext))
matchprob <- matchprob[matchprob > 0] #drop NoMatch
matchprob <- tapply(unlist(matchprob), names(unlist(matchprob)), sum) #group by term and sum
matchprob <- matchprob[order(matchprob, decreasing = TRUE)] # order by probability
matchprob

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

# Investigate Kneser - Ney smoothing (account for context), trying to find 2nd word in bigram
wi_1 <- "playing"
quizoptions <- c("daily", "weekly", "outside", "inside")
# confirm wi_1 is in bigram list corpus
if (wi_1 %in% names(unifreq)) {
  quizbigrams <- paste(wi_1, "_", quizoptions, sep = "")
  # find all wi's for this wi_1
  matchingbigrams <- bifreq[grep(paste("^",wi_1,"_", sep = ""), names(bifreq))]
  unigram_ct_wi_1 <- unifreq[[grep(paste("^", wi_1, "$", sep = ""), names(unifreq))]]
  D <- .75 # discount rate
  lambda <- D/unigram_ct_wi_1 * length(grep(paste("^",wi_1,"_", sep = ""), names(bifreq))) # last term is number of times discount applied to frequencies of wi-1, wi
  p_continuation_w <- length(grep(paste("^",wi_1,"_", sep = ""), names(bifreq)))
  totalBigramTypes <- length(bifreq)
  bigram_ct_wi_1 <- sum(bifreq[(grep(paste("^",wi_1,"_", sep = ""), names(bifreq)))] - D)
  for (i in 1:length(matchingbigrams)) {
    #
    bigram_ct_wi_1 <- matchingbigrams[[i]] - D
    matchingbigrams[i] <- max(bigram_ct_wi_1/unigram_ct_wi_1, 0) + lambda * (p_continuation_w/totalBigramTypes)
  }
  matchingbigrams <- matchingbigrams[order(matchingbigrams, decreasing = TRUE)]
    subset(matchingbigrams, names(matchingbigrams) %in% quizbigrams)
} else {
  subset(unifreq, names(unifreq) %in% quizoptions)
}


# update model above for app
wi_1 <- "playing with fire"
#need to do some text parsing here to get last full word, later last 2-3 words for recursive prediction
wi_1 <- unlist(strsplit(wi_1, " "))[length(unlist(strsplit(wi_1, " ")))]
if (wi_1 %in% names(unifreq)) {
  # find all wi's for this wi_1
  matchingbigrams <- bifreq[grep(paste("^",wi_1,"_", sep = ""), names(bifreq))]
  unigram_ct_wi_1 <- unifreq[[grep(paste("^", wi_1, "$", sep = ""), names(unifreq))]]
  D <- .75 # discount rate
  lambda <- D/unigram_ct_wi_1 * length(grep(paste("^",wi_1,"_", sep = ""), names(bifreq))) # last term is number of times discount applied to frequencies of wi-1, wi
  p_continuation_w <- length(grep(paste("^",wi_1,"_", sep = ""), names(bifreq)))
  totalBigramTypes <- length(bifreq)
  bigram_ct_wi_1 <- sum(bifreq[(grep(paste("^",wi_1,"_", sep = ""), names(bifreq)))] - D)
  for (i in 1:length(matchingbigrams)) {
    #
    bigram_ct_wi_1 <- matchingbigrams[[i]] - D
    matchingbigrams[i] <- max(bigram_ct_wi_1/unigram_ct_wi_1, 0) + lambda * (p_continuation_w/totalBigramTypes)
  }
  matchingbigrams <- matchingbigrams[order(matchingbigrams, decreasing = TRUE)]
  predWords <- gsub(paste(wi_1, "_", sep = ""), "", names(matchingbigrams[1:7]))
  predWords
} else {
  predWords <- names(uniprob[1:7])
}



# experiement with word cloud
library(wordcloud)
words <- unifreq
words <- words[!(names(words) %in% stopwords())]
wordcloud(words = names(words), freq = words, max.words = 25, random.order = FALSE)


# LATER: Attempt recursive model starting at fourgrams, then reapply to quiz 3
# also, need to rework using training and test sets in preparation for app. 


# OLD CODE
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


# create training, development and test sets using 80/10/10 split



