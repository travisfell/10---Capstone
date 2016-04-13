# CapstoneExplorationModeling.R
# This script contains code to prep the environment, load in 
# objects from week one, and conduct the exporation and modeling
# required for week two. 

# I. Prep the Environment
# first, let's set the working directory
setwd("C:/Users/fellt/Desktop/Data Science/Coursera Data Science Specialization/10 - Capstone")

# load up some libraries we'll probably need

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
set.seed(1003)
options(scipen=999) # turn off scientific notation

right = function (string, char){
  substr(string,nchar(string)-(char-1),nchar(string))
}

# create training, development and test sets
indexes = sample(1:ndoc(usVcorpQ), size=0.1*ndoc(usVcorpQ))
usVcorpQ_train <- corpus(usVcorpQ[-indexes])
usVcorpQ_test <- corpus(usVcorpQ[indexes])
save(usVcorpQ_train, file = "usVcorpQ_train.rda")
save(usVcorpQ_test, file = "usVcorpQ_test.rda")

# prep corpus
usVcorpQ_train <- toLower(usVcorpQ_train)
# create tokens
unigramtokens_train <- tokenize(usVcorpQ_train, verbose=TRUE, removeNumbers=TRUE,removePunct=TRUE, removeTwitter = TRUE)
bigramtokens_train <- tokenize(usVcorpQ_train, verbose=TRUE, removeNumbers=TRUE,removePunct=TRUE, removeTwitter = TRUE, ngrams = 2)
trigramtokens_train <- tokenize(usVcorpQ_train, verbose=TRUE, removeNumbers=TRUE,removePunct=TRUE, removeTwitter = TRUE, ngrams = 3)
fourgramtokens_train <- tokenize(usVcorpQ_train, verbose=TRUE, removeNumbers=TRUE,removePunct=TRUE, removeTwitter = TRUE, ngrams = 4)
rm(usVcorpQ_train)

# create DFM for each and and apply profanity filtering
bad <- read.csv("bad.csv", header = TRUE, strip.white = TRUE, stringsAsFactors = FALSE) #vector of profanity words 
bad <- as.character(bad$words) # need to convert data frame columnn to a character vector for dfm to work
unigrams_train <- dfm(unigramtokens_train, verbose=TRUE, toLower=TRUE, ignoredFeatures=bad) #ignoredFeatures=c(bad, stopwords())) # should keep stop words to be consistent & help w/KN
rm(unigramtokens_train)
bigrams_train <- dfm(bigramtokens_train, verbose=TRUE, toLower=TRUE, ignoredFeatures=bad)
rm(bigramtokens_train)
trigrams_train <- dfm(trigramtokens_train, verbose=TRUE, toLower=TRUE, ignoredFeatures=bad)
rm(trigramtokens_train)
fourgrams_train <- dfm(fourgramtokens_train, verbose=TRUE, toLower=TRUE, ignoredFeatures=bad)
rm(fourgramtokens_train)


# load in n-grams objects if needed
 load("unigrams.Rda")
 load("bigrams.Rda")
 load("trigrams.Rda")
 load("fourgrams.Rda")

# II. Find and sort terms matrices by frequency
unifreq_train <- colSums(unigrams_train) # may also try coercion to data frame or matrix per the help text
unifreq_train <- unifreq_train[order(unifreq_train, decreasing = TRUE)]
bifreq_train <- colSums(bigrams_train)
bifreq_train <- bifreq_train[order(bifreq_train, decreasing = TRUE)] 
trifreq_train <- colSums(trigrams_train)
trifreq_train <- trifreq_train[order(trifreq_train, decreasing = TRUE)]
fourfreq_train <- colSums(fourgrams_train)
fourfreq_train <- fourfreq_train[order(fourfreq_train, decreasing = TRUE)]

# remember, bigrams and trigrams have "_" separator. To remove '_' characters from bigrams and trigrams for matching: 
# names(bifreq) <- gsub("_", " ", names(bifreq))

#convert frequencies to probabilities
uniprob_train <- unifreq_train/sum(unifreq_train)
biprob_train <- bifreq_train/sum(bifreq_train)
triprob_train <- trifreq_train/sum(trifreq_train)
fourprob_train <- fourfreq_train/sum(fourfreq_train)

save(uniprob_train, file = "uniprob_train.Rda")
save(unifreq_train, file = "unifreq_train.Rda")
save(bifreq_train, file = "bifreq_train.Rda")
save(trifreq_train, file = "trifreq_train.Rda")
save(fourfreq_train, file = "fourfreq_train.Rda")


# III. Experiment with and test the model
#init testing variables
totaltests <- 50
matchedtests <- 0

for (a in 1:totaltests) {
  #pull random phrases from test set and prep for comparison to model
  testdoc <- sample(usVcorpQ_test, size = 1)
  testdoc$documents <- toLower(testdoc$documents)
  ngramstokens_test <- tokenize(testdoc$documents, verbose=FALSE, removeNumbers=TRUE,removePunct=TRUE, removeTwitter = TRUE, ngrams = 2)
  ngrams_test <- dfm(ngramstokens_test, verbose=FALSE, toLower=TRUE, ignoredFeatures=bad)
  testNgram <- sample(features(ngrams_test), 1)
  testNgram <- sub("_", " ", testNgram)
  wi <- unlist(strsplit(testNgram, " "))[length(unlist(strsplit(testNgram, " ")))] # word attempting to predict
  wi_1 <- sub(paste(" ", wi, sep = ""), "", testNgram)
  
  # Investigate Kneser - Ney smoothing (account for context), trying to find 2nd word in bigram
  #wi_1 <- 'playing'
  #parse text to get last full word, later last 2-3 words for recursive prediction
  #wi_1 <- unlist(strsplit(wi_1, " "))[length(unlist(strsplit(wi_1, " ")))]
  if (wi_1 %in% names(unifreq_train)) { # if entered word not in the corpus
    matchingbigrams <- bifreq_train[grep(paste("^",wi_1,"_", sep = ""), names(bifreq_train))]
    unigram_ct_wi_1 <- unifreq_train[[grep(paste("^", wi_1, "$", sep = ""), names(unifreq_train))]]
    D <- .75 # discount rate
    lambda <- D/unigram_ct_wi_1 * length(grep(paste("^",wi_1,"_", sep = ""), names(bifreq_train))) # last term is number of times discount applied to frequencies of wi-1, wi
    p_continuation_w <- length(grep(paste("^",wi_1,"_", sep = ""), names(bifreq_train)))
    totalBigramTypes <- length(bifreq_train)
    bigram_ct_wi_1 <- sum(bifreq_train[(grep(paste("^",wi_1,"_", sep = ""), names(bifreq_train)))] - D)
    for (i in 1:length(matchingbigrams)) {
      bigram_ct_wi_1 <- matchingbigrams[[i]] - D
      matchingbigrams[i] <- max(bigram_ct_wi_1/unigram_ct_wi_1, 0) + lambda * (p_continuation_w/totalBigramTypes)
    }
    predWordsProb <- matchingbigrams[order(matchingbigrams, decreasing = TRUE)]
    predWordsProb <- predWordsProb[1:25]
    names(predWordsProb) <- gsub(paste(wi_1, "_", sep = ""), "", names(predWordsProb))
    predWords <- names(predWordsProb)
    } else { # if entered word not in corpus
    predWordsProb <- uniprob_train
    predWordsProb <- predWordsProb[!(names(predWordsProb) %in% stopwords())]
    predWordsProb <- predWordsProb[1:25]
    predWords <- names(predWordsProb)
    }
  if (wi %in% predWords[1:3]) {
      #print(c(paste("wi term: ", wi, " from test ngram ", testNgram, " in top 3 predicted words: ", sep  = ""), predWords[1:3]))
      matchedtests <- matchedtests + 1
    } else {
      #print(c(paste("wi term:  ", wi, " from test ngram ", testNgram, " not in top 3 predicted words: ", sep  = ""),  predWords[1:3]))
    }
  print(paste("Completed test ", a, ". Current match % is ", matchedtests/a,  sep = ""))
}  
print(paste("Final test match rate = ", matchedtests/totaltests, sep = ""))


# create word cloud of top 25 results
textwordcloud <- wordcloud(words = names(predWordsProb)
                           , random.color = TRUE
                           , rot.per = .25
                           , colors = c("black","red", "blue", "green", "orange", "brown", "pink", "yellow")
                           , scale = c(3, .5)
                           , freq = predWordsProb
                           , max.words = 25
                           , random.order = FALSE
                           )


# return values
return(list(predWords[1:5], textwordcloud))



# LATER: Attempt recursive model starting at fourgrams, then reapply to quiz 3
# also, need to rework using training and test sets in preparation for app. 
