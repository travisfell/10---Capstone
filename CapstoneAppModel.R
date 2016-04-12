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

# load in n-grams objects if needed
 load("unigrams.Rda")
 load("bigrams.Rda")
 load("trigrams.Rda")
 load("fourgrams.Rda")

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


# Investigate Kneser - Ney smoothing (account for context), trying to find 2nd word in bigram
wi_1 <- 'playing'
#parse text to get last full word, later last 2-3 words for recursive prediction
wi_1 <- unlist(strsplit(wi_1, " "))[length(unlist(strsplit(wi_1, " ")))]
if (wi_1 %in% names(unifreq)) { # if entered word not in the corpus
  matchingbigrams <- bifreq[grep(paste("^",wi_1,"_", sep = ""), names(bifreq))]
  unigram_ct_wi_1 <- unifreq[[grep(paste("^", wi_1, "$", sep = ""), names(unifreq))]]
  D <- .75 # discount rate
  lambda <- D/unigram_ct_wi_1 * length(grep(paste("^",wi_1,"_", sep = ""), names(bifreq))) # last term is number of times discount applied to frequencies of wi-1, wi
  p_continuation_w <- length(grep(paste("^",wi_1,"_", sep = ""), names(bifreq)))
  totalBigramTypes <- length(bifreq)
  bigram_ct_wi_1 <- sum(bifreq[(grep(paste("^",wi_1,"_", sep = ""), names(bifreq)))] - D)
  for (i in 1:length(matchingbigrams)) {
    bigram_ct_wi_1 <- matchingbigrams[[i]] - D
    matchingbigrams[i] <- max(bigram_ct_wi_1/unigram_ct_wi_1, 0) + lambda * (p_continuation_w/totalBigramTypes)
  }
  predWordsProb <- matchingbigrams[order(matchingbigrams, decreasing = TRUE)]
  predWordsProb <- predWordsProb[1:25]
  names(predWordsProb) <- gsub(paste(wi_1, "_", sep = ""), "", names(predWordsProb))
  predWords <- names(predWordsProb)
} else { # if entered word not in corpus
  predWordsProb <- uniprob
  predWordsProb <- predWordsProb[!(names(predWordsProb) %in% stopwords())]
  predWordsProb <- predWordsProb[1:25]
  predWords <- names(predWordsProb)
}
predWords

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
