#global.R
#This script is for storing functions used by server.R and ui.R.

#prep the environment
library(shiny)
library(quanteda)
load("uniprob.Rda")
load("unifreq.Rda")
load("bifreq.Rda")

right = function (string, char){
  substr(string,nchar(string)-(char-1),nchar(string))
}

textpred <- function(enteredtext){
  wi_1 <- enteredtext
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
    matchingbigrams <- matchingbigrams[order(matchingbigrams, decreasing = TRUE)]
    predWords <- gsub(paste(wi_1, "_", sep = ""), "", names(matchingbigrams[1:7]))
    predWords
    } else { # if entered word not in corpus
    predWords <- names(uniprob[1:7])
    }
  predWords
}

# need to create word cloud here, send back to UI via different variable