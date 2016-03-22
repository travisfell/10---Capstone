# CapstoneModelDevelopment.R
# This file is only for developing the predictive n-gram model
# and back off model for unseen n-grams

# prep environment
library(tm)
library(NLP)
library(quanteda)
library(data.table)

# test pulling first matched terms by frequency by each keystroke
freqFrame <- as.data.frame(freq)
freqFrame <- setDT(freqFrame, keep.rownames = TRUE)
freqFrame <- freqFrame[order(freqFrame$freq, decreasing = TRUE),]
freqFrameOrd <- freqFrame[grep('^hap.*', freqFrame$rn),] #incrementally add key strokes in grep stmt
head(freqFrameOrd[order(freqFrameOrd$freq, decreasing = TRUE),], n = 7)
             
# test pulling matched bigrams after first term entered (first white space entered by user)
bigramFreqFrame <- as.data.frame(freq.bigram)
bigramFreqFrame <- setDT(bigramFreqFrame, keep.rownames = TRUE)
bigramFreqFrame <- bigramFreqFrame[order(bigramFreqFrame$freq.bigram, decreasing = TRUE),]
bigramFreqFrameOrd <- bigramFreqFrame[grep('^happy .*', bigramFreqFrame$rn),] #incrementally add key strokes in grep stmt
head(bigramFreqFrameOrd[order(bigramFreqFrameOrd$freq, decreasing = TRUE),], n = 7)

# test pulling matched trigrams after first term entered (first white space entered by user)
trigramFreqFrame <- as.data.frame(freq.trigram)
trigramFreqFrame <- setDT(trigramFreqFrame, keep.rownames = TRUE)
trigramFreqFrame <- trigramFreqFrame[order(trigramFreqFrame$freq.trigram, decreasing = TRUE),]
trigramFreqFrameOrd <- trigramFreqFrame[grep('^happy mothers .*', trigramFreqFrame$rn),] #incrementally add key strokes in grep stmt
head(trigramFreqFrameOrd[order(trigramFreqFrameOrd$freq, decreasing = TRUE),], n = 7)

# include "backoff" or missing terms code here