# CapstoneExplorationModeling.R
# This script contains code to prep the environment, load in 
# objects from week one, and conduct the exporation and modeling
# required for week two. 


# first, let's set the working directory
setwd("C:/Users/fellt/Desktop/Data Science/Coursera Data Science Specialization/10 - Capstone")

# load up some libraries we'll probably need
library(tm)
library(SnowballC) # to help stemDocument work
# load libraries to assist with multi threaded processing
library(parallel, quietly=T)
library(doParallel, quietly=T)
# turn on parallel processing to help improve performance
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

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

inspect(usVcorpTDM)

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

# find foreign language words

# filter out garbled text

# find n-grams (2 and 3 word)
