# CapstoneData_v2.R
# this script preps the environment, sources data, pulls samples, creates a corpus
# runs various transformations and creates a TDM

# prep the environment
setwd("C:/Users/fellt/Desktop/Data Science/Coursera Data Science Specialization/10 - Capstone")
library(tm)
#load quanteda and dependencies
  library(SnowballC) 
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
set.seed(1003)

# acquire data
if(!file.exists("Coursera-SwiftKey.zip")){
  #' download data
  download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",
                "Coursera-SwiftKey.zip")
  
  #' unzip file
  unzip("Coursera-SwiftKey.zip")
}else{
  print("Already have data.")
}

# read in and sample data
samplerate <- .10
usBlogs <- readLines('final/en_US/en_US.blogs.txt')
usBlogSmall <- sample(usBlogs, length(usBlogs)*samplerate)
rm(usBlogs)
usNews <- readLines('final/en_US/en_US.news.txt')
usNewsSmall <- sample(usNews, length(usNews)*samplerate)
rm(usNews)
usTwitter <- readLines('final/en_US/en_US.twitter.txt', skipNul = TRUE)
usTwitterSmall <- sample(usTwitter, length(usTwitter)*samplerate)
rm(usTwitter)
usAllSmall <- c(usBlogSmall, usNewsSmall, usTwitterSmall)
rm(usBlogSmall)
rm(usNewsSmall)
rm(usTwitterSmall)

# save the usAllSmall collection of documents 
# save(usAllSmall, file = "usAllSmall.rda")
# reload the collection
# load("usAllSmall.Rda")

# clean up non-standard charaters
usAllSmall <- iconv(usAllSmall, "latin1", "ASCII", sub="")

# build corpus with quanteda
usVcorpQ <- corpus(usAllSmall)
summary(usVcorpQ)
class(usVcorpQ)
# save the usVcorpQ corpus 
# save(usVcorpQ, file = "usVcorpQ.Rda")
# reload the usVcorp corpus
# load("usVcorpQ.Rda")
rm(usAllSmall)

# prep corpus
usVcorpQ <- toLower(usVcorpQ)

# create tokens
unigramtokens <- tokenize(usVcorpQ, verbose=TRUE, removeNumbers=TRUE,removePunct=TRUE, removeTwitter = TRUE)
bigramtokens <- tokenize(usVcorpQ, verbose=TRUE, removeNumbers=TRUE,removePunct=TRUE, removeTwitter = TRUE, ngrams = 2)
trigramtokens <- tokenize(usVcorpQ, verbose=TRUE, removeNumbers=TRUE,removePunct=TRUE, removeTwitter = TRUE, ngrams = 3)
fourgramtokens <- tokenize(usVcorpQ, verbose=TRUE, removeNumbers=TRUE,removePunct=TRUE, removeTwitter = TRUE, ngrams = 4)
rm(usVcorpQ)

# create DFM for each and and apply profanity filtering
bad <- read.csv("bad.csv", header = TRUE, strip.white = TRUE, stringsAsFactors = FALSE) #vector of profanity words 
bad <- as.character(bad$words) # need to convert data frame columnn to a character vector for dfm to work
unigrams <- dfm(unigramtokens, verbose=TRUE, toLower=TRUE, ignoredFeatures=bad) #ignoredFeatures=c(bad, stopwords())) # should keep stop words to be consistent & help w/KN
rm(unigramtokens)
bigrams <- dfm(bigramtokens, verbose=TRUE, toLower=TRUE, ignoredFeatures=bad)
rm(bigramtokens)
trigrams <- dfm(trigramtokens, verbose=TRUE, toLower=TRUE, ignoredFeatures=bad)
rm(trigramtokens)
fourgrams <- dfm(fourgramtokens, verbose=TRUE, toLower=TRUE, ignoredFeatures=bad)
rm(fourgramtokens)
rm(bad)

# explore and validate dfms
nfeature(unigrams)
topfeatures(unigrams)
nfeature(bigrams)
topfeatures(bigrams)
nfeature(trigrams)
topfeatures(trigrams)
nfeature(fourgrams)
topfeatures(fourgrams)
ndoc(unigrams)

# save the dfms
save(unigrams, file = "unigrams.Rda")
save(bigrams, file = "bigrams.Rda")
save(trigrams, file = "trigrams.Rda")
save(fourgrams, file = "fourgrams.Rda")
