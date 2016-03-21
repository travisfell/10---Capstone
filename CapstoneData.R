# Capstone Date acquisition and exploration script
# this script obtains the data for the Capstone project
# and executes normal data exploration and cleaning steps 
# associated with any new data sets


# first, let's set the working directory
setwd("C:/Users/fellt/Desktop/Data Science/Coursera Data Science Specialization/10 - Capstone")

# load up some libraries we'll probably need
library(R.utils)
library(tm)
library(SnowballC) # to help stemDocument work
# load libraries to assist with multi threaded processing
library(parallel, quietly=T)
library(doParallel, quietly=T)
# turn on parallel processing to help improve performance
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

# Now, download the data using the link on the 
# capstone webpage
download.file("https://eventing.coursera.org/api/redirectStrict/VYmaDK_AgFQF7Lxt6LQVf1i3BcAkm_OZwAe5IU-m0_bP_AbIKyNnwWBGqIYPMxeXi1jFreu9mSce3kkDEcrB5Q.nKFU8mGvHIF_NqifbTJOCg.PdPVLe2Tri_4JhLMtsFKCsi03HwSetzTeY6v4Buz24fmTYdGQBiF9dRfOPZlQgL9ix2vfkOzQDxxTH_IlLlQ2oRmiUWjyU-pEG015VYID3tcSAZItG_4Wx_s4qiNbjMR7jp0An9Xj2YWWEni11DV8Fnz4RFr11cAMWAH3ycvlVVRAz6iYnfR_tN_xIcbmyJwJPVm7GMMr9mt_Wf2L68TCs2VUcsdzIRfKIflXqFF-c1oLltQsq618r22dChpkXvMB5CKsffMa9najDCB2t5yCLOcaRkvcYAOOt8M2Ui_eYcJoaL8OQIh0m78KThHe-9Yvb7RxDbL4oDZKDc4ZiaLctIJKPYpvxZVfrSZRcVzsVJcAkc1mmzjuh35LTDfjwsrudpCMlPfSmNNB5HwGYz4WWqVe4Td4Q3T9NXgM-SMb2U",
    destfile = 'capstone.zip')

# read in random sample of the data for exploration and cleaning tasks
# assign document paths to variables for later use
usBlogs <- 'final/en_US/en_US.blogs.txt'
usNews <- 'final/en_US/en_US.news.txt'
usTwit <- 'final/en_US/en_US.twitter.txt'

# get line count for each file
usBlogsAll <- as.numeric(countLines(usBlogs))
usNewsAll <- as.numeric(countLines(usNews))
usTwitAll <- as.numeric(countLines(usTwit))


# define function to read files and output smaller sample files
subsamfile <- function(infile,outfile,k,header=T) {
  ci <- file(infile,"r")
  co <- file(outfile,"w")
  if (header) {
    hdr <- readLines(ci,n=1)
    writeLines(hdr,co)
  }
  recnum = 0
  numout = 0
  while (TRUE) {
    inrec <- readLines(ci,n=1)
    if (length(inrec) == 0) { # end of file?
      close(co) 
      return(numout)
    }
    recnum <- recnum + 1
    if (recnum %% k == 0) {
      numout <- numout + 1
      writeLines(inrec,co)
    }
  }
}

# call function for each file, then close connection
subsamfile(usBlogs, 'final-sample/en_US/en_US.blogs.sample.txt', 100, F)
close(file(usBlogs))
subsamfile(usNews, 'final-sample/en_US/en_US.news.sample.txt', 100, F)
close(file(usNews))
subsamfile(usTwit, 'final-sample/en_US/en_US.twitter.sample.txt', 100, F)
close(file(usTwit))

# read in sampled files
usBlogsSmall <- read.csv2(file = 'final-sample/en_US/en_US.blogs.sample.txt', stringsAsFactors = FALSE, header = FALSE, quote = "")
usNewsSmall <- read.csv2(file = 'final-sample/en_US/en_US.news.sample.txt', stringsAsFactors = FALSE, header = FALSE, quote = "")
usTwitSmall <- read.csv2(file = 'final-sample/en_US/en_US.twitter.sample.txt', stringsAsFactors = FALSE, header = FALSE, quote = "")
usAllSmall <- rbind(usBlogsSmall, usNewsSmall, usTwitSmall)

#get word count for each file
usVcorpBlogs <- VCorpus(DataframeSource(usBlogsSmall), list(reader = readPlain))
usVcorpBlogsTDM <- TermDocumentMatrix(usVcorpBlogs)
length(Terms(usVcorpBlogsTDM)) # 47990

usVcorpTwit <- VCorpus(DataframeSource(usTwitSmall), list(reader = readPlain))
usVcorpTwitTDM <- TermDocumentMatrix(usVcorpTwit)
length(Terms(usVcorpTwitTDM)) # 44248

usVcorpTwit <- VCorpus(DataframeSource(usNewsSmall), list(reader = readPlain))
usVcorpNewsTDM <- TermDocumentMatrix(usVcorpNews)
length(Terms(usVcorpNewsTDM)) # 8863


# search and replace certain contractions: 'd , 're , n't , 'll , 've , it's 
head(usAllSmall[grepl("you d", usAllSmall[,1], ignore.case = TRUE) == TRUE,], n = 5)
usAllSmall[grepl("ya'll", usAllSmall[,1], ignore.case = TRUE) == TRUE,] <- gsub("ya'll", "yall ", usAllSmall[grepl("ya'll", usAllSmall[,1], ignore.case = TRUE) == TRUE,], ignore.case = TRUE) #need to resolve this edge case before handling other contractions of this type
usAllSmall[grepl("can't", usAllSmall[,1], ignore.case = TRUE) == TRUE,] <- gsub("can't", "cannot ", usAllSmall[grepl("can't", usAllSmall[,1], ignore.case = TRUE) == TRUE,], ignore.case = TRUE) #need to resolve this edge case before handling other contractions of this type
usAllSmall[grepl("'ll ", usAllSmall[,1], ignore.case = TRUE) == TRUE,] <- gsub("'ll ", " will ", usAllSmall[grepl("'ll ", usAllSmall[,1], ignore.case = TRUE) == TRUE,], ignore.case = TRUE)
usAllSmall[grepl("'d ", usAllSmall[,1], ignore.case = TRUE) == TRUE,] <- gsub("'d ", " would ", usAllSmall[grepl("'d ", usAllSmall[,1], ignore.case = TRUE) == TRUE,], ignore.case = TRUE)
usAllSmall[grepl("'re ", usAllSmall[,1], ignore.case = TRUE) == TRUE,] <- gsub("'re ", " are ", usAllSmall[grepl("'re ", usAllSmall[,1], ignore.case = TRUE) == TRUE,], ignore.case = TRUE)
usAllSmall[grepl("'ve ", usAllSmall[,1], ignore.case = TRUE) == TRUE,] <- gsub("'ve ", " have ", usAllSmall[grepl("'ve ", usAllSmall[,1], ignore.case = TRUE) == TRUE,], ignore.case = TRUE)
usAllSmall[grepl("n't ", usAllSmall[,1], ignore.case = TRUE) == TRUE,] <- gsub("n't ", " not ", usAllSmall[grepl("n't ", usAllSmall[,1], ignore.case = TRUE) == TRUE,], ignore.case = TRUE)
usAllSmall[grepl("it's ", usAllSmall[,1], ignore.case = TRUE) == TRUE,] <- gsub("it's ", "it is ", usAllSmall[grepl("it's ", usAllSmall[,1], ignore.case = TRUE) == TRUE,], ignore.case = TRUE)

# identify tokens in each data set
# 1. import and inspect the data using tm
#usVcorp <- VCorpus(DirSource("final-sample/en_US"), list(reader = readPlain)) 
#usVcorp <- VCorpus(DirSource("final-sample/en_US")
#                   , list(reader = readPlain)
#                   , dbControl = list(dbName = "pcorpus.db", dbType = "DB1")
#                  )

usVcorp <- VCorpus(DataframeSource(usAllSmall), list(reader = readPlain))

# created usVcorp_bkup after this step, usVcorp_bkup <- usVcorp
#usVcorp <- usVcorp_bkup
class(usVcorp)
inspect(usVcorp)

# 2. transform the data
usVcorp <- tm_map(usVcorp, content_transformer(tolower))
# transform common contractions
#  usVcorp <- tm_map(usVcorp, function(x) {gsub("'re ", " are ", x)})
#  usVcorp <- tm_map(usVcorp, function(x) {gsub("'ll ", " will ", x)})
#  usVcorp <- tm_map(usVcorp, function(x) {gsub("'d ", " would ", x)})
#  usVcorp <- tm_map(usVcorp, function(x) {gsub("it's", "it is", x)})
#  usVcorp <- tm_map(usVcorp, function(x) {gsub(" i'm ", " i am ", x)})
usVcorp <- tm_map(usVcorp, removePunctuation)
usVcorp <- tm_map(usVcorp, removeWords, stopwords("english")) 
usVcorp <- tm_map(usVcorp, stripWhitespace)
usVcorp <- tm_map(usVcorp, removeNumbers)
#Filter for profanity
  bad <- read.csv("bad.csv", header = TRUE, strip.white = TRUE, stringsAsFactors = FALSE) # see http://www.cs.cmu.edu/~biglou/resources/bad-words.txt
  usVcorp <- tm_map(usVcorp, removeWords, bad$words)
#usVcorp = tm_map(usVcorp, content_transformer(function(x) iconv(x, to="ASCII", sub=" "))) #remove odd characters
usVcorp = tm_map(usVcorp, function(x) iconv(x, to="ASCII", sub=" ")) #remove odd characters
usVcorp <- tm_map(usVcorp, stemDocument)
usVcorp <- tm_map(usVcorp, PlainTextDocument)


# save the usVcorp corpus 
#save(usVcorp, file = "usVcorp.Rda")
# reload the usVcorp corpus
#load("usVcorp.Rda")
                          
# 3. Build TDM
usVcorpTDM <- TermDocumentMatrix(usVcorp)

# save the usVcorpTDM TDM 
#save(usVcorpTDM, file = "usVcorpTDM.Rda")
# reload the usVcorpTDM TDM
#load("usVcorpTDM.Rda")

class(usVcorpTDM)
inspect(usVcorpTDM)



# how to handle contractions? 
# find distinct list of all words with appostrophes
# create gsub routines for those words, run before the removePunctuation step
usVcorp_bkup2 <- usVcorp_bkup
usVcorp_bkup2 <- tm_map(usVcorp_bkup2, content_transformer(tolower))
usVcorp_bkup2 <- tm_map(usVcorp_bkup2, content_transformer(gsub), pattern = "you're", replacement = "you are", x = x, ignore.case = TRUE))
usVcorp_bkup2 <- tm_map(usVcorp_bkup2, content_transformer(gsub), pattern = "i'll", replacement = " will", ignore.case = TRUE)
usVcorp_bkup2 <- tm_map(usVcorp_bkup2, content_transformer(gsub), pattern = "we'll", replacement = "we will", ignore.case = TRUE)
usVcorp_bkup2 <- tm_map(usVcorp_bkup2, content_transformer(gsub), pattern = "it's", replacement = "it is", ignore.case = TRUE)
usVcorp_bkup2 <- tm_map(usVcorp_bkup2, content_transformer(gsub), pattern = "i'm", replacement = "i am", ignore.case = TRUE)
usVcorpTDMRaw <- TermDocumentMatrix(usVcorp_bkup2)
rawTerms <- Terms(usVcorpTDMRaw)
grep("you're", rawTerms, value = TRUE, ignore.case = TRUE)
rawTerms <- gsub("'ll", " will", rawTerms)

