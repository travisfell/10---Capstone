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
# find number of lines in each file
usBlogs <- 'final/en_US/en_US.blogs.txt'
usNews <- 'final/en_US/en_US.news.txt'
usTwit <- 'final/en_US/en_US.twitter.txt'

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


# identify tokens in each data set
# 1. import and inspect the data using tm
#usVcorp <- VCorpus(DirSource("final-sample/en_US"), list(reader = readPlain)) 
#usVcorp <- VCorpus(DirSource("final-sample/en_US")
#                   , list(reader = readPlain)
#                   , dbControl = list(dbName = "pcorpus.db", dbType = "DB1")
#                  )

usBlogsVcorp <- VCorpus(DataframeSource(usBlogsSmall), list(reader = readPlain))
usNewsVcorp <- VCorpus(DataframeSource(usNewsSmall), list(reader = readPlain))
usTwitVcorp <- VCorpus(DataframeSource(usTwitSmall), list(reader = readPlain))

usVcorp <- c(usBlogsVcorp, usNewsVcorp, usTwitVcorp)

rm(usBlogsVcorp)
rm(usNewsVcorp)
rm(usTwitVcorp)

class(usVcorp)
summary(usVcorp)
inspect(usVcorp)

# 2. transform the data
usVcorp <- tm_map(usVcorp, content_transformer(tolower))
usVcorp <- tm_map(usVcorp, removePunctuation)
usVcorp <- tm_map(usVcorp, removeWords, stopwords("english")) 
usVcorp <- tm_map(usVcorp, stripWhitespace)
usVcorp <- tm_map(usVcorp, removeNumbers)
#Filter for profanity
  bad <- read.csv("bad.csv", header = TRUE, strip.white = TRUE, stringsAsFactors = FALSE) # see http://www.cs.cmu.edu/~biglou/resources/bad-words.txt
  usVcorp <- tm_map(usVcorp, removeWords, bad$words)
usVcorp <- tm_map(usVcorp, stemDocument)
usVcorp = tm_map(usVcorp, content_transformer(function(x) iconv(x, to="ASCII", sub=" "))) #remove odd characters

# save the usVcorp corpus 
#save(usVcorp, file = "usVcorp.Rda")
# reload the usVcorp corpus
#load("usVcorp.Rda")
                          
# 4. Build TDM
usVcorpTDM <- TermDocumentMatrix(usVcorp)

# save the usVcorpTDM TDM 
#save(usVcorpTDM, file = "usVcorpTDM.Rda")
# reload the usVcorpTDM TDM
#load("usVcorpTDM.Rda")

class(usVcorpTDM)
summary(usVcorpTDM)
head(usVcorpTDM, n = 10)
