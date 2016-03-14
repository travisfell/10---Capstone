# CapStone quiz 1


#quiz question 3
usBlogsBig <-  readLines(usBlogs)
usNewsBig <- readLines(usNews)
max(nchar(usBlogsBig))
max(nchar(usNewsBig))

#quiz question 4
usTwitBig <- readLines(usTwit)

twitLove <- length(grep("love", usTwitBig))
twitHate <- length(grep("hate", usTwitBig))
twitLove/twitHate

#quiz question 5
usTwitBig[grep("biostats", usTwitBig)]

#quiz question 6
grep("A computer once beat me at chess, but it was no match for me at kickboxing", usTwitBig)
