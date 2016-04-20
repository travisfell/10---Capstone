CapstoneSlideDeck
========================================================
author: Travis Fell
date: April 19, 2016
autosize: true

Overview of the Text Prediction Cloud App
========================================================
This simple and straight forward app predicts words from your entered text and displays it in one of two ways

- Top 3 most likely: Like handheld device keyboard apps, this app shows the top three most likely next words
- Word cloud: Expand the possibilities by seeing a word cloud of the top 25 predicted words. 


Method
========================================================
Below is an overview of the data processing, algorithm and testing methods for this app. 

1. Pulled a 10% sample of US news stories, tweets and blog posts. 
2. Subdivided the corpus into an 80/20 training/test set. 
3. Created unigram and bigram lists along with frequencies of each. 
4. Developed a Knesser-Ney algorithm using the bigram and unigram lists. 
5. Developed a testing algorithm that randomly sampled the test set 20 times and recorded how often the model predicted the last word within the top 3 results. After 10 simulations, the model consistently had betwen a 20% and 25% success rate. 

Demo
========================================================
[show screen captures and explanatory text]

