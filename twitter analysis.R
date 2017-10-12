#install.packages("RColorBrewer")
#install.packages("tm")
#install.packages("wordcloud")
#install.packages('base64enc')
#install.packages('ROAuth')
#install.packages('plyr')
#install.packages('stringr')
#install.packages('twitteR')

library(RColorBrewer)
library(wordcloud)
library(tm)
library(twitteR)
library(ROAuth)
library(plyr)
library(stringr)
library(base64enc)

#download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")

# Set constant requestURL
requestURL <- "https://api.twitter.com/oauth/request_token"
# Set constant accessURL
accessURL <- "https://api.twitter.com/oauth/access_token"
# Set constant authURL
authURL <- "https://api.twitter.com/oauth/authorize"

setup_twitter_oauth(consumerKey,consumerSecret,accessToken,accessTokenSecret)
consumerKey <- "DwaHiZJhpjelZxL8cENqOobCl"
consumerSecret <- "rq7c7qAnyzpyP4RFYptRZ7vBQSqzdAODCjq5YAzhL4JwlhrFD4"
accessToken <- "2874133135-JVx3N70ToNxiF6QYwERIX87yGyIeilAb0lrKLCa"
accessTokenSecret <- "U71q9Aat9qJUCgGZc9JEBCB0utp30CEPmp7GzT5lnno4O"

#Objectname <- searchTwitter(searchString, n=no.of tweets, lang=NULL)
insta <- searchTwitter('#HappyIndependanceDay',n=5000,lang = 'en')
length(insta)
insta

#tweet=userTimeline("@BarackObama",n=100)
#To get tweets from your home timeline.
#.homeTimeline (n=15)
#Get your tweets in which you were tagged in
#mentions (n=15)

#install.packages("SnowballC")
library(wordcloud)
library(SnowballC)
library(tm)
#Step 1 : Extract tweets from Twitter.
insta <- searchTwitter('#IndependenceDayIndia',n=5000,lang = 'en')
#Step 2 : Identiy & create text files to turn into a cloud.
#The first step is to identify & create text files on which you want to create the word cloud.
insta_text <- sapply(insta, function(x) x$getText())
#Step 3 : Create a corpus from the collection of text files.
insta_text_corpus <- Corpus(VectorSource(insta_text))
#Step 4 : Data Cleaning on the text files
#4.1 Remove punctuation.
insta_text_corpus <- tm_map(insta_text_corpus, removePunctuation)
#Transform text to lower case.
insta_text_corpus <- tm_map(insta_text_corpus, content_transformer(tolower))
#4.3 To remove stopwords.
insta_text_corpus <- tm_map(insta_text_corpus, function(x)removeWords(x,stopwords()))
#4.4 Remove your own stop word
# specify your stopwords as a character vector
insta_text_corpus <- tm_map(insta_text_corpus, removeWords, c("RT", "are","that"))
#4.6 Remove URL's from text
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
insta_text_corpus <- tm_map(insta_text_corpus, content_transformer(removeURL))
#Step 5 : Build a term-document matrix
insta_2 <- TermDocumentMatrix(insta_text_corpus)
insta_2 <- as.matrix(insta_2)
insta_2 <- sort(rowSums(insta_2),decreasing=TRUE)
#Converting words to dataframe
insta_2 <- data.frame(word = names(insta_2),freq=insta_2)
#The frequency table of words
head(insta_2, 10)
#Step 6 : Plot word frequencies
barplot(insta_2[1:10,]$freq, las = 2, names.arg = insta_2[1:10,]$word,
        col ="yellow", main ="Most frequent words",
        ylab = "Word frequencies")
#Step 7 : Generate the Word cloud
set.seed(1234)1
wordcloud(insta_text_corpus,min.freq=1,max.words=80,scale=c(2.2,1), colors=brewer.pal(8, "Dark2"), random.color=T , random.order=F)

#sentimental analysis
pos.words <- read.csv("pve.csv")
neg.words <- read.csv("nve.csv")
#Step 1 : Scan the words into R
pos.words <- scan("pve.csv",what = 'character')
neg.words <- scan("nve.csv",what = 'character')
#Step 2 : If you want to add your words into the positve and negative words list
pos.words = c(pos.words, 'new','nice' ,'good', 'horizon')
neg.words = c(neg.words, 'wtf', 'behind','feels', 'ugly', 'back','worse' , 'shitty', 'bad' ,'freaking' ,'sucks','horrible')
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use 
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}
#Step 3 : Put tweets into data frame
test <- ldply(insta,function(t) t$toDataFrame() )
#Step 4 : Apply sentiment Function to the tweets
result <- score.sentiment(test$text,pos.words,neg.words)
#Step 5 : Summary of the Scores
summary(result$score)
#Step 6 : Histogram of the Scores
hist(result$score,col = "yellow", main ="Score of tweets", xlab = "result$score"
     ,ylab = "Count of tweets")
#Step 7 : Count of tweets as per score
count(result$score)
#Step 8 : Count of Scores of tweets
library(ggplot2)
qplot(result$score,xlab = "Score of tweets")

