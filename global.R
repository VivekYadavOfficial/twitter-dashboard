library(twitteR)
library(tm)
library(wordcloud)
library(plyr)

authenticat <- function() {
  #create api key and others mentioned here by visiting dev.twitter.com
  api_key <- "HjgMdkxQRJfRB9UZySPKBsp60"
  api_secret <- "JfBqWkBDS3YGZxBUEbENFNO4jYcBf2IQFNspNo5kA5DlAo2u7b"
  access_token <- "596155228-upJWe5cACGBgwZ0xVPHxdmBP6rXB5Qcc6d6Sqjav"
  access_token_secret <- "FeW9Fcryr7CM8Soyl909LHuDtTSZ5pvRxGuz2chsUyrg1"

  #direct authentication
  setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
}
#wordlists
#pos <- scan(file="positive-words.txt", what="chraracter", comment=";")
#neg <- scan(file="negative-words.txt", what="character", comment=";")

#processing function
processTweets <- function(searchterm,ntweets,isdate,startD=NULL,endD=NULL,stype="mixed") {
  if(!isdate) {
    startD <- NULL
    endD <- NULL
  }
  t <- searchTwitter(searchterm, n=200, lang="en", since = startD, until = endD, geocode = NULL, resultType = stype)
  #extract the text part of the tweet data
  t.text <- sapply(t, function(t) t$getText())
  
  #cleaning the text
  #remove non-ASCII charaters
  t.text <- sapply(t.text, function(x) iconv(x, "latin1", "ASCII", sub=""))
  
  #using gsub, replace the unwanted text parts
  #replace rt from text
  t.text <- gsub("rt", "", t.text)
  
  #replace usernames
  t.text <- gsub("@\\w+", "", t.text)
  
  #similarly replacing tabs, blanck spaces, link etc.
  t.text <- gsub("[[:punct:]]", "", t.text)
  t.text <- gsub("[[:cntrl:]]", "", t.text)
  t.text <- gsub("http\\w+", "", t.text)
  t.text <- gsub("https\\w+", "", t.text)
  t.text <- gsub("[ |\t]{2,}", "", t.text)
  t.text <- gsub("^ ", "", t.text)
  t.text <- gsub(" $", "", t.text)
  t.text <- gsub("\\d+", "", t.text)
  
  #convert the text to lowercase
  t.text <- tolower(t.text)
  
  #create corpus of tweets
  t.text.corpus <- Corpus(VectorSource(t.text))
  
  #now remove stop words
  t.text.corpus <- tm_map(t.text.corpus, function(x) removeWords(x,stopwords()))
  
}
