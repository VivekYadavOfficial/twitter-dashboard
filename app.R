#vivekyadavofficial

#load package
library(shiny)

#load global objects
#source("global.R", local = T)

library(twitteR)
library(tm)
library(wordcloud)
library(plyr)
library(ggplot2)
library(ggmap)

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
pos <- scan(file="positive-words.txt", what="chraracter", comment=";")
neg <- scan(file="negative-words.txt", what="character", comment=";")


#sentiment scoring function
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
  
{
  
  require(plyr)
  
  require(stringr)
  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # split into words. str_split is in the stringr package
    
    word.list = str_split(sentence, '\\s+')
    
    # sometimes a list() is one level of hierarchy too much
    
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    
    pos.matches = match(words, pos.words)
    
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    
    pos.matches = !is.na(pos.matches)
    
    neg.matches = !is.na(neg.matches)
    
    #sum all the non NA to find the score of the tweet
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
    
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  
  return(scores.df)
  
}

#processing function
processTweets <- function(searchterm,ntweets,isdate,startD=NULL,endD=NULL,stype="mixed") {
  if(!isdate) {
    startD <- NULL
    endD <- NULL
  }
  t <- searchTwitter(searchterm, n=50, lang="en", since = startD, until = endD, geocode = NULL, resultType = stype)
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
  t.text.corpus <<- tm_map(t.text.corpus, function(x) removeWords(x,stopwords()))
  
  #tweet to data.frame
  t.df <<- twListToDF(t)
}

#ui
#fluidPage ui layout
ui <- fluidPage(
  h1("Twitter Analytics  Dashboard", align="center"),
  hr(),
  #fluidRow(column(12,
  #textInput("searchbox", label = "Search Box", placeholder = "Enter #tag or search term" ),actionButton("textsubmit",label = "Search"),offset = 4)),
  #hr(),
  sidebarLayout(
    sidebarPanel(
      helpText("Tweak settings"),
      textInput("searchbox", label = "Search Box",placeholder = "Enter #tag or search term"),
      selectInput("stype",
                  label = "Search Type",
                  choices = c("popular","recent","mixed"),
                  selected = "mixed"),
      sliderInput("ntweets",
                  label = "Number of Tweets to fetch:",
                  min = 100, max = 1000, value = 200),
      checkboxInput("isdate",label = "Date Range?",value = F),
      dateRangeInput("daterange", label = NULL),
      actionButton("submitbutton",label = "Search"),width = 3),
    mainPanel("Visuaizations:",
              tabsetPanel(
                tabPanel("WordCloud", plotOutput("wordcloud")),
                tabPanel("BarPlot", plotOutput("barplot")),
                tabPanel("WorldPlot", plotOutput("worldplot"))
              )
              )
  ),
  hr()
)

authenticat()

#server
server <- function(input,output,session) {
  
  pr <- eventReactive(input$submitbutton,{
    withProgress({
    setProgress(message = "Fetching and Processing the tweets...")
    processTweets(input$searchbox,input$ntweets,input$isdate,input$daterange[1],input$daterange[2],input$stype) 
    })
  })
  
  output$wordcloud <- renderPlot({
    pr()
    wordcloud(t.text.corpus, min.freq=10, scale=c(2, 0.5), colors=brewer.pal(8, "Dark2"), random.color=TRUE, random.order=FALSE, max.words=40)
  })
  
  output$worldplot <- renderPlot({
    pr()
    
    #selected 200 locations for spatial plotting
    loc <- t.df$location
    
    #cleaned the invalid characters in location
    loc <- lapply(loc, function(x) iconv(x, "latin1", "ASCII", sub=""))
    loc <- sapply(loc, function(x) as.character(x))

    #get the longitude and latitude for the locations
    loca <- geocode(loc)
    
    #now call the function score.sentiment on tweets df
    scores <<- score.sentiment(t.df$text, pos, neg)
    
    #get scores for the valid geocodes
    s <- scores$score[!is.na(loca)]
    
    #remove NA from s and loca both
    s <- na.omit(s)
    loca <- na.omit(loca)
    
    #plotting world map with ggplot
    world <- ggplot() + borders("world", colour="gray50", fill="gray50")
    world.map <- world + geom_point(aes(x=loca$lon, y=loca$lat), color=as.numeric(s)+3, size=as.numeric(s)+3)
  })
}
  
# Run the application 
shinyApp(ui = ui, server = server)