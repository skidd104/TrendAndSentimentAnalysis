
#Sentiment Analysis
library (twitteR)

# library (data.table)
library(tm)
library(ggplot2)
library (stringr)

#install.packages("syuzhet")
library (syuzhet)
library(wordcloud)
library (RColorBrewer)


#set the credentials
Consumer_secret <- ""
Consumer_key <- ""
Access_secret <- ""
Access_token <- ""

#extract timeline tweets
extractTimelineTweets <- function (username, tweetCount){
  #timeline tweets
  twitterUser <- getUser(username)
  tweets = userTimeline(twitterUser, n=tweetCount)
  tweets.df = twListToDF(tweets)
  tweets.df$text <- sapply (tweets.df$text, function(x) iconv(enc2utf8(x), sub = "byte"))
  
  return (tweets.df)
}

encodeSentiment <- function(x) {
  if(x <= -0.5){
    "1) very negative"
  }else if(x > -0.5 & x < 0){
    "2) negative"
  }else if(x > 0 & x < 0.5){
    "4) positive"
  }else if(x >= 0.5){
    "5) very positive"
  }else {
    "3) neutral"
  }
}


#connect to twitter app
setup_twitter_oauth(consumer_key = Consumer_key,
                    consumer_secret = Consumer_secret,
                    access_token = Access_token,
                    access_secret = Access_secret
                    )




tweetsDF <- extractTimelineTweets ("Resident_bio", 3200)
View (tweetsDF)
head (tweetsDF$text)# dont include in rmarkdown

#cleaning the data
nohandles <- str_replace_all(tweetsDF$text, "@\\w+", " ")
nohandles$cleanedText <- gsub("http.*", " ", nohandles)
nohandles$cleanedText <- gsub("https.*", " ", nohandles$cleanedText )
head(nohandles$cleanedText) #dont include

nohandles$cleanedText <- str_replace_all(nohandles$cleanedText, "[^[:alnum:]]", " ")
nohandles$cleanedText <-  str_replace_all(nohandles$cleanedText,"[[^a-zA-Z0-9]]", " ")
head(nohandles$cleanedText)


#Wordcloud
wordCorpus <- Corpus(VectorSource(nohandles$cleanedText))
wordCorpus[[1]]$content

#removing punctuations
wordCorpus <- tm_map(wordCorpus, removePunctuation)
wordCorpus[[1]]$content
wordCorpus <- tm_map(wordCorpus, removeNumbers)
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
wordCorpus[[1]]$content
wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english"))
wordCorpus[[1]]$content
wordCorpus <- tm_map(wordCorpus, removeWords, c("amp"))
wordCorpus <- tm_map(wordCorpus, removeWords, c("ste"))#manual assignment
wordCorpus[[5]]$content
wordCorpus <- tm_map(wordCorpus, stripWhitespace)
wordCorpus[[1]]$content
str(wordCorpus)

pal <- brewer.pal(9,"YlGnBu")
pal <- pal[-(1:4)]
set.seed(123)
par(mfrow = c(1,1))
wordcloud(words = wordCorpus, scale=c(5,0.1), max.words=500, random.order=FALSE, 
          rot.per=0.35, use.r.layout=FALSE, colors=pal)

#sentiment analysis
tweetSentiments <- get_sentiment (tweetsDF$text,method = "syuzhet")
tweets <- cbind(tweetsDF, tweetSentiments)

#positive and negative sentiments
tweets$sentiment <- sapply(tweets$tweetSentiments,encodeSentiment)
head(tweets, n = 1)


qplot(tweets$tweetSentiments) + theme(legend.position="none")+
  xlab("Sentiment Score") +
  ylab("Number of tweets") + 
  ggtitle("Tweets by Sentiment Score") 


ggplot(tweets, aes(sentiment, fill = sentiment)) +
  geom_bar() + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  ylab("Number of tweets") + 
  ggtitle("Tweets by Sentiment") 

# NRC Sample (various emotions such as anger, fear, joy, ...)
tweetSentiments <- get_nrc_sentiment(tweetsDF$text)
head(tweetSentiments)
#viewing of the tweets
tweetsDF$text[6]

tweets <- cbind(tweetsDF, tweetSentiments)
tweets[c(1:5),c(1, 17:26)] #remove in rmarkdown

sentimentTotals <- data.frame(colSums(tweets[,c(17:26)]))

names(sentimentTotals) <- "count"
sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
rownames(sentimentTotals) <- NULL
sentimentTotals


ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for All Tweets")
