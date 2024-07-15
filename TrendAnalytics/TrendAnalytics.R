library (twitteR)
library (openssl)
library (NLP)
library (tm)
library (stringr)
library (dplyr)
library (ggplot2)
library(wordcloud)
library (rtweet)
library (tidytext)
library (tidyverse)

CONSUMER_SECRET <- ""
CONSUMER_KEY <- ""
ACCESS_TOKEN <- ""
ACCESS_SECRET <- ""

setup_twitter_oauth(
  consumer_key = CONSUMER_KEY,
  consumer_secret = CONSUMER_SECRET,
  access_token = ACCESS_TOKEN,
  access_secret = ACCESS_SECRET
)

trendingTweets = searchTwitter("#ResidentEvil4Remake -filter:retweets",
                               n =1000, lang = "en",
                               since = "2022-06-10",
                               until = "2022-06-18",
                               retryOnRateLimit = 120)
str (trendingTweets[[1]])
#perform a quick clean up
trendingTweets.df = twListToDF(trendingTweets)
trendingTweets.df$text <- sapply(trendingTweets.df$text,function(x) iconv(enc2utf8(x), sub="byte"))
class (trendingTweets.df)
names(trendingTweets.df)
head(trendingTweets.df$text)

#remove emoji
nohandles <- str_replace_all(trendingTweets.df$text, "@\\w+", " ")
nohandles$cleanedText <- gsub("http.*", " ", nohandles)
nohandles$cleanedText <- gsub("https.*", " ", nohandles$cleanedText )
head(nohandles$cleanedText) #dont include

nohandles$cleanedText <- str_replace_all(nohandles$cleanedText, "[^[:alnum:]]", " ")
nohandles$cleanedText <-  str_replace_all(nohandles$cleanedText,"[[^a-zA-Z0-9]]", " ")
head(nohandles$cleanedText)


#encoding a function to check for the source of the text
encodeSource <- function(x) {
  if(grepl(">Twitter for iPhone</a>", x)){
    "iphone"
  }else if(grepl(">Twitter for iPad</a>", x)){
    "ipad"
  }else if(grepl(">Twitter for Android</a>", x)){
    "android"
  } else if(grepl(">Twitter Web Client</a>", x)){
    "Web"
  } else if(grepl(">Twitter for Windows Phone</a>", x)){
    "windows phone"
  }else if(grepl(">Facebook</a>", x)){ #This looks unreliable...
    "facebook"
  }else {
    "others"
  }
}
  
#using sapply () function to gives output in vector or matrix

head (trendingTweets.df$text)
head (trendingTweets.df$created)
class (trendingTweets.df$created)

sapply(trendingTweets.df, function (x) sum (is.na(x)))


#plotiing the data on a frame
#plot on tweets by time/date
ggplot (data = trendingTweets.df, aes (x = created)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab ("Time") + ylab ("Number of tweets") +
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

trendingTweets.df$created[1:5]

#ts_plot
ts_plot(trendingTweets.df, "hours") +
  labs(x = NULL, y = NULL,
       title = "Frequency of tweets with a #ResidentEvil4Remake",
       subtitle = paste0(format(min(trendingTweets.df$created), "%d %B %Y"), " to ", format(max(trendingTweets.df$created),"%d %B %Y")),
       caption = "Data collected from Twitter's REST API via rtweet") +
  theme_minimal()




trendingTweets.df$screenName
trendingTweets.df$tweetSource = sapply(trendingTweets.df$statusSource, encodeSource)
table(trendingTweets.df$statusSource)

table (trendingTweets.df$tweetSource)
#used for descriptive analysis
ggplot(
  trendingTweets.df[trendingTweets.df$tweetSource != "others",], aes(tweetSource)) +
    geom_bar (fill = "aquamarine4") +
    theme (legend.position = "none",
           axis.title.x = element_blank(),
           axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab ("Number of tweets") +
    ggtitle("Tweet by Source")

#gettiing the names to be place in vectors
namesCorpus <- Corpus(VectorSource(trendingTweets.df$screenName))
class(trendingTweets.df$screenName)

str (namesCorpus)

class (namesCorpus)

pal <- brewer.pal (8, "Dark2")
pal <- pal[-(1:4)]

set.seed(123)
par (mar = c (0,0,0,0), mfrow = c(1,1))


wordcloud(words = namesCorpus, scale = c(3,0.5),
          max.words = 100,
          random.order = FALSE,
          rot.per = 0.50,
          use.r.layout = TRUE,
          colors = pal,
          )


