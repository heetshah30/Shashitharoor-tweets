install.packages("twitteR")
install.packages("ROAuth")
library("twitteR")
library("ROAuth")
download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")
cred <- OAuthFactory$new(consumerKey='8vVRlgCOLVUwyOTVdDvuepuiY',
                         consumerSecret='hTXUxY4uwOkVzh6GFe8KLyx2c1X9QWX1iC0vGpmk1xo1Rexaax',
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')
cred$handshake(cainfo="cacert.pem")
save(cred, file="twitter authentication.Rdata")
load("twitter authentication.Rdata")
registerTwitterOAuth(cred)
?setup_twitter_oauth
setup_twitter_oauth(consumer_key='8vVRlgCOLVUwyOTVdDvuepuiY', consumer_secret='hTXUxY4uwOkVzh6GFe8KLyx2c1X9QWX1iC0vGpmk1xo1Rexaax', access_token='298013572-QN2qWkvBo3zVdJzB6J2UzPi3dm09y5pohRi5xwJp', access_secret='gPVf7XktWt2xjWTKO7u8o5UZzSJLwJhx5PE9nOBoP5Mmi')
search.string <- "from:ShashiTharoor"
no.of.tweets <- 10000
tweets <- searchTwitteR(search.string, n=no.of.tweets, lang="en")
install.packages("base64enc")

View(Opendata)
tweets
Opendata.text <- sapply(Opendata, function(x) x$getText())
#convert all text to lower case
Opendata <- tolower(Opendata)

# Replace blank space (???rt???)
Opendata <- gsub("rt", "", Opendata)

# Replace @UserName
tweets.text <- gsub("@\\w+", "", Opendata)

# Remove punctuation
Opendata <- gsub("[[:punct:]]", "", Opendata)

# Remove links
Opendata <- gsub("http\\w+", "", Opendata)

# Remove tabs
Opendata <- gsub("[ |\t]{2,}", "", Opendata)

# Remove blank spaces at the beginning
Opendata <- gsub("^ ", "", Opendata)

# Remove blank spaces at the end
Opendata <- gsub(" $", "", Opendata)
Opendata <- sapply(Opendata,function(row) iconv(row, "latin1", "ASCII", sub=""))
install.packages("tm")
library("tm")
#create corpus
Opendata.corpus <- Corpus(VectorSource(Opendata))

#clean up by removing stop words
Opendata.corpus <- tm_map(Opendata.corpus, function(x)removeWords(x,stopwords()))
#install wordcloud if not already installed
install.packages("wordcloud")
library("wordcloud")

#generate wordcloud
wordcloud(Opendata.corpus, min.freq = 2, scale=c(2.5,0.5),colors=brewer.pal(8, "Dark2"),  random.color= FALSE, random.order = FALSE, max.words = 50)
warnings()
# define tdm as matrix
m = as.matrix(tweets.text.corpus)
# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE) 
# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)
search.string <- "@ShashiTharoor"
no.of.tweets <- 1000
Kerala <- searchTwitter(search.string, n=no.of.tweets, lang="en", since="2015-11-01", until="2015-12-31")
install.packages("RCurl")
install.packages("RJSONIO")
install.packages("stringr")
library(RCurl)
library(RJSONIO)
library(stringr)
# Install packages for sentiment analysis
install.packages("ggplot2")
install.packages("plyr")
install.packages("gridExtra")
library("ggplot2")
library("plyr")
library("gridExtra")

tweets <- read.csv(file = "~/Desktop/Kerala.csv", stringsAsFactors = FALSE)
install.packages("ggplot2")
tweets <- tweets[, -1]
View(tweets)
class(tweets)
tweets.text <- tweets$text

install.packages("lubridate")
install.packages("scales")
library(ggplot2)
library(lubridate)
library(scales)
tweets <- read.csv(file = "~/Desktop/ShashiTharoor_tweets.csv", stringsAsFactors = FALSE)
tweets$created_at <- ymd_hms(tweets$created_at)
tweets$created_at <- with_tz(tweets$created_at, "Asia/Calcutta")
ggplot(data = tweets, aes(x = created_at)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Time") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

tweets.text <- tweets$text
# remove retweet entities
tweets.text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets.text)
# remove at people
tweets.text = gsub("@\\w+", "", tweets.text)
# remove punctuation
tweets.text = gsub("[[:punct:]]", "", tweets.text)
# remove numbers
tweets.text = gsub("[[:digit:]]", "", tweets.text)
# remove html links
tweets.text = gsub("http\\w+", "", tweets.text)
# remove unnecessary spaces
tweets.text = gsub("[ \t]{2,}", "", tweets.text)
tweets.text = gsub("^\\s+|\\s+$", "", tweets.text)

# define "tolower error handling" function 
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}
# lower case using try.error with sapply 
tweets.text = sapply(tweets.text, try.error)

# remove NAs in some_txt
some_txt = some_txt[!is.na(some_txt)]
names(some_txt) = NULL

library(sentiment)
# classify emotion
class_emo = classify_emotion(tweets.text, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol = classify_polarity(tweets.text, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]
# data frame with results
sent_df = data.frame(text=tweets.text, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
library(twitteR)
library(sentiment)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)

# plot distribution of emotions
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="number of tweets") +
  theme(title = "Sentiment Analysis of Tweets by Shashi Tharoor(classification by emotion)", element_text(size = 12))

