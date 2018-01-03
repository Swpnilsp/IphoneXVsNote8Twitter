library(twitteR)
library(dplyr)
library(tm )
library(wordcloud)
library(tidytext)
library(tidyverse)


## iphoneX launch- Sep 12, 2017
## iphoneX sale- Nov 03, 2017
consumer_key<-'HjGiIoPNW4wa5oMRKDOUtDHdU'
consumer_secret<-'ifcwxyOLUPNsr1Rci1qkTaXAaCx5EGyZ0OxaFEAIUQyAQQcNFf'
access_token<-'81578669-CxBJMX8fHKVy8RBTZK5gHKojx3HBQTiVFimTdLMEl'
access_token_secret<-'H9gNZgVWEzqFxAGoU978wXWGceZdGmYMHQQ2mtS3wPlYS'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_token_secret)
iphonex = twitteR::searchTwitter('#iphonex -filter:retweets',lang = "en", n = 10000, since = '2017-12-20',
                            until = '2017-12-30',retryOnRateLimit = 1)
d = twitteR::twListToDF(iphonex)
write.csv(d,"/Users/swapnilpatil/Study/MS-Bana/Projects/Iphonex vs Galaxy/iphone.csv")

note8 = twitteR::searchTwitter('#note8 -filter:retweets',lang = "en", n = 10000, since = '2017-12-20',
                               until = '2017-12-30',retryOnRateLimit = 1)
d = twitteR::twListToDF(note8)
write.csv(d,"/Users/swapnilpatil/Study/MS-Bana/Projects/Iphonex vs Galaxy/note8.csv")


iphonex<-read.csv("/Users/swapnilpatil/Study/MS-Bana/Projects/Iphonex vs Galaxy/iphone.csv",fileEncoding="latin1")
note8<-read.csv("/Users/swapnilpatil/Study/MS-Bana/Projects/Iphonex vs Galaxy/note8.csv",fileEncoding="latin1")


# for now, we will just concentrate on the text of tweets
iphonex<-iphonex$text
note8<-note8$text
iphonex<-as.character(iphonex)
note8<-as.character(note8)


# Text tranformations- removing whitespaces, commong stop words,punctuations 

iphonex<-stripWhitespace(iphonex)
## this will remove all other characters except $ sign
iphonex<-gsub("[^[:alnum:][:space:]$]", "", iphonex)
iphonex<-tolower(iphonex)
iphonex<-removeWords(iphonex, c(stopwords("english"),'apple','iphone','iphonex','mobile','phone','ampamp',
                                'iphone8','retweet','just','comment','amp'))



#iphonex<-unlist(strsplit(iphonex, split = ' '))
#iphonex<-stemDocument(iphonex,language = "english")

note8<-stripWhitespace(note8)
note8<-gsub("[^[:alnum:][:space:]$]", "", note8)
note8<-tolower(note8)
note8<-removeWords(note8, c(stopwords("english"),'samsung','note8','galaxy','mobile','phone','samsungmobile',
                            'amp','just','ampamp'))

#note8<-unlist(strsplit(note8, split = ' '))

# converting to vector
iphoneTweets<-VectorSource(iphonex)
note8Tweets<-VectorSource(note8)

# converting verctor source to Volatile Corpus, which is a nested list ($content and $meta). This helps extracting individual tweets 
# for example, note8Tweets[[20]][1] will retreive 20th tweet and note8Tweets[[20]][2] will retreive metadata of 20th

iphoneTweets<-VCorpus(iphoneTweets)
note8Tweets<-VCorpus(note8Tweets)


# Creating document term  matrix
iphone_dtm<-DocumentTermMatrix(iphoneTweets)
note8_dtm<-DocumentTermMatrix(note8Tweets)
iphone_m<-as.matrix(iphone_dtm)
note8_m<-as.matrix(note8_dtm)
dim(iphone_m)
dim(note8_m)

## getting the word frequencies
iphone_wf<-colSums(iphone_m)
iphone_wf<-sort(iphone_wf,decreasing = TRUE)
note8_wf<-colSums(note8_m)
note8_wf<-sort(note8_wf,decreasing = TRUE)

#Plotting most frequent words
barplot(iphone_wf[1:20],col='red',las=2,main = 'Iphone')
barplot(note8_wf[1:20],col='red',las=2,main = 'Note8')

wordcloud(names(iphone_wf),iphone_wf,colors = c("grey80", "darkgoldenrod1", "tomato"),max.words = 100)
wordcloud(names(note8_wf),note8_wf,colors = c("grey80", "darkgoldenrod1", "tomato"),max.words = 100)


## common words in both iphonex and note8 related tweets
iphonex<-read.csv("/Users/swapnilpatil/Study/MS-Bana/Projects/Iphonex vs Galaxy/iphone.csv",fileEncoding="latin1")
note8<-read.csv("/Users/swapnilpatil/Study/MS-Bana/Projects/Iphonex vs Galaxy/note8.csv",fileEncoding="latin1")
all_iphone<-paste(iphonex$text,collapse = '')
all_note8<-paste(note8$text,collapse = '')
all_tweets<-c(all_iphone,all_note8)
all_tweets<-stripWhitespace(all_tweets)
all_tweets<-gsub("[^[:alnum:][:space:]$]", "", all_tweets)
all_tweets<-tolower(all_tweets)
all_tweets<-removeWords(all_tweets, c(stopwords("english"),'mobile','phone','amp','ampamp'))

all_tweets<-VectorSource(all_tweets)
all_tweets<-VCorpus(all_tweets)

all_tweets_tdm<-TermDocumentMatrix(all_tweets)
all_tweets_m<-as.matrix(all_tweets_tdm)
commonality.cloud(all_tweets_m,colors = c("grey80", "darkgoldenrod1", "tomato"),max.words = 50)


## Sentiment analysis
words<-colnames(iphone_m)
count<-colSums(iphone_m)
note.sentiment$words<-words
note.sentiment$count<-count
note.sentiment<-as.data.frame(note.sentiment)
note.sentiment<-separate(note.sentiment,note.sentiment$word,sep = ' ')
iphone.sentiment<-as.data.frame(iphone_wf)



