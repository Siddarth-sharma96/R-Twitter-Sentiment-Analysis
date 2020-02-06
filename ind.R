# package twitteR
install.packages("twitteR")
library("twitteR")
install.packages(c('ROAuth','RCurl'))
require('ROAuth')
require('RCurl')
library(twitteR)
library(httr)
library(RCurl)
library(ROAuth)
setwd("C:/Users/imsid/OneDrive/Desktop/apni duniya/mem/da")
key = "ksJ9FZ06ZXJDa8tZpWhUQKDrS"
secret = "di4n5pA6QEvDNFaANhgQv5E2Wfrvqtbl6ECQRjRqnBZQu7zhRj"
token = "3259188055-NqgHhLf2W1MRfki5cFyqATfooVzT0QBYWGEE1gh"
tokensecret= "Ch9RyM1Tf6uTueFPnFB5ys8N2F8p1ewM5E75sP6vIx6iH"
library("twitteR")
library("httr")
setup_twitter_oauth(key,secret,token,tokensecret)
udemytweets=searchTwitter("#CAB", n=3500)#,geocode = "28.613889,77.208889,500mi")

df=do.call("rbind", lapply(udemytweets,as.data.frame))

install.packages("tm")
install.packages("tmap")
library("tmap")
library("tm")

#install.packages("CRAN")
list=sapply(udemytweets,function(x)x$getText())
udemycorpus=Corpus(VectorSource(list))
udemycorpus <- tm_map(udemycorpus, function(x) iconv(x, to='UTF-8', sub='byte'))

install.packages("SnowballC")
require(SnowballC)

udemycorpus=tm_map(udemycorpus,tolower)
udemycorpus=tm_map(udemycorpus, removePunctuation)
udemycorpus=tm_map(udemycorpus, removeNumbers)
udemycorpus=tm_map(udemycorpus, stripWhitespace)
#stop words are useless words like and, the etc
udemycorpus=tm_map(udemycorpus, function(x)removeWords(x,stopwords()))
udemycorpus= tm_map(udemycorpus,stemDocument)
udemycorpus <- Corpus(VectorSource(udemycorpus))

install.packages("RSentiment")
install.packages("rJava")

dtm_up<- DocumentTermMatrix(VCorpus(VectorSource(udemycorpus[[1]]$content)))
freq_up=colSums(as.matrix(dtm_up))
require(RSentiment)

sentiments_up=calculate_sentiment(names(freq_up))
sentiments_up=cbind(sentiments_up,as.data.frame(freq_up))
sent_pos_up=sentiments_up[sentiments_up$sentiment=="Positive",]
sent_neg_up=sentiments_up[sentiments_up$sentiment=="Negative",]
cat("Negative sentiments: ",sum(sent_neg_up$freq_up),"Positive sentiments: ",sum(sent_pos_up$freq_up))

#_____________________WORDCLOUD__________________________________________________________


install.packages("wordcloud2")
update.packages("RColorBrewer")
library(RColorBrewer)
library(wordcloud2)

sent_neg_up2=sent_neg_up
sent_pos_up2=sent_pos_up

sent_neg_up2$sentiment <- NULL
sent_pos_up2$sentiment <- NULL

#wordcloud for negative sentiments
wordcloud2(data=sent_neg_up2, size = 3.3,color = rep_len( c("#ec3390","#4bff2f","blue","red","yellow") ,
nrow(sent_neg_up2) ), backgroundColor="black",
minRotation = -pi/6, maxRotation = -pi/6, rotateRatio = 0, shape="star") 

#wordcloud for positive sentiments
wordcloud2(data=sent_pos_up2, size = 27.5,color = rep_len( c("yellow","red","blue","#ec3390","#4bff2f") ,
nrow(sent_pos_up2) ), backgroundColor="black",
minRotation = -pi/6, maxRotation = -pi/6, rotateRatio = 0) 

#_______________Exploring the emotions  in the tweets_____________________________________________________________________________________________________________________

#sentiments in the negative tweets
texts=as.character(sent_neg_up2$text)

install.packages("syuzhet")
library(syuzhet)

tweetsentiment=get_nrc_sentiment(texts)

barplot(
  sort(colSums(prop.table(tweetsentiment[,1:8]))),cex.names = 0.7,las=1,
  main = "Emotions in the tweets againt #CAA"
)

#sentiments in the positive tweets
texts2=as.character(sent_pos_up2$text)

tweetsentiment2=get_nrc_sentiment(texts2)

barplot(
  sort(colSums(prop.table(tweetsentiment2[,1:8]))),cex.names = 0.7,las=1,
  main = "Emotions in the tweets in support of #CAA"
)
#_______________________________________________________________________________



pos=readLines("Positive-Words.txt")
neg=readLines("negative_words.txt")


library("stringr")
library("plyr")

#creating a function to calculate the sentiments from each tweet, to classify words into positive and negative 

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  # Parameters
  # sentences: vector of text to score
  # pos.words: vector of words of postive sentiment
  # neg.words: vector of words of negative sentiment
  # .progress: passed to laply() to control of progress bar
  
  # create simple array of scores with laply
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   # remove punctuation - using global substitute, gsub means global subsitution with blank space
                   sentence = gsub("[[:punct:]]", "", sentence)
                   # remove control characters
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   # remove digits
                   sentence = gsub('\\d+', '', sentence)
                   
                   # define error handling function when trying tolower
                   tryTolower = function(x)
                   {
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     return(y)
                   }
                   # use tryTolower with sapply 
                   sentence = sapply(sentence, tryTolower)
                   
                   # split sentence into words with str_split (stringr package)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list) # converting string to vector
                   
                   # compare words to the dictionaries of positive & negative terms
                   pos.matches = match(words, pos.words)#matching the vector to positive words
                   neg.matches = match(words, neg.words)
                   
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   
                   # final score
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words, .progress=.progress )
  
  # data frame with scores for each sentence
  scores.df = data.frame(text=sentences, score=scores)
  return(scores.df)
}
                     
#testing the function                     
mytest= c("great you are here", "awesome experience", 
          "You had a bad night", "She loves ugly candy")
# the score.sentiment function is self written
testsentiment = score.sentiment(mytest, pos, neg)
testsentiment

#gathering tweets from four major cities of India_________________________________

delhitweets = searchTwitter("#CAA",geocode="28.613889,77.208889,500mi", n=1000, lang="en")#, cainfo="cacert.pem")
mumbaitweets = searchTwitter("#CAA",geocode="18.975,72.825833,500mi", n=1000, lang="en")#, cainfo="cacert.pem")
bangloretweets = searchTwitter("#CAA", geocode="12.983333,77.583333,500mi",n=900, lang="en")#, cainfo="cacert.pem")
kolkatatweets = searchTwitter("#CAA",geocode="22.5726,88.3639,500mi", n=900, lang="en")#, cainfo="cacert.pem")

#Calculating sentiment scores for tweets from each City________________________

delhi_txt = sapply(delhitweets, function(x) x$getText())
mumbai_txt = sapply(mumbaitweets, function(x) x$getText())
banglore_txt = sapply(bangloretweets, function(x) x$getText())
kolkata_txt = sapply(kolkatatweets, function(x) x$getText())

# how many tweets of each city__________________________________________
nd = c(length(delhi_txt), length(mumbai_txt), length(banglore_txt), length(kolkata_txt))
# join texts
country = c(delhi_txt, mumbai_txt, banglore_txt, kolkata_txt) 
# apply function score.sentiment
scores = score.sentiment(country, pos, neg, .progress='text')

# add variables to data frame
scores$country = factor(rep(c("Kolkata", "Mumbai", "Banglore", "delhi"), nd))
scores$very.pos = as.numeric(scores$score >= 4)
scores$very.neg = as.numeric(scores$score <= 3)

# how many very positives and very negatives
numpos = sum(scores$very.pos)
numneg = sum(scores$very.neg)

#global score
global_score = round( 100 * numpos / (numpos + numneg) )
head(scores)
boxplot(score~country, data=scores)
library("lattice")

histogram(data=scores, ~score|country, main="Sentiment Analysis of 4 Countries", xlab="", sub="Sentiment Score")




