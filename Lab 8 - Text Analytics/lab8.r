rm(list = ls())
setwd("C:\\Users\\ramym\\Desktop\\SecondTerm\\BigData\\Lab 8 - Text Analytics")
#install.packages('tm')
#install.packages("wordcloud")
library('tm')
library("wordcloud")
#################################### 1 & 2 & 3 ##################
dfm <- read.csv("movie_reviews.csv")
head(dfm)
co<-Corpus(VectorSource(dfm$text))
############################### 4 ###################
co <- tm_map(co, tolower)
#################################### 5 ##################
co <- tm_map(co,removeNumbers)
co <- tm_map(co,removePunctuation)
co <- tm_map(co,stripWhitespace)
################################## 6 ##################
co <- tm_map(co, removeWords, stopwords("english"))
################################## 7 ##############
dtm <- DocumentTermMatrix(co)
################################ 8 ##################
inspect(dtm)
# sparisity is 100%
#############################
dtm <- removeSparseTerms(dtm, 0.9999)
inspect(dtm)
############################
freq<- findFreqTerms(dtm, 65)
freq
#########################3
findAssocs(dtm,terms= c("titanic",'marvel'), 0.05)
################################
dtm2 <- as.matrix(dtm)
frequency <- colSums(dtm2)
frequency<-sort(frequency,TRUE)
##############################
frequency[1:5]
##########################3###
words <- names(frequency)
wordcloud(words[1:100], frequency)
