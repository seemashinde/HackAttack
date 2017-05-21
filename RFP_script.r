library(tm)
library(tau)
library(plyr)
library(wordcloud)
library(SnowballC)

hss_data <- read.csv("c:/r/hss_data.csv")

names(hss_data)

head(hss_data)

mycorpus <- Corpus(VectorSource(hss_data$Question))

mycorpus

inspect(mycorpus[1:4])

mycorpus<-tm_map(mycorpus,tolower)
mycorpus<-tm_map(mycorpus,removePunctuation)
mycorpus<-tm_map(mycorpus,removeNumbers)
mycorpus<-tm_map(mycorpus,removeWords,stopwords(kind="English"))
stopwords(kind="English")
mycorpus<-tm_map(mycorpus,stripWhitespace)
mycorpus<-tm_map(mycorpus,PlainTextDocument)

#stem document
dictCorpus<-mycorpus
mycorpus<-tm_map(mycorpus,stemDocument)
mycorpus<-tm_map(mycorpus,stemCompletion,dictCorpus)

#inspect(mycorpus[1:17])


#build Term Document Matrix


myTDM<-TermDocumentMatrix(mycorpus[1], control=list(minWordLength=1))

myTDM <- DocumentTermMatrix(myTDM)

#myTDMdf<-as.data.frame(as.matrix(t(myTDM)))

myTDMdf<-as.matrix(mycorpus)

#dim(myTDMdf)

#find most frequent terms
findFreqTerms(myTDM,lowfreq=2)

inspect(mycorpus[1:10])

#sapply(mycorpus,'a',"content")

#n-grams analysis
mydf<-data.frame(text=unlist(sapply(mycorpus,'a',"content")),stringAsFactors=FALSE)
mydf$text<-as.character(mydf$text)

mydf$text

ngram_1L<-textcnt(mydf$text,n=1L,method="string")

ngram_t1<-data.frame(counts=unclass(ngram_1L),size=nchar(names(ngram_1L)),text=names(ngram_1L))

n1L<-arrange(ngram_t1,desc(counts))

counts1<-n1L[,c(3,1)]
View(counts1)

ngram_3L<-textcnt(mydf$text,n=3L,method="string")
ngram_t3<-data.frame(counts=unclass(ngram_3L),size=nchar(names(ngram_3L)),text=names(ngram_3L))
n3L<-arrange(ngram_t3,desc(counts))
counts3<-n3L[,c(3,1)]
View(counts3)

#finding the types of comments
#k-means clustering
#wss<-(nrow(myTDMdf))*sum(apply(myTDMdf,2,var))
#wss

#k<-kmeans(myTDMdf,1,nstart=2)
#k

#groups<-data.frame(k$cluster)
#groups
#table(groups)

#Question<-as.data.frame(hss_data[,"Question"])
#Question
#finalDF<-as.data.frame(cbind(Question,groups))
#names(finalDF)<-c("Question","group")

#finalDF

#x1<-subset(finalDF, group==2)
#View(x1)