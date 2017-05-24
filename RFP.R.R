library(tm)
library(tau)
library(plyr)
library(wordcloud)
library(SnowballC)
library(caret)
library(rpart)
library(kernlab)
library(party)
library(rminer)
library(randomForest)

hss_data <- read.csv("c:/r/hss_data2.csv", header = T, stringsAsFactors = F)

str(hss_data)

hss_data$Answer <- factor(hss_data$Answer)

summary(hss_data)

prop.table(table(hss_data$Answer))

set.seed(100)

inTrain <- createDataPartition(y=hss_data$Answer,p=0.6,list=FALSE)

train_data <- hss_data[inTrain,]

test_data <- hss_data[-inTrain,]

summary(test_data)

inTest <- createDataPartition(y=test_data$Answer,p=0.5,list=FALSE)

test_data1 <- test_data[inTest,]
test_data2 <- test_data[-inTest,]

str(test_data1)
train_corpus <- Corpus(VectorSource(train_data$Question))

inspect(train_corpus)

mytd <- DocumentTermMatrix(train_corpus, 
        control = list(removeNumbers = T, removePunctuation=T, stripWhitespace = T, tolower = T, stopwords=T, streaming = T, PlainTextDocument=T))

inspect(mytd)

train_corpus_m <- as.matrix(mytd)

FFT <- findFreqTerms(mytd)

dim(train_corpus_m)

inspect(train_corpus[1:4])

mean_train <- sort(colMeans(as.matrix(mytd)),decreasing = T)

mean_train[1:20]

#average frequency of top 20 words
avg20 <- mean(mean_train[1:20])

avg20

barplot(mean_train[1:20],border = NA,
        xlab = "Top 20 words", ylab = "frequency", ylim = c(0,1))

#compute the average frequency if zeros are not counted in averaging
myTDM_nonNA <- as.matrix(mytd)

dim(myTDM_nonNA)

myTDM_nonNA

is.na(myTDM_nonNA)<- 0

dim(myTDM_nonNA)

mean_train <- sort(colMeans(as.matrix(myTDM_nonNA,na.rm = TRUE)),decreasing = T)

dim(mean_train)

#average frequency of top 20 words
avg20 <- mean(mean_train[1:20])

avg20

barplot(mean_train[1:20],border = NA,
        xlab = "Top 20 words", ylab = "frequency", ylim = c(0,1))


#Remove unwanted words
mystopwrds <- c("null")
inspect(train_corpus_m[1:17])

#train_corpus_2 <- tm_map(train_corpus,removeWords,mystopwrds)

#myTDM2 <- DocumentTermMatrix(train_corpus_2[1:17])

#dim(myTDM2)

mean_train2 <- sort(colMeans(as.matrix(mytd)),decreasing = T)

mean_train2[1:20]

#average frequency of top 20 words
avg20 <- mean(mean_train2[1:20])

avg20

barplot(mean_train2[1:20],border = NA,
        xlab = "Top 20 words", ylab = "frequency", ylim = c(0,1))


#Word cloud for first 20 words

wordcloud(names(mean_train2[1:20]),mean_train2[1:20],scale=c(5,1),
                colors= brewer.pal(8,"Dark2"))


# classification using different methods 

train_BowFreq <- as.matrix(train_corpus_m)

train_BowFreq <- as.data.frame(train_BowFreq)

train_BowFreq

train_data_f <- data.frame(y = train_data$Answer, x= train_BowFreq)

dim(train_data_f)

# Test data processing

test_corpus1 <- Corpus(VectorSource(test_data1$Question))

mytdm_t1 <- DocumentTermMatrix(test_corpus1, control = list(removeNumbers = T,
    removePunctuation=T, stripWhitespace = T, tolower = T, stopwords=T, 
    streaming = T, PlainTextDocument=T, dictionary =FFT ))

FFT
dim(mytdm_t1)

test_tdm1 <- as.matrix(mytdm_t1)

test_data1_f <- data.frame(y= test_data1$Answer,x= test_tdm1 )


str(test_data1_f)
dim(test_data1_f)
dim(train_data_f)

test_data1_f

# ctree classifier

bow_ctree <- ctree(y ~., data =train_data_f )

summary(bow_ctree)
plot(bow_ctree, type = 'simple')

test1pred <- predict(bow_ctree,newdata=test_data1_f)

#Evaluate prediction
confusionMatrix(test1pred, test_data1_f[,1], positive = 'Yes', dnn = c("Prediction","True"))

mmetric(test1pred,test_data1_f[,1],c("ACC","TPR","PRECISION","F1"))

# Naive Bayes classifier

bow_nb <- naiveBayes(y ~., data =train_data_f )
summary(bow_nb)
#plot(bow_nb, type = 'simple')
test1pred <- predict(bow_nb,newdata=test_data1_f)
#Evaluate prediction
confusionMatrix(test1pred, test_data1_f[,1], positive = 'Yes', dnn = c("Prediction","True"))
mmetric(test1pred,test_data1_f[,1],c("ACC","TPR","PRECISION","F1"))

# ksvm classifier

bow_ksvm <- ksvm(y ~., data =train_data_f )
summary(bow_ksvm)
#plot(bow_ksvm, type = 'simple')
test1pred <- predict(bow_ksvm,newdata=test_data1_f)
#Evaluate prediction
confusionMatrix(test1pred, test_data1_f[,1], positive = 'Yes', dnn = c("Prediction","True"))
mmetric(test1pred,test_data1_f[,1],c("ACC","TPR","PRECISION","F1"))

# random forest
output.forest <- randomForest(y ~ ., data = train_data_f)

# View the forest results.
print(output.forest) 
