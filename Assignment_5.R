length(stopwords("english"))
str(tweetsSparse)


findFreqTerms(frequencies, lowfreq=100)

tweetGLM = glm(Negative ~ ., data=trainSparse, family="binomial")
predictions = predict(tweetGLM, newdata=testSparse, type="response")

table(testSparse$Negative, predictions>.5)

(253+32)/nrow(testSparse)

table(train$responsive)/nrow(train)
table(test$responsive)/nrow(test)

wiki = read.csv("wiki.csv", stringsAsFactors=FALSE)
str(wiki)
table(wiki$Vandal)

# Load tm package
library(tm)

# Create corpus
corpusAdded = Corpus(VectorSource(wiki$Added))
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded = tm_map(corpusAdded, stemDocument)
dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded
sparseAdded = removeSparseTerms(dtm, 0.997)
sparseAdded

wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))


# Create corpus
corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, stemDocument)
dtmRemoved = DocumentTermMatrix(corpusRemoved)
dtmRemoved
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
sparseRemoved

wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))

wikiWords = cbind(wordsAdded, wordsRemoved)

wikiWords$Vandal = wiki$Vandal

summary(wikiWords)

library(caTools)
set.seed(123)

spl = sample.split(wikiWords$Vandal, 0.7)

wikiTrain = subset(wikiWords, spl == TRUE)
wikiTest = subset(wikiWords, spl == FALSE)

table(wikiTest$Vandal)/nrow(wikiTest)


library(rpart)
library(rpart.plot)

wikiCART = rpart(Vandal~., data=wikiTrain, method="class")

# Make predictions on the test set

predVandal = predict(wikiCART, newdata=wikiTest)
predVandal[1:10,]
predVandal.prob = predVandal[,2]

# Compute accuracy
table(wikiTest$Vandal, predVandal.prob >= 0.5)

(618+12)/nrow(wikiTest)

wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
table(wikiWords2$HTTP)

wikiTrain2 = subset(wikiWords2, spl==TRUE)
wikiTest2 = subset(wikiWords2, spl==FALSE)

wikiCART2 = rpart(Vandal~., data=wikiTrain2, method="class")

# Make predictions on the test set

predVandal2 = predict(wikiCART2, newdata=wikiTest2)
predVandal2[1:10,]
predVandal2.prob = predVandal2[,2]

# Compute accuracy
table(wikiTest2$Vandal, predVandal2.prob >= 0.5)
(609+57)/nrow(wikiTest2)

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))

mean(wikiWords2$NumWordsAdded)

wikiTrain3 = subset(wikiWords2, spl==TRUE)
wikiTest3 = subset(wikiWords2, spl==FALSE)

wikiCART3 = rpart(Vandal~., data=wikiTrain3, method="class")

# Make predictions on the test set

predVandal3 = predict(wikiCART3, newdata=wikiTest3)
predVandal3[1:10,]
predVandal3.prob = predVandal3[,2]

# Compute accuracy
table(wikiTest3$Vandal, predVandal3.prob >= 0.5)
(514+248)/nrow(wikiTest3)

wikiWords3 = wikiWords2

wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

wikiTrain4 = subset(wikiWords3, spl==TRUE)
wikiTest4 = subset(wikiWords3, spl==FALSE)

wikiCART4 = rpart(Vandal~., data=wikiTrain4, method="class")

# Make predictions on the test set

predVandal4 = predict(wikiCART4, newdata=wikiTest4)
predVandal4[1:10,]
predVandal4.prob = predVandal4[,2]

# Compute accuracy
table(wikiTest4$Vandal, predVandal4.prob >= 0.5)
(595+241)/nrow(wikiTest4)

prp(wikiCART4)



trials = read.csv("clinical_trial.csv", stringsAsFactors=FALSE)
str(trials)
summary(trials)

max(nchar(trials$abstract))
table(nchar(trials$abstract)==0)
trials$title[which.min(nchar(trials$title))]



corpusTitle = Corpus(VectorSource(trials$title))
corpusTitle = tm_map(corpusTitle, tolower)
corpusAbstract = Corpus(VectorSource(trials$abstract))
corpusAbstract = tm_map(corpusAbstract, tolower)
corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)
corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))
corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)
dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)
dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))
str(dtmTitle)
str(dtmAbstract)

colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))
dtm = cbind(dtmTitle, dtmAbstract)

dtm$trial = trials$trial
str(dtm)

library(caTools)
set.seed(144)

spl = sample.split(dtm$trial, 0.7)

train = subset(dtm, spl == TRUE)
test = subset(dtm, spl == FALSE)

table(train$trial)/nrow(train)


trialCART = rpart(trial~., data=train, method="class")
prp(trialCART)

predTrialTrain = predict(trialCART)
predTrialTrain[1:10,]
predTrialTrain.prob = predTrialTrain[,2]
max(predTrialTrain.prob)

table(train$trial, predTrialTrain.prob >= 0.5)
(631+441)/nrow(train)
441/(131+441)
631/(631+99)


predTrialTest = predict(trialCART, newdata=test)
predTrialTest[1:10,]
predTrialTest.prob = predTrialTest[,2]

# Compute accuracy
table(test$trial, predTrialTest.prob >= 0.5)
(261+162)/nrow(test)


library(ROCR)
ROCRpred = prediction(predTrialTest.prob, test$trial)
as.numeric(performance(ROCRpred, "auc")@y.values)


table(test$trial, predTrialTest.prob >= 0.5)
table(test$trial, predTrialTest.prob >= 0.2)
table(test$trial, predTrialTest.prob >= 0.8)



emails = read.csv("emails.csv", stringsAsFactors=FALSE)
str(emails)
table(emails$spam)

max(nchar(emails$text))
which.min(nchar(emails$text))

library(tm)
corpus = Corpus(VectorSource(emails$text))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)
str(dtm)
spdtm = removeSparseTerms(dtm, 0.95)
str(spdtm)
emailsSparse = as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))

which.max(colSums(emailsSparse))

emailsSparse$spam=emails$spam

sort(colSums(subset(emailsSparse, spam == 0)))
sort(colSums(subset(emailsSparse, spam == 1)))

emailsSparse$spam = as.factor(emailsSparse$spam)
library(caTools)
set.seed(123)

spl = sample.split(emailsSparse$spam, 0.7)

train = subset(emailsSparse, spl == TRUE)
test = subset(emailsSparse, spl == FALSE)


spamLog = glm(spam~.,data=train,family="binomial")

library(rpart)
library(rpart.plot)
spamCART = rpart(spam~., data=train, method="class")

library(randomForest)
library(caTools)
set.seed(123)
spamRF = randomForest(spam ~ ., data = train)

PredictLog = predict(spamLog,type="response")
PredictCART = predict(spamCART)[,2]
PredictRF = predict(spamRF,type="prob")[,2]

table(PredictLog<0.00001)
table(PredictLog>0.99999)
table(PredictLog>=0.00001 & PredictLog<0.99999)

summary(spamLog)
prp(spamCART)

table(train$spam, PredictLog >= 0.5)
(3052+954)/nrow(train)

library(ROCR)
ROCRpred = prediction(PredictLog, train$spam)
as.numeric(performance(ROCRpred, "auc")@y.values)

table(train$spam, PredictCART >= 0.5)
(2885+894)/nrow(train)

library(ROCR)
ROCRpred = prediction(PredictCART, train$spam)
as.numeric(performance(ROCRpred, "auc")@y.values)

table(train$spam, PredictRF >= 0.5)
(3013+914)/nrow(train)

library(ROCR)
ROCRpred = prediction(PredictRF, train$spam)
as.numeric(performance(ROCRpred, "auc")@y.values)


PredictLog = predict(spamLog,newdata=test,type="response")
PredictCART = predict(spamCART,newdata=test)[,2]
PredictRF = predict(spamRF,newdata=test,type="prob")[,2]


table(test$spam, PredictLog >= 0.5)
(1257+376)/nrow(test)

library(ROCR)
ROCRpred = prediction(PredictLog, test$spam)
as.numeric(performance(ROCRpred, "auc")@y.values)

table(test$spam, PredictCART >= 0.5)
(1228+386)/nrow(test)

library(ROCR)
ROCRpred = prediction(PredictCART, test$spam)
as.numeric(performance(ROCRpred, "auc")@y.values)

table(test$spam, PredictRF >= 0.5)
(1290+386)/nrow(test)

library(ROCR)
ROCRpred = prediction(PredictRF, test$spam)
as.numeric(performance(ROCRpred, "auc")@y.values)


wordCount = rowSums(as.matrix(dtm))
hist(wordCount)
hist(log(wordCount))

emailsSparse$logWordCount=log(wordCount)


boxplot(emailsSparse$logWordCount~emailsSparse$spam)

train2 = subset(emailsSparse, spl == TRUE)
test2 = subset(emailsSparse, spl == FALSE)

library(rpart)
library(rpart.plot)
spam2CART = rpart(spam~., data=train2, method="class")

library(randomForest)
library(caTools)
set.seed(123)
spam2RF = randomForest(spam ~ ., data = train2)

prp(spam2CART)

Predict2CART = predict(spam2CART,newdata=test2)[,2]
Predict2RF = predict(spam2RF,newdata=test2,type="prob")[,2]

table(test$spam, Predict2CART >= 0.5)
(1214+384)/nrow(test2)

library(ROCR)
ROCRpred = prediction(Predict2CART, test$spam)
as.numeric(performance(ROCRpred, "auc")@y.values)

table(test$spam, Predict2RF >= 0.5)
(1296+383)/nrow(test2)

library(ROCR)
ROCRpred = prediction(Predict2RF, test$spam)
as.numeric(performance(ROCRpred, "auc")@y.values)
