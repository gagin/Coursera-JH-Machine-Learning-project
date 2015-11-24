# Machine Learning Coursera - Week 4 quiz
library(checkpoint)
checkpoint(snapshotDate = '2015-05-01')

library(caret)
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 


vtr<-vowel.train
vtr$y<-as.factor(vtr$y)
vts<-vowel.test
vts$y<-as.factor(vts$y)

set.seed(33833)
fit.rf<-train(y~.,data=vtr,method="rf")
confusionMatrix(predict(fit.rf,vts),vts$y)$overall[1]
fit.gbm<-train(y~.,data=vtr,method="gbm",verbose=FALSE)
confusionMatrix(predict(fit.gbm,vts),vts$y)$overall[1]
p.rf<-predict(fit.rf,vts)
p.gbm<-predict(fit.gbm,vts)
agreed<-p.rf==p.gbm
confusionMatrix(p.rf[agreed],vts[agreed,"y"])$overall[1]

# 0.6060606 
# 0.5108225
# 0.63125

print(sessionInfo())


################


library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)

f.rf <- train(diagnosis~.,data=training, method="rf")
p.rf <- predict(f.rf,testing)
confusionMatrix(p.rf,testing$diagnosis)$overall[1]
# 0.7804878 
f.gbm <- train(diagnosis~.,data=training, method="gbm", verbose=FALSE)
p.gbm <- predict(f.gbm,testing)
confusionMatrix(p.gbm,testing$diagnosis)$overall[1]
# 0.804878 
f.lda <- train(diagnosis~.,data=training, method="lda")
p.lda <- predict(f.lda,testing)
confusionMatrix(p.lda,testing$diagnosis)$overall[1]
# 0.7682927 

merged<-as.data.frame(training$diagnosis)
names(merged)<-"diagnosis"
merged$rf<-predict(f.rf,training)
merged$gbm<-predict(f.gbm,training)
merged$lda<-predict(f.lda,training)

f.e<-train(diagnosis~.,data=merged,method="rf")

merged2<-as.data.frame(testing$diagnosis)
names(merged2)<-"diagnosis"
merged2$rf<-predict(f.rf,testing)
merged2$gbm<-predict(f.gbm,testing)
merged2$lda<-predict(f.lda,testing)
confusionMatrix(predict(f.e,merged2),as.integer(testing$diagnosis))$overall[1]


