setwd(file.path(normalizePath("~"),"kaggle","predmachlearn-034", "quiz4"))


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

# on August 1st snapshot
# 0.6060606 
# 0.5108225
# 0.63125

# On May 1st snapshot
# 0.6060606 
# 0.530303
# 0.6622951 
# GBM was still 2.1.1

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
# 0.804878 August
# 0.7926829  May
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
confusionMatrix(predict(f.e,merged2), testing$diagnosis)$overall[1]
# 0.7926829

############################## 3
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(233)

mod<-train(CompressiveStrength~.,data=training,method="lasso")
plot(mod$finalModel, xvar="penalty")
#Cement

######################## 4

library(lubridate)  # For year() function below
dat = read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv"))
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)

install.packages("forecast")
library(forecast)
?bats
ba <- bats(tstrain, use.parallel = FALSE)
fo <- forecast(ba, h=length(testing$date), level=95)
sum( fo$lower < testing$visitsTumblr & fo$upper > testing$visitsTumblr) / length(testing$visitsTumblr) 
# 0.9617021

########################### 5
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(325)
mo <- e1071::svm(CompressiveStrength~.,data=training)
mo.pr <- predict(mo,testing)
(rmse <- sqrt(sum((mo.pr - testing$CompressiveStrength)^2)/length(mo.pr)))
# 6.715009