library(randomForest)
library(caret)
library(psych)
library(rpart)
library(rpart.plot)
library(rattle)
library(car)
library(lmtest)
library(SDMTools)
set.seed(416)

Train<-read.csv("Analysis Data/TrainData.csv", stringsAsFactors = T)
Test<-read.csv("Analysis Data/TestData.csv", stringsAsFactors = T)

Train$IsFirstGen<-as.factor(Train$IsFirstGen)
Train$IsWV<-as.factor(Train$IsWV)
Train$FiveYrGradEng<-as.factor(Train$FiveYrGradEng)

Test$IsFirstGen<-as.factor(Test$IsFirstGen)
Test$IsWV<-as.factor(Test$IsWV)
Test$FiveYrGradEng<-as.factor(Test$FiveYrGradEng)



entryvars<-c( "MathEntryPoint",
              "HSGPA",
              "IsFirstGen",
              "ACTSATM",
              "ACTSATV",
              "IsWV",
              "TransferCount",
              "APCount",
              "SEM0HoursEnrolled",
              "Gender",
              "Race",
              "AZENSEM0",
              "FiveYrGradEng")

SEM0vars<-c( "MathEntryPoint",
             "HSGPA",
             "IsFirstGen",
             "ACTSATM",
             "ACTSATV",
             "IsWV",
             "TransferCount",
             "APCount",
             "SEM0HoursEnrolled",
             "Gender",
             "Race",
             "AZENSEM0",
             "TGPASEM0",
             "SEM1HoursEnrolled",
             "AZENSEM1",
             "AZCPSEM0",
             "FCUMCMPSEM0",
             "AZCPCUMSEM0",
             "FiveYrGradEng")

trainE<-Train[entryvars]
testE<-Test[entryvars]

trainS1<-Train[SEM0vars]
testS1<-Test[SEM0vars]

#Baseline Model

ZeroRulePred<-rep(0,1378)

cm<-confusionMatrix(data = as.factor(ZeroRulePred), reference = as.factor(Test$FiveYrGradSTEM))

BLmeanAcc <- cm[["overall"]][["Accuracy"]]
#ENTRY CHARACTERISTICS----------------------------------------------

#RF

fit<-randomForest(as.factor(FiveYrGradEng) ~ .
                  , data = trainE, importance = TRUE, ntree = 10000)

save(fit, file = "Models/RF_1stFreshEng_Entry.Rdata")

varImpPlot(fit)

PredictionRandomF<-predict(fit, testE)
confusionMatrix(PredictionRandomF, as.factor(testE$FiveYrGradEng))

#Classification Tree

fit<-rpart(FiveYrGradEng ~. 
           , data = trainE, method = "class",control=rpart.control(minsplit=2, cp=0))

save(fit, file = "Models/CT_1stFreshEng_Entry.Rdata")

ptree<- prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

save(fit, file = "Models/PT_1stFreshEng_Entry.Rdata")

fancyRpartPlot(ptree)

Prediction<-predict(ptree, testE, type = "class")
confusionMatrix(Prediction, testE$FiveYrGradEng)

#Model Compare

control <- trainControl(method="repeatedcv", number=10, repeats=2)


fit.cart <- train(FiveYrGradEng~., data=trainE, method="rpart", trControl=control)
fit.rf <- train(FiveYrGradEng~., data=trainE, method="rf", trControl=control)

results <- resamples(list(CART=fit.cart,RF=fit.rf))

scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales=scales)

#SEM0 CHARACTERISTICS----------------------------------------------

#RF

fit<-randomForest(as.factor(FiveYrGradEng) ~ .
                  , data = trainS1, importance = TRUE, ntree = 10000)

save(fit, file = "Models/RF_1stFreshEng_SEM0.Rdata")

varImpPlot(fit)

PredictionRandomF<-predict(fit, testS1)
confusionMatrix(PredictionRandomF, as.factor(testS1$FiveYrGradEng))

#Classification Tree

fit<-rpart(FiveYrGradEng ~. 
           , data = trainS1, method = "class",control=rpart.control(minsplit=2, cp=0))

save(fit, file = "Models/CT_1stFreshEng_SEM0.Rdata")

ptree<- prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

save(fit, file = "Models/PT_1stFreshEng_SEM0.Rdata")

fancyRpartPlot(ptree)

Prediction<-predict(ptree, testS1, type = "class")
confusionMatrix(Prediction, testS1$FiveYrGradEng)

#Model Compare

control <- trainControl(method="repeatedcv", number=10, repeats=2)


fit.cart <- train(FiveYrGradEng~., data=trainS1, method="rpart", trControl=control)
fit.rf <- train(FiveYrGradEng~., data=trainS1, method="rf", trControl=control)

results <- resamples(list(CART=fit.cart,RF=fit.rf))

scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales=scales)


#ENTRY CHARACTERISTICS----------------------------------------------

#RF

fit<-randomForest(as.factor(FiveYrGradEng) ~ .
                  , data = Train, importance = TRUE, ntree = 10000)

save(fit, file = "Models/RF_1stFreshEng_SEM1.Rdata")

varImpPlot(fit)

PredictionRandomF<-predict(fit, Test)
confusionMatrix(PredictionRandomF, as.factor(Test$FiveYrGradEng))

#Classification Tree

fit<-rpart(FiveYrGradEng ~. 
           , data = Train, method = "class",control=rpart.control(minsplit=2, cp=0))

save(fit, file = "Models/CT_1stFreshEng_SEM1.Rdata")

ptree<- prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

save(fit, file = "Models/PT_1stFreshEng_SEM1.Rdata")

fancyRpartPlot(ptree)

Prediction<-predict(ptree, Test, type = "class")
confusionMatrix(Prediction, Test$FiveYrGradEng)

#Model Compare

control <- trainControl(method="repeatedcv", number=10, repeats=2)


fit.cart <- train(FiveYrGradEng~., data=Train, method="rpart", trControl=control)
fit.rf <- train(FiveYrGradEng~., data=Train, method="rf", trControl=control)

results <- resamples(list(CART=fit.cart,RF=fit.rf))

scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales=scales)
