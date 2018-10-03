library(randomForest)
library(caret)
library(psych)
library(rpart)
library(rpart.plot)
library(rattle)
library(car)
library(lmtest)
set.seed(416)

Train<-read.csv("Analysis Data/TrainData.csv", stringsAsFactors = T)
Test<-read.csv("Analysis Data/TestData.csv", stringsAsFactors = T)

Train$IsFirstGen<-as.factor(Train$IsFirstGen)
Train$IsWV<-as.factor(Train$IsWV)
Train$FiveYrGradSTEM<-as.factor(Train$FiveYrGradSTEM)

Test$IsFirstGen<-as.factor(Test$IsFirstGen)
Test$IsWV<-as.factor(Test$IsWV)
Test$FiveYrGradSTEM<-as.factor(Test$FiveYrGradSTEM)



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
              "FiveYrGradSTEM",
              "MAJSEM0")

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
             "FiveYrGradSTEM",
             "MAJSEM0",
             "MAJSEM1")

trainE<-Train[entryvars]
testE<-Test[entryvars]

trainS1<-Train[SEM0vars]
testS1<-Test[SEM0vars]

#Baseline Model


ZeroRulePred<-rep(0,2019)

cm<-confusionMatrix(data = as.factor(ZeroRulePred), reference = as.factor(Test$FiveYrGradSTEM))

BLmeanAcc <- cm[["overall"]][["Accuracy"]]

#ENTRY----------------------------------------------

#RF

fit<-randomForest(as.factor(FiveYrGradSTEM) ~ .
                  , data = trainE, importance = TRUE, ntree = 10000)

save(fit, file = "Models/RF_1stFreshSTEM_Entry.Rdata")

varImpPlot(fit)

PredictionRandomF<-predict(fit, testE)
confusionMatrix(PredictionRandomF, as.factor(testE$FiveYrGradSTEM))

#Classification Tree

fit<-rpart(FiveYrGradSTEM ~. 
           , data = trainE, method = "class",control=rpart.control(minsplit=2, cp=0))

save(fit, file = "Models/CT_1stFreshSTEM_Entry.Rdata")

ptree<- prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

save(ptree, file = "Models/PT_1stFreshSTEM_Entry.Rdata")

fancyRpartPlot(ptree)

Prediction<-predict(ptree, testE, type = "class")
confusionMatrix(Prediction, testE$FiveYrGradSTEM)

#Model Compare

control <- trainControl(method="repeatedcv", number=10, repeats=2)


fit.cart <- train(FiveYrGradSTEM~., data=trainE, method="rpart", trControl=control)
fit.rf <- train(FiveYrGradSTEM~., data=trainE, method="rf", trControl=control)

results <- resamples(list(CART=fit.cart,RF=fit.rf))

scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales=scales)

#SEM0 CHARACTERISTICS----------------------------------------------

#RF

fit<-randomForest(as.factor(FiveYrGradSTEM) ~ .
                  , data = trainS1, importance = TRUE, ntree = 10000)

save(fit, file = "Models/RF_1stFreshSTEM_SEM0.Rdata")

varImpPlot(fit)

PredictionRandomF<-predict(fit, testS1)
confusionMatrix(PredictionRandomF, as.factor(testS1$FiveYrGradSTEM))

#Classification Tree

fit<-rpart(FiveYrGradSTEM ~. 
           , data = trainS1, method = "class",control=rpart.control(minsplit=2, cp=0))

save(fit, file = "Models/CT_1stFreshSTEM_SEM0.Rdata")

ptree<- prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

save(fit, file = "Models/PT_1stFreshSTEM_SEM0.Rdata")

fancyRpartPlot(ptree)

Prediction<-predict(ptree, testS1, type = "class")
confusionMatrix(Prediction, testS1$FiveYrGradSTEM)

#Model Compare

control <- trainControl(method="repeatedcv", number=10, repeats=2)


fit.cart <- train(FiveYrGradSTEM~., data=trainS1, method="rpart", trControl=control)
fit.rf <- train(FiveYrGradSTEM~., data=trainS1, method="rf", trControl=control)

results <- resamples(list(CART=fit.cart,RF=fit.rf))

scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales=scales)


#SEM1----------------------------------------------

#RF

fit<-randomForest(as.factor(FiveYrGradSTEM) ~ .
                  , data = Train, importance = TRUE, ntree = 10000)

save(fit, file = "Models/RF_1stFreshSTEM_SEM1.Rdata")

varImpPlot(fit)

PredictionRandomF<-predict(fit, Test)
confusionMatrix(PredictionRandomF, as.factor(Test$FiveYrGradSTEM))

#Classification Tree

fit<-rpart(FiveYrGradSTEM ~. 
           , data = Train, method = "class",control=rpart.control(minsplit=2, cp=0))

save(fit, file = "Models/CT_1stFreshSTEM_SEM1.Rdata")

ptree<- prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

save(ptree, file = "Models/PT_1stFreshSTEM_SEM1.Rdata")

fancyRpartPlot(ptree)

Prediction<-predict(ptree, Test, type = "class")
confusionMatrix(Prediction, Test$FiveYrGradSTEM)

#Model Compare

control <- trainControl(method="repeatedcv", number=10, repeats=2)


fit.cart <- train(FiveYrGradSTEM~., data=Train, method="rpart", trControl=control)
fit.rf <- train(FiveYrGradSTEM~., data=Train, method="rf", trControl=control)

results <- resamples(list(CART=fit.cart,RF=fit.rf))

scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales=scales)
