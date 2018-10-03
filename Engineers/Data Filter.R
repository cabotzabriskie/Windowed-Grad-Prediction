library(plyr)
library(dplyr)

data <- read.csv("Input Data/DemoRptMajSeq.csv", stringsAsFactors = F)

data<-subset(data, MAJSEM0 == 9)
data<-subset(data, FirstWVUSemNum <= 68)
data<-subset(data, AdmitCode == 1)
data<-subset(data, ACTSATM != -1)
data<-subset(data, !is.na(MathEntryPoint))
data<-subset(data, !is.na(Gender))
data<-subset(data, !is.na(IsFirstGen))
data$HSGPA<-as.numeric(data$HSGPA)
data<-subset(data, !is.na(HSGPA))

data$AllSems<-data$LastSemNum-data$FirstWVUSemNum
data$FiveYr<-"N"
data$FiveYr<-ifelse(data$AllSems <= 14, "Y", data$FiveYr)
data$FiveYrGradEng<-0
data$FiveYrGradEng[data$DegreeClassification == 9]<-1
data$FiveYrGradEng<-ifelse(data$FiveYr == "N", 0, data$FiveYrGradEng)


data$Race<-"NA"
for(i in 1:nrow(data)){
  if(!is.na(data$RaceIndian[i])) data$Race[i]<-"O"
  if(!is.na(data$RaceHawaiian[i])) data$Race[i]<-"O"
  if(!is.na(data$RaceWhite[i])) data$Race[i]<-"W"
  if(!is.na(data$RaceHispanic[i])) data$Race[i]<-"H"
  if(!is.na(data$RaceBlack[i])) data$Race[i]<-"B"
  if(!is.na(data$RaceAsian[i])) data$Race[i]<-"A"
}


data<-subset(data, Race != "NA")


data<-data[c("AnonID",
             "MathEntryPoint",
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
             "AZCPSEM1",
             "TGPASEM1",
             "AZCPCUMSEM1",
             "FCGPASEM1",
             "FCUMCMPSEM0",
             "FCUMCMPSEM1",
             "AZCPCUMSEM0",
             "FiveYrGradEng")]

data$AZENSEM0<-ifelse(is.na(data$AZENSEM0), 0, data$AZENSEM0)
data$AZENSEM1<-ifelse(is.na(data$AZENSEM1), 0, data$AZENSEM1)
data$AZCPSEM1<-ifelse(is.na(data$AZCPSEM1), 0, data$AZCPSEM1)
data$TGPASEM1<-ifelse(is.na(data$TGPASEM1), 0, data$TGPASEM1)
data$AZCPCUMSEM1<-ifelse(is.na(data$AZCPCUMSEM1), 0, data$AZCPCUMSEM1)

colnames(data)[colSums(is.na(data)) > 0]


#approx 60 40 split
train<-data[sample(nrow(data), 2065),]
l<-unique(train$AnonID)
test<-data[!data$AnonID %in% l,]

train<-train[,!names(train) %in% c("AnonID")]
test<-test[,!names(test) %in% c("AnonID")]

write.csv(train, "Analysis Data/TrainData.csv", row.names = F)
write.csv(test, "Analysis Data/TestData.csv", row.names = F)




