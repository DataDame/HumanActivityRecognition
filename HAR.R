library(knitr)
opts_chunk$set(cache=TRUE,echo=TRUE)
options(width=120)
library(caret)

setwd("~/Documents/DataScience/HAR")
downloadDataset <- function(URL="", destFile="data.csv"){
  if(!file.exists(destFile)){
    download.file(URL, destFile, method="curl")
  }else{
    message("Dataset already downloaded.")
  }
}

trainURL<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testURL <-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
downloadDataset(trainURL, "pml-training.csv")
downloadDataset(testURL, "pml-testing.csv")
training <- read.csv("pml-training.csv",na.strings=c("NA",""))
testing <-read.csv("pml-testing.csv",na.strings=c("NA",""))

dim(training)
summary(training)
head(training)

columnNACounts <- colSums(is.na(training))        
badColumns <- columnNACounts >= 19000             
cleanTrainingdata <- training[!badColumns]        
sum(is.na(cleanTrainingdata))

cleanTrainingdata <- cleanTrainingdata[, c(7:60)]

columnNACounts <- colSums(is.na(testing))         
badColumns <- columnNACounts >= 20                
cleanTestingdata <- testing[!badColumns]        
sum(is.na(cleanTestingdata))

cleanTestingdata <- cleanTestingdata[, c(7:60)]

partition <- createDataPartition(y = cleanTrainingdata$classe, p = 0.6, list = FALSE)
trainingdata <- cleanTrainingdata[partition, ]
testdata <- cleanTrainingdata[-partition, ]

model <- train(classe ~ ., data = trainingdata, method = "rf", prox = TRUE, trControl = trainControl(method = "cv", number = 4, allowParallel = TRUE))
training_pred <- predict(model, trainingdata)
confusionMatrix(training_pred, trainingdata$classe)

testing_pred <- predict(model, testdata)
confusionMatrix(testing_pred, testdata$classe)

answers <- predict(model, cleanTestingdata)
answers <- as.character(answers)
answers
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(answers)
