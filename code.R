rm(list=ls(all=TRUE)) 

library(lattice)
library(ggplot2)
library(caret)
#library(randomForest)

downloadUrl <-"https://d396qusza40orc.cloudfront.net/predmachlearn/"
trainingDataCsvFile <- "pml-training.csv"
testingDataCsvFile <- "pml-testing.csv"
if (!file.exists(trainingDataCsvFile)) {
  download.file(paste(downloadUrl,trainingDataCsvFile,sep=""),destfile=trainingDataCsvFile)
}
if (!file.exists(testingDataCsvFile)) {
  download.file(paste(downloadUrl,testingDataCsvFile,sep=""),destfile=testingDataCsvFile)
}

naStrings <- c("", "NA", "#DIV/0!")
trainingData <- read.csv(trainingDataCsvFile, na.strings=naStrings, row.names=1)
testingData <- read.csv(testingDataCsvFile, na.strings=naStrings, row.names=1)


trainingData <- trainingData[,-c(1:6)]
testingData <- testingData[,-c(1:6)]


numberOfNAvaluesTreshold <- 0.25*dim(trainingData)[1]
belowTresholdVariables <- apply(trainingData, 2, function(x) (sum(is.na(x))<numberOfNAvaluesTreshold))
trainingData <- trainingData[,belowTresholdVariables]
testingData <- testingData[,belowTresholdVariables]


nearZeroVarianceVariables <- nearZeroVar(trainingData)
if(length(nearZeroVarianceVariables)>0){
  trainingData <- trainingData[-nearZeroVarianceVariables]
  testingData <- testingData[-nearZeroVarianceVariables]
}

#print(names(trainingData))

plot(trainingData$classe, main="Frequency distribution of classe in training data", xlab="classe", ylab="frequency")






set.seed(12345)
partition <- createDataPartition(y=trainingData$classe, p=0.6,list=FALSE)
trainingPartition <- trainingData[partition,]
validationPartition  <- trainingData[-partition,]



randomForestModel <- train(classe ~ .,data=trainingPartition,method="rf",ntree=10,trControl=trainControl(method = "cv", number = 5))



randomForestModel_PredictionTraining <- predict(randomForestModel$finalModel,newdata=trainingPartition)
cm <- confusionMatrix(randomForestModel_PredictionTraining,trainingPartition$classe)
print(cm)


randomForestModel_PredictionValidation <- predict(randomForestModel$finalModel,newdata=validationPartition)
cm <- confusionMatrix(randomForestModel_PredictionValidation,validationPartition$classe)
print(cm)


randomForestModel_PredictionTesting <- predict(randomForestModel$finalModel,newdata=testingData)
print(randomForestModel_PredictionTesting)




answers <- as.vector(randomForestModel_PredictionTesting)
pml_write_files = function(x) {
  n = length(x)
  for (i in 1:n) {
    filename = paste0("problem_id_", i, ".txt")
    write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, 
                col.names = FALSE)
  }
}

pml_write_files(answers)