


library(caret)
library(randomForest)
set.seed(1406)

trainFile<-read.csv("pml-training.csv", header=T)
testFile<-read.csv("pml-testing.csv", header=T)


trainFileMissing <- sapply(trainFile, function (x) any(is.na(x) | x == ""))
predictors0 <- !trainFileMissing & grepl("belt|arm|dumbbell|forearm", names(trainFileMissing))
predictors <- c("classe",names(trainFileMissing)[predictors0])


cleanTrain <- (trainFile[, predictors])
cleanTrainDivided <- createDataPartition(cleanTrain$classe, p=0.70,list=F)
training<-cleanTrain[cleanTrainDivided,] 
test<-cleanTrain[-cleanTrainDivided,] 

dim(training)
dim(test)


time1 = proc.time()
(randForest = randomForest(classe~., data=training, ntree = 100))

plot(randForest)

prediction <- predict(randForest, test, type = "class")
confusionMatrix(prediction, test$classe)

predictionRF <- predict(randForest, testFile)
predictionRF

#You should create a report describing 
# how you built your model,
# how you used cross validation, (divided the train0 in training and test)
# what you think the expected out of sample error is, and (see in the output)
# why you made the choices you did. (rf are widely known as an accurate approach)

#rm(list=ls(all=TRUE))