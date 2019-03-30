library(caret)
library(readr)
library(rstudioapi)
library(e1071)
library(dplyr)
library(rpart)


#rm(list = setdiff(ls(), lsf.str()))

current_path = getActiveDocumentContext()$path
setwd(dirname(current_path))
setwd("..")
rm(current_path)
EP <- read.csv( file ="./data/Epa.csv" , header = TRUE , sep = ',')
NP <- read.csv(file = "./data/Npa.csv", header = TRUE , sep =',')
EP <- EP[,c(1,5,9,18)]



Knn <- EP %>% mutate_at(scale, .vars = vars(-1))


Knn <- PPfunction(Knn)

EP <- PPfunction(EP)



Knn <- RmOut(Knn)

EP <- RmOut(EP)

List <- TrainAndTestSets(EP$Volume,0.75,EP,233)

ListK <- TrainAndTestSets(Knn$Volume,0.75,Knn,233)


ModelRandomForest <- TrainingFunction(Volume ~.,List$trainingSet,"rf",5)

ModelRandomForest

KNN <- TrainingFunction(Volume ~.,ListK$trainingSet,"knn",5)

PredictionRandomForest <- predict(ModelRandomForest,List$testingSet)

TestResultsRF <- postResample(PredictionRandomForest,List$testingSet$Volume)

KnnPrediction <- predict(KNN,ListK$testingSet)

TestResultsKNN <-postResample(KnnPrediction,ListK$testingSet$Volume)



SVM <- train()


svm.model <- svm(V4 ~ ., data = trainset, cost = 1000, gamma = 0.0001)
svm.pred  <- predict(svm.model, testset[,-3])
  crossprod(svm.pred - testset[,3]) / length(testindex)


svm.model <- svm(Volume ~ . , data = List$trainingSet,cost=1000 , gamma = 0.001)

svm.pred <- predict(svm.model,List$testingSet)

TestResultsSVM <- postResample(svm.pred,List$testingSet$Volume)

AllTestResults <- cbind(TestResultsKNN,TestResultsRF,TestResultsSVM)

AllTestResults                         
