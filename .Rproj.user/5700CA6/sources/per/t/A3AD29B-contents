#### Pre Process Function ####


B <-boxplot(N$Volume)

O <- boxplot(N$Volume)$out


N[which(N$Volume %in% O),]

N <- N[-which(N$Volume %in% O),]






PPfunction <- function(data) {
  
  N <- dummyVars(" ~ .", data = data)
  
  N <- data.frame(predict(N, newdata = data))
                  
  N <- N[,colSums(is.na(N)) == 0] 
  
  N 
  
}

N <- PPfunction(EP)

#### Train and Test Set function ####


TrainAndTestSets <- function(label,p,data,seed){
  set.seed(seed)
  
  inTrain <- createDataPartition(y= label, p = p , list = FALSE)
  training <- data[inTrain,]
  testing <- data[-inTrain,]
  
  
  list(trainingSet=training,testingSet = testing)
  
}



list <-TrainAndTestSets(N$Volume,0.75,N,123)




#### Training Functions ####


TrainingFunction <- function(formula,data,method,tune){
  
  fitcontrol <-  trainControl(method = "repeatedcv", repeats = 4)

      Model <- train(formula, data = data,method = method, trcontrol = fitcontrol , tunelenght = tune)  
                                          
                             Model                       
      
}


M <- TrainingFunction(Volume ~ ., list$trainingSet, "rf" , 3)


M
