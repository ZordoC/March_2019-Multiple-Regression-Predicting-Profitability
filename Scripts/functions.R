#### Pre Process Function ####








PPfunction <- function(data) {
  
  N <- dummyVars(" ~ .", data = data)
  
  N <- data.frame(predict(N, newdata = data))
                  
  N <- N[,colSums(is.na(N)) == 0] 
  
  N 
  
}



#### Train and Test Set function ####


TrainAndTestSets <- function(label,p,data,seed){
  set.seed(seed)
  
  inTrain <- createDataPartition(y= label, p = p , list = FALSE)
  training <- data[inTrain,]
  testing <- data[-inTrain,]
  
  
  list(trainingSet=training,testingSet = testing)
  
}








#### Training Functions ####


TrainingFunction <- function(formula,data,method,tune){
  
  fitcontrol <-  trainControl(method = "repeatedcv", repeats = 4)

      Model <- train(formula, data = data,method = method, trcontrol = fitcontrol , tunelenght = tune)  
                                          
                             Model                       
      
}


