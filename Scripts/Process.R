

#### Pre Process #### 

PPfunction <- function(data) {
  
  N <- dummyVars(" ~ .", data = data)
  
  N <- data.frame(predict(N, newdata = data))
  
  N <- N[,colSums(is.na(N)) == 0] 
  
  N 
  
}



# How to apply :  , need dplyr package
# EP[2:4] <- apply(EP[2:4],2,normFunc)

#### Removing Outliers #### 

RmOut <- function(D,V)

  {
  
  Out <- boxplot(D$V ,plot = FALSE)$out
  K <- D[-which(D$V %in% Out),]
  K
}

L <- RmOut(EP,Volume)

###
MyPlotFunction <- function(data,variable=0,variable2=0)
{
  if (variable2 == 0)
  {  
    G <- ggplot(data,aes(x= data[[variable]]) ) + 
      geom_bar()+ labs( x = variable) 
  }
  else
    G <- ggplot(data,aes(data,x= data[[variable]],y= data[[variable2]])) +
      geom_bar(stat = "Identity") + 
      labs(x = variable,y=variable2 , title = 'Bar Plot' )
  
  
  
}





###

SubSetDataProductTypes <- function(data,p,p1 = 0,p2 = 0 , p3 = 0 , p4 = 0)
{
  if ( p1 == 0 && p2 == 0 && p3 == 0 && p4 == 0)
  {
    Nsub <- subset(data, data$ProductType == p)
    
    return(Nsub)
  }
  else if (p2 == 0 && p3 == 0 && p4 == 0){
    Nsub <- subset(data, data$ProductType == p)
    
    Nsub2 <-subset(data, data$ProductType == p1)
    
    Nsub2 <- rbind(Nsub,Nsub2)
    
    return(Nsub2)
  }
  else  if (p3 == 0 && p4 == 0) 
  {
    
    Nsub <- subset(data, data$ProductType == p)
    
    Nsub2 <-subset(data, data$ProductType == p1)
    
    Nsub3 <- subset(data,data$ProductType == p2)
    
    Nsub3 <- rbind(Nsub,Nsub2,Nsub3)
    
    return(Nsub3)
    
  }
  
  else  if (p4 == 0){
    Nsub <- subset(data, data$ProductType == p)
    
    Nsub2 <-subset(data, data$ProductType == p1)
    
    Nsub3 <- subset(data,data$ProductType == p2)
    
    Nsub4 <- subeset(data,data$ProductType == p3)
    
    Nsub4 <- rbind(Nsub,Nsub2,Nsub3,Nsub4)
    
    return(Nsub4)
  }
  
  else{
    
    Nsub <- subset(data, data$ProductType == p)
    
    Nsub2 <-subset(data, data$ProductType == p1)
    
    Nsub3 <- subset(data,data$ProductType == p2)
    
    Nsub4 <- subeset(data,data$ProductType == p3)
    
    Nsub5 <- subset(data,data$ProductType == p4)
    
    Nsub5 <- rbind(Nsub,Nsub2,Nsub3,Nsub4,Nsub5)
    
    return(Nsub5)
  } 
}







