

#### Pre Process #### 

PPfunction <- function(data) {
  
  N <- dummyVars(" ~ .", data = data)
  
  N <- data.frame(predict(N, newdata = data))
  
  N <- N[,colSums(is.na(N)) == 0] 
  
  N 
  
}



#### Removing Outliers #### 


B <-boxplot(N$Volume)

O <- boxplot(N$Volume)$out

N[which(N$Volume %in% O),]

N <- N[-which(N$Volume %in% O),]




#### Plots #### 


ggplot(N,
       aes(x=Volume))+
  geom_bar(color="blue", fill="yellow")


###
MyPlotFunction <- function(data,variable,variable2)
{
          if (variable2 == 0)
            {  
        G <- ggplot(data,aes(x=variable) ) + geom_bar()
          }
  
          else 
            
          G <- ggplot(data,aes(x=variable,y=variable2)) + geom_bar()
          
            
}

G <- MyPlotFunction(N,Volume,0)  

G

    
G <- MyPlotFunction(N,Volume,N$Price)  
  
G  
  
### 

E1 <-subset(EP, EP$ProductType =="PC")

E2 <- subset(EP, EP$ProductType =="Laptop")

rbind(E1,E2)


###

  

  