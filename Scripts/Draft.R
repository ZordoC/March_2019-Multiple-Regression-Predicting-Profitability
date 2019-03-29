library(caret)
library(readr)
library(rstudioapi)




current_path = getActiveDocumentContext()$path
setwd(dirname(current_path))
setwd("..")
rm(current_path)

EP <- read.csv( file ="./data/Epa.csv" , header = TRUE , sep = ',')
NP <- read.csv(file = "./data/Npa.csv", header = TRUE , sep =',')

#  rm(list=ls(all=TRUE))
rm(list = setdiff(ls(), lsf.str()))

plot(M)
bptest()
B <-boxplot(EP$Volume)
O <- boxplot(EP$Volume)$out
EP[which(EP$Volume %in% O),]
EP <- EP[-which(EP$Volume %in% O),]


N <-PPfunction(EP)

list <-TrainAndTestSets(N$Volume,0.75,N,123)



                         