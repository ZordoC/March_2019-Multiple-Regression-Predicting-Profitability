library(caret)
library(readr)
library(rstudioapi)




current_path = getActiveDocumentContext()$path
setwd(dirname(current_path))
setwd("..")
rm(current_path)

EP <- read.csv( file ="./data/Epa.csv" , header = TRUE , sep = ',')
NP <- read.csv(file = "./data/Npa.csv", header = TRUE , sep =',')

rm(list=ls(all=TRUE))

EP[,EP$Volume < 4000]

qqnorm(EP$Volume)


plot(M)
bptest()
B <-boxplot(N$Volume)

O <- boxplot(N$Volume)$out
O



N[which(N$Volume %in% O),]

N <- N[-which(N$Volume %in% O),]



p <- predict(M,list$testingSet)

postResample(p,list$testingSet$Volume)
