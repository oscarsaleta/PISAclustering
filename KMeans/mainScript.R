rm(list=ls())
source("clusteringLibrary.R")

# REAL DATA
inputData <- readData("merged_features.csv","list_of_features.csv")
a <- inputData$a
a <- cbind(a[,1],apply(a[,-1],2, function(x) {(x/(max(x)-min(x)))})) #Normalization: divide by range of column features
PISA <- inputData$PISA
p <- 0
scores <- a[,1]
scoresSorted <- sort(scores,decreasing = FALSE) #From lowest to greatest

test <- loopThroughSeeds(a,0,0)
