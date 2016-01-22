rm(list=ls())
source("clusteringLibrary.R")

# REAL DATA
inputData <- readData("merged_features.csv","list_of_features.csv")
a <- inputData$a
a <- cbind(a[,1],apply(a[,-1],2, function(x) {(x/(max(x)-min(x)))})) #Normalization: divide by range of column features
countries <- inputData$countries
PISA <- inputData$PISA
p <- 0
scores <- a[,1]
scoresSorted <- sort(scores,decreasing = FALSE) #From lowest to greatest

test <- loopThroughSeeds(a,countries,0,2,0.2)
save(test,file="Results.RData")

for (j in 1:ncol(test$groupMatrix)) {
  if (is.na(test$groupMatrix[1,j]))
    next
  png(filename=paste0("seed",j-1,".png"))
  par(new=F)
  stripchart(scores[test$groupMatrix[-c(1,2),][,j+2]==1],col=1,xlim = c(min(scores),max(scores)))
  par(new=T)
  stripchart(scores[test$groupMatrix[-c(1,2),][,j+2]==2],col=2,axes=F,xlim=c(min(scores),max(scores)))
  dev.off
}

save.image(file=".RData")

seed2 <- computeWithSeed(a,2,0.15)
seed2$accuracy
length(seed2$groups$relevantFeatures)
stripchart(scores[seed2$groups$cl$cluster==1],col=1,xlim=c(min(scores),max(scores)))
par(new=T)
stripchart(scores[seed2$groups$cl$cluster==2],col=2,xlim=c(min(scores),max(scores)),axes=F)
par(new=F)
