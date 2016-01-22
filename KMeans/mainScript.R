rm(list=ls())

# WARNING: DO NOT EXECUTE COMMENTED LINES (AROUND 5h OF EXECUTION)
source("clusteringLibrary.R")
# 
# # REAL DATA
inputData <- readData("merged_features.csv","list_of_features.csv")
a <- inputData$a
#Normalization: divide by range of column features
a <- cbind(a[,1],apply(a[,-1],2, function(x) {(x/(max(x)-min(x)))})) 
countries <- inputData$countries
PISA <- inputData$PISA
p <- 0
scores <- a[,1]
scoresSorted <- sort(scores,decreasing = FALSE) #From lowest to greatest


# look for best seed in range
best <- fasterSeedLoop(a,1004,1008,0.2); # tested from 0 to 14
best$seed 
best$accuracy 
kmcl <- computeWithSeed(a,best$seed,0.2)
stripchart(scores[kmcl$groups$cl$cluster==1],col=1,pch=19,xlim=c(min(scores),max(scores)))
par(new=T)
stripchart(scores[kmcl$groups$cl$cluster==2],col=2,pch=19,xlim=c(min(scores),max(scores)),axes=F)
legend("topright",c("Cluster 1","Cluster 2"),col=c(2,1),pch=19)
par(new=F)


# find feature labels and feature correlation (vs pisa score)
kmcl <- computeWithSeed(a,1006,0.2)
model <- list();
model$matrix <- kmcl$result$aCluster;
model$features <- getModelFeatures(kmcl$result$rFeatures+1,inputData$features$feature)
model$featureCorrelations <- featurePisaCorr(model$matrix)
View(model$matrix)
View(model$features)
View(model$featureCorrelations)
View(model$featureCorrelations[order(model$featureCorrelations[,2],decreasing=T),])
View(cbind(model$features,model$featureCorrelations[,2]))

save(model,"model.Rdata")

# save.image(file=".RData")

# load(".RData",.GlobalEnv)