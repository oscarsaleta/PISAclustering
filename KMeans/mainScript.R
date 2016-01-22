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
stripchart(scores[kmcl$result$cl$cluster==1],col=1,pch=19,xlim=c(min(scores),max(scores)))
par(new=T)
stripchart(scores[kmcl$result$cl$cluster==2],col=2,pch=19,xlim=c(min(scores),max(scores)),axes=F)
legend("topright",c("Cluster 1","Cluster 2"),col=c(2,1),pch=19)
par(new=F)


# find feature labels and feature correlation (vs pisa score)
kmcl <- computeWithSeed(a,1006,0.2)
model <- list();
model$matrix <- kmcl$result$aCluster;
model$features <- getModelFeatures(kmcl$result$rFeatures+1,inputData$features)
model$featureCorrelations <- featurePisaCorr(model$matrix)
model$featureCorr_good <- featurePisaCorr_Good(model$matrix,kmcl$groups$g1)
model$featureCorr_bad <- featurePisaCorr_Bad(model$matrix,kmcl$groups$g2)

CFM <- cbind(model$features,model$featureCorrelations[,2])
o_CFM <- CFM[order(CFM[,4],decreasing=T),]
View(o_CFM)
write.csv(o_CFM,file="total.csv")

gCFM <- cbind(model$features,model$featureCorr_good[,2])
o_gCFM <- gCFM[order(gCFM[,4],decreasing=T),]
View(o_gCFM)
write.csv(o_gCFM,file="good.csv")

bCFM <- cbind(model$features,model$featureCorr_bad[,2])
o_bCFM <- bCFM[order(bCFM[,4],decreasing=T),]
View(o_bCFM)
write.csv(o_bCFM,file="bad.csv")

View(cbind(model$features,model$featureCorr_good[order(model$featureCorr_good[,2],decreasing=T),][,2]))
View(cbind(model$features,model$featureCorr_bad[order(model$featureCorr_bad[,2],decreasing=T),][,2]))

save(model,file="model.Rdata")

countryList <- as.character(inputData$countries)
countriesG1 <- getCountryNames(countryList,kmcl$groups$g1)
countriesG2 <- getCountryNames(countryList,kmcl$groups$g2)

# save.image(file=".RData")
plot(scores,col=kmcl$result$cl$cluster,pch=19,xlab="Country index",ylab="PISA score",main="Clustered PISA scores")
lines(0:30,rep(460,31),col=3)
lines(0:30,rep(mean(model$matrix[kmcl$groups$g1,1]),31),type="b",pch=NA)
lines(0:30,rep(mean(model$matrix[kmcl$groups$g2,1]),31),type="b",pch=NA,col=2)
legend("topright",c("Group 1","Group 2"))
# load(".RData",.GlobalEnv)