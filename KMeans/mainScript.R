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
# best <- fasterSeedLoop(a,1004,1008,0.2); # tested from 0 to 14
# best$seed 
# best$accuracy 
# kmcl <- computeWithSeed(a,best$seed,0.2)
for (i in 1004:1008) {
  clustering <- computeWithSeed(a,i,0.2)
  png(filename=paste0("s",i,".png"),width=640,height=480)
  stripchart(scores[clustering$result$cl$cluster==1],col=1,pch=19,xlim=c(min(scores),max(scores)),
             main=paste0("Seed ",i,", p-value ",clustering$result$p,", tail accuracy ",clustering$accuracy),
             xlab="PISA score")
  par(new=T)
  stripchart(scores[clustering$result$cl$cluster==2],col=2,pch=19,xlim=c(min(scores),max(scores)),axes=F)
  legend("topright",c("Cluster 1","Cluster 2"),col=c(2,1),pch=19)
  par(new=F)
  dev.off()
}

# find feature labels and feature correlation (vs pisa score)
kmcl <- computeWithSeed(a,1006,0.2)
model <- list();
model$matrix <- kmcl$result$aCluster;
model$features <- getModelFeatures(kmcl$result$rFeatures+1,inputData$features)
model$featureCorrelations <- featurePisaCorr(model$matrix)
model$featureCorrelations_abs <- abs(model$featureCorrelations)
model$featureCorr_good <- featurePisaCorr_Good(model$matrix,kmcl$groups$g1)
model$featureCorr_good_abs <- abs(model$featureCorr_good)
model$featureCorr_bad <- featurePisaCorr_Bad(model$matrix,kmcl$groups$g2)
model$featureCorr_bad_abs <- abs(model$featureCorr_bad)

CFM <- cbind(model$features,model$featureCorrelations[,2],model$featureCorrelations_abs[,2])
o_CFM <- CFM[order(CFM[,4],decreasing=T),]
# View(o_CFM)
write.csv(o_CFM,file="total.csv")

gCFM <- cbind(model$features,model$featureCorr_good[,2],model$featureCorr_good_abs[,2])
o_gCFM <- gCFM[order(gCFM[,4],decreasing=T),]
# View(o_gCFM)
write.csv(o_gCFM,file="good.csv")

bCFM <- cbind(model$features,model$featureCorr_bad[,2],model$featureCorr_bad_abs[,2])
o_bCFM <- bCFM[order(bCFM[,4],decreasing=T),]
# View(o_bCFM)
write.csv(o_bCFM,file="bad.csv")


save(model,file="model.Rdata")

# countryList <- as.character(inputData$countries)
# countriesG1 <- getCountryNames(countryList,kmcl$groups$g1)
# countriesG2 <- getCountryNames(countryList,kmcl$groups$g2)

# save.image(file=".RData")
png(filename="seed1006.png",width = 640,height = 480)
plot(scores,col=kmcl$result$cl$cluster,pch=19,xlab="Country index",ylab="PISA score",
     main=paste0("Seed 1006, p-value ",kmcl$result$p,", tail accuracy ",format(round(kmcl$accuracy,3),nsmall=3)))
lines(0:30,rep(460,31),col=3)
lines(0:30,rep(mean(model$matrix[kmcl$groups$g1,1]),31),lty="dashed")
lines(0:30,rep(mean(model$matrix[kmcl$groups$g2,1]),31),lty="dashed",col=2)
legend("topright",c(paste0("G1 (mean ",format(round(mean(scores[kmcl$groups$g1]),1),nsmall=1),")"),
                    paste0("G2 (mean ",format(round(mean(scores[kmcl$groups$g2]),1),nsmall=1),")")),
       pch=19,col=c(1,2))
dev.off()
# load(".RData",.GlobalEnv)