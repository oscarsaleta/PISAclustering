rm(list=ls())
library(sparcl)

# REAL DATA
dataC <- read.csv("merged_features_short.csv",header = TRUE)

features <- read.csv("list_of_features.csv",header = TRUE)

dataCTemporal <- rbind(1:ncol(dataC),dataC[,-1])

#features <- features[colSums(is.na(dataC))==0,]
#features <- features[!apply(dataC,2,function(x){any(x=='a')}),]
#features <- features[!apply(features,1,function(x){any(is.na(x))}),]

dataCTemporal <- dataCTemporal[,colSums(is.na(dataCTemporal))==0]
# other weird values such as '0' o 'a'
dataCTemporal <- dataCTemporal[,!apply(dataCTemporal,2,function(x){any(x=='a')})]
keptFeatures <- dataCTemporal[1,]
dataCTemporal <- dataCTemporal[-1,]
dataC <- cbind(dataC[,1],dataCTemporal)

PISA <- read.csv("PISA.csv",header = FALSE)
PISA <- PISA[rowSums(is.na(PISA))==0,]


data <- cbind(PISA$V2,dataC)
data <- data[,-2] #delete first column (countries' names)

a <- as.matrix(data)
a <- a[order(a[,1]),]
a <- cbind(a[,1],apply(a[,-1],2, function(i) {i/(max(i)-min(i))})) #Normalization: divide by range of column features
rownames(a) <- NULL
colnames(a) <- NULL
p <- 0
scores <- a[,1]
scoresSorted <- sort(scores,decreasing = FALSE) #From lowest to greatest

#SHUFFLE DATA MATRIX BY COLUMNS
aIni <- a
