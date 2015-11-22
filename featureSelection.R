########################################
#THE SELECTION ALGORITHM
########################################

#We start by 2 main clusters

##########################
#REAL DATA ALGORITHM
##########################

#mydata <- read.table("C:/Users/Gemma/Documents/Master/ModellingWorkshop/PISA.csv", sep=",", header=FALSE)
#COUNTRIES <- matrix(mydata$V1,ncol=1)
#RESULTS <- matrix(mydata$V2,ncol=1)
#NumObservations<-print(length(RESULTS))
#SORTED<-mydata[order(mydata[,2],mydata[,1],decreasing=FALSE),] #Matrix of countries and its PISA scores, sorted from the lowest to the highest values

##########################
#FAKE DATA ALGORITHM
##########################

counter<-c(0,0)
minimumMean<-which.min(means) #We want to divide minimum and maximum mean scores.
maximumMean<-which.max(means)
for( i in 1:length(RESULTS_list[[minimumMean]])){
  if( RESULTS_list[[minimumMean]][i] %in% scoresSorted[1:length(RESULTS_list[[minimumMean]])]){
    # TODO: WHAT IF 2 COUNTRIES HAVE THE SAME SCORE? WE COULD ADD A NAMES COLUMN AND COMPARE THE NAMES (and change all the code to take this column into account)
    # or create just a vector of names ordered by score?
    counter[1]=counter[1]+1
  }
  
}
for( i in 1:length(RESULTS_list[[maximumMean]])){
  if( RESULTS_list[[maximumMean]][i] %in% scoresSorted[c(length(RESULTS_list[[minimumMean]])+1:length(RESULTS_list[[maximumMean]]))]){
    counter[2]=counter[2]+1
  }
  
}
# print(paste("counter",counter))
percentageSuccess <- c(counter[1]/length(RESULTS_list[[minimumMean]])*100,counter[2]/length(RESULTS_list[[maximumMean]])*100)
# print(paste("percentage success: ",percentageSuccess))

#CHECK IF IMPROVEMENT IN % SUCCESS HAS OCCURED. IF NOT, RECOVER FEATURE

WEIGHTS<-sparsehc$ws[,1]
# print(paste("weight: ",WEIGHTS))
deleteFeature <- which.max(WEIGHTS)

#aPrev<-a
#if(percentageSuccess[1]>percentageSuccessPrev[1] || percentageSuccess[2]>percentageSuccessPrev[2]){

a<-a[,-(deleteFeature+1)] #As first columns is PISA scores
#}
