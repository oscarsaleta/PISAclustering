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
minimumMean<-which.min(means) #We want to divide minimum and maximum mean scores. Localize which of te 2 lists corresponds to each condition
maximumMean<-which.max(means)
#print(paste("scores max: ",scoresSorted[c(length(RESULTS_list[[minimumMean]])+1:length(RESULTS_list[[maximumMean]]))]))
for( i in 1:length(RESULTS_list[[minimumMean]])){
  if( RESULTS_list[[minimumMean]][i] %in% scoresSorted[1:length(RESULTS_list[[minimumMean]])]){
    counter[1]=counter[1]+1
    print(paste("minimumMean",length(RESULTS_list[[minimumMean]])))
    print(paste("maximumMean",length(RESULTS_list[[maximumMean]])))
  }
  
}
for( i in 1:length(RESULTS_list[[maximumMean]])){
  if( RESULTS_list[[maximumMean]][i] %in% scoresSorted[c(length(RESULTS_list[[minimumMean]])+1:length(RESULTS_list[[maximumMean]]))]){
    counter[2]=counter[2]+1
    
  }
  
}
print(paste("counter 1:",counter[1]))
print(paste("counter 2:",counter[2]))
percentageSuccess <- c(counter[1]/length(RESULTS_list[[minimumMean]])*100,counter[2]/length(RESULTS_list[[maximumMean]])*100)
print(paste("percentage success: ",percentageSuccess))

#CHECK IF IMPROVEMENT IN % SUCCESS HAS OCCURED. IF NOT, RECOVER FEATURE

print(paste("percentage success previous: ",percentageSuccessPrev))

#aPrev<-a
if(percentageSuccess[1]>=percentageSuccessPrev[1] || percentageSuccess[2]>=percentageSuccessPrev[2]){
  ##ATTENTION!! Decide the criteria, as we could have this example: one gets better and the other gets worse
  #  "percentage success:  0"  "percentage success:  75"
  #  "percentage success previous:  50" "percentage success previous:  25"
  
  
  #WEIGHTSfirst<-sparsehc$ws
  #print(paste("weight: ",WEIGHTSfirst))
  print(paste("weight: ",sparsehc$ws))
  deleteFeature <- orderedWeights[1] #which.max(WEIGHTSfirst)
  print(paste("Deleting feature ",deleteFeature))
  print("improvement")
  #if it has improved we remove another feature
  aPrev<-a #we store the matrix to the former one, to recover it in case no improve is achieved
  a<-a[,-(deleteFeature+1)] #As first columns is PISA scores
  
}else{
  
  #WEIGHTSfirst<-sparsehc$ws[,1]
  #print(paste("weight: ",WEIGHTSfirst))
  #deleteFeature <- which.max(WEIGHTSfirst)
  #if it has improved we remove another feature
  #aPrev<-a #we store the matrix to the former one, to recover it in case no improve is achieved
  #a<-a[,-(deleteFeature+1)] #As first columns is PISA scores
  
  #We try with the second most important weight  
  #WEIGHTS<-WEIGHTSfirst[-deleteFeature]
  print(paste("weight: ",WEIGHTS))
  deleteFeature <- orderedWeights[2] #which.max(WEIGHTS)
  print(paste("Deleting feature ",deleteFeature))
  print("not improvement")
  #we go back to previous condition  
  a<-aPrev
  
}

percentageSuccessPrev <- percentageSuccess