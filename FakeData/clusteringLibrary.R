#    GENERATE FAKE DATA    ##############################################
#########################################################################
generateData <- function() {
  set.seed(1)
  # CREATE DATA
  features=9
  observations=10
  x <- matrix(c(rnorm(features*observations)),ncol=features)
  #Vector to distinguish between relevant features classes
  y <- c(rep(1,5),rep(2,5))
  #Vector to distinguish between fake relevant features classes
  z <- c(rep(1,3),rep(2,2),rep(3,3),rep(4,2))
  
  #Relevant features
  x[y==1,1] <- x[y==1,1]+5
  x[y==1,2] <- x[y==1,2]+9
  x[y==1,3] <- x[y==1,3]+6
  
  x[y==2,1] <- x[y==2,1]+10
  x[y==2,2] <- x[y==2,2]+4
  x[y==2,3] <- x[y==2,3]+3
  
  #Relevant fake features. Similarities bewteen different classes
  x[z==1,4] <- x[z==1,4]+3
  x[z==3,4] <- x[z==3,4]+3
  
  x[z==1,5] <- x[z==1,5]+7
  x[z==3,5] <- x[z==3,5]+7
  
  x[z==2,6] <- x[z==2,6]+15
  x[z==4,6] <- x[z==2,6]+15
  
  #non relevant features. N(different averages and SE, but equivalent for all countries)
  x[,7] <- rnorm(10, mean = 2, sd = 1)
  x[,8] <- rnorm(10, mean = 10, sd = 0.5)
  x[,9] <- rnorm(10, mean = 4, sd = 0.5)
  
  #We create the matrix with the normalization of the features performed.
  #The new values will range [0->1]
  a <- matrix(c(rnorm(features*observations)),ncol=features)
  for (i in 1:9){
    # a[,i] <- (x[,i]-min(x[,i]))/(max(x[,i])-min(x[,i]))
    # a[,i] <- (x[,i]-mean(x[,i]))/(sd(x[,i]))
    a[,i] <- x[,i]/max(x[,i])
  }
  
  #PISA rates: in 2012 results -> max value=613; min value=368
  PISA <- x <- matrix(c(rnorm(10,sd=40)),ncol=1)
  ranking <-c(rep(1,5),rep(2,5))
  PISA[ranking==1,] <- round(PISA[ranking ==1,]+550)
  PISA[ranking==2,] <- round(PISA[ranking ==2,]+400)
  
  a<-cbind(PISA,a)
  # scores <- a[,1]
  # scoresSorted <- sort(scores,decreasing = FALSE) #From lowest to greatest
  
  return(list(y=y,a=a))
}
#########################################################################


#    GENERATE FAKE DATA V2    ###########################################
#########################################################################
generateData2 <- function() {
  set.seed(1)
  
  features=9
  observations=10
  x <- matrix(rep(0,features*observations),ncol=features)
  
  #Vector to distinguish between relevant features classes
  y <- c(rep(1,5),rep(2,5))
  #Vector to distinguish between fake relevant features classes
  z <- c(rep(1,3),rep(2,2),rep(3,3),rep(4,2))
  
  #Relevant features
  x[y==1,1] <- x[y==1,1]+rnorm(5, mean = 75, sd = 8) #Simulate %
  x[y==1,2] <- x[y==1,2]+rnorm(5, mean = 0.9, sd = 0.1) #Simulate %
  x[y==1,3] <- x[y==1,3]+rnorm(5, mean = 37, sd = 4) #Simulate age
  
  x[y==2,1] <- x[y==2,1]+rnorm(5, mean = 55, sd = 3)
  x[y==2,2] <- x[y==2,2]+rnorm(5, mean = 1.2, sd = 0.1)
  x[y==2,3] <- x[y==2,3]+rnorm(5, mean = 53, sd = 3)
  
  #Relevant fake features. Similarities bewteen different classes
  x[z==1,4] <- x[z==1,4]+rnorm(3, mean = 0.8, sd = 0.05)
  x[z==3,4] <- x[z==3,4]+rnorm(3, mean = 0.8, sd = 0.05)
  
  x[z==2,4] <- x[z==2,4]+rnorm(2, mean = 4, sd = 0.8)
  x[z==4,4] <- x[z==4,4]+rnorm(2, mean = 4, sd = 0.1)
  
  x[z==1,5] <- x[z==1,5]+rnorm(3, mean = 45, sd = 3)
  x[z==3,5] <- x[z==3,5]+rnorm(3, mean = 45, sd = 3)
  
  x[z==2,5] <- x[z==2,5]+rnorm(2, mean = 65, sd = 3)
  x[z==4,5] <- x[z==4,5]+rnorm(2, mean = 65, sd = 3)
  
  x[z==1,6] <- x[z==1,6]+rnorm(3, mean = 500, sd = 10)
  x[z==3,6] <- x[z==3,6]+rnorm(3, mean = 500, sd = 10)
  
  x[z==2,6] <- x[z==2,6]+rnorm(2, mean = 5000, sd = 100)
  x[z==4,6] <- x[z==4,6]+rnorm(2, mean = 5000, sd = 100)
  
  #non relevant features. N(different averages and SE, but equivalent for all countries)
  x[,7] <- rnorm(10, mean = 50, sd = 0.02)
  x[,8] <- rnorm(10, mean = 0.5, sd = 0.011)
  x[,9] <- rnorm(10, mean = 25, sd = 0.01)
  
  print(x)
  #We create the matrix with the normalization of the features performed.
  #The new values will range [0->1]
  a <- matrix(c(rnorm(features*observations)),ncol=features)
  for (i in 1:9){
    #a[,i] <- (x[,i]-min(x[,i]))/(max(x[,i])-min(x[,i]))
    a[,i] <- (x[,i])/(sd(x[,i]))
    #a[,i] <- x[,i]
  }
  
  
  #PISA rates: in 2012 results -> max value=613; min value=368
  PISA <- x <- matrix(c(rnorm(10,sd=40)),ncol=1)
  ranking <-c(rep(1,5),rep(2,5))
  PISA[ranking==1,] <- round(PISA[ranking ==1,]+550)
  PISA[ranking==2,] <- round(PISA[ranking ==2,]+400)
  
  a<-cbind(PISA,a)
  
  return(list(y=y,a=a))
}



#    GET SORTED SCORES FROM FAKE DATA    ################################
#########################################################################
getSortedScores <- function(a) {
  scores <- a[,1]
  scoresSorted <- sort(scores,decreasing=FALSE)
  return(scoresSorted)
}
#########################################################################



#    REMOVE FEATURE    ##################################################
#########################################################################
removeFeature <- function(a,weights,nFeature) {
  removeCol <- weights[nFeature]+1
#   print(paste("Removing feature",removeCol-1))
  return(a[,-removeCol])
}
#########################################################################



#    COMPUTE K-MEANS    #################################################
#########################################################################
computeKMeans <- function(a,y) {
  f=file();sink(f)
  perm.out <- KMeansSparseCluster.permute(a[,-1],wbounds = c(1.5,2:6),nperms = 5,K=2)
  sink();close(f)
  # plot(perm.out)
  f=file();sink(f)
  sparsekc <- KMeansSparseCluster(a[,-1],wbounds = perm.out$bestw, K=2)
  sink();close(f)
  groupColors <- ifelse(sparsekc[[1]]$Cs == 1,"red","blue")
  realShapes <- ifelse(a[,1]<mean(a[,1]),16,17)
  par(mfrow=c(1,2))
  plot(sparsekc)
  plot(a[,1],col=groupColors,pch=realShapes)
  par(mfrow=c(1,1))
  orderedWeights <- order(sparsekc[[1]]$ws,decreasing=T)
  groups <- sparsekc[[1]]$Cs
  GROUPS <- list(which(groups==1),which(groups==2))
  return(list(groups=GROUPS,orderedWeights=orderedWeights))
}



#    COMPUTE DENDOGRAM    ###############################################
#########################################################################
computeDendogram <- function(a,y) {
  # SPARSE
  par(mfrow=c(1,1))
  # Tuning parameter selection for sparse hierarchical clustering
  f=file();sink(f)
  perm.out <- HierarchicalSparseCluster.permute(a[,-1], wbounds=c(1.5,2:6),nperms=5);
  sink();close(f)
  # print(perm.out)
  plot(perm.out)
  # Clustering
  f=file();sink(f)
  sparsehc <- HierarchicalSparseCluster(dists=perm.out$dists,wbound=perm.out$bestw, method="complete");
  sink();close(f)
  par(mfrow=c(1,2))
  plot(sparsehc)
  # print(sparsehc)
  # Plot dendogram with colors
  par(mfrow=c(1,1))
  ColorDendrogram(sparsehc$hc,y=y,main="My Simulated Data",branchlength=1)
  
  # Dendogram matrix
  MERGE <- sparsehc$hc$merge
  HEIGHT <- sparsehc$hc$height
  DEN <-cbind(MERGE,HEIGHT,deparse.level = 0) #Without labels
  nElements<-nrow(DEN)
  
  orderedWeights <- order(sparsehc$ws,decreasing=T)
  
  return(list(merge=MERGE,height=HEIGHT,den=DEN,nElements=nElements,
              orderedWeights=orderedWeights))
}
#########################################################################



#    CUT DENDOGRAM IN TWO MAIN CLUSTERS    ##############################
#########################################################################
cutDendogram2 <- function(DEN) {
  twoGroupsHeight <- length(DEN[,1])-1
  LimitHeight <- DEN[twoGroupsHeight,3]
  #Calculates the row which node is closest to the threshold
  MAXHEIGHT <- which(abs(DEN[,3]-LimitHeight)==min(abs(DEN[,3]-LimitHeight)))
  v1 <- DEN[1:MAXHEIGHT,2]>0 & DEN[1:MAXHEIGHT,1]<0
  v2 <- DEN[1:MAXHEIGHT,1]>0 & DEN[1:MAXHEIGHT,2]>0
  nDel <- length(v1[v1==T])+length(v2[v2==T])*2 # delete grows by 1 in case v1 and by 2 in case v2
  nGroups <- MAXHEIGHT-nDel
  
  for (i in 1:MAXHEIGHT) {
    if (DEN[i,1]<0 && DEN[i,2]<0) { # two points form a cluster
      assign(paste0("G",i),c(-DEN[i,1],-DEN[i,2]))
    } else if (DEN[i,2]>0 && DEN[i,1]<0) { # a point is added to a cluster
      assign(paste0("G",i),c(eval(as.symbol(paste0("G",DEN[i,2]))),-DEN[i,1]))
    } else { # two clusters are merged
      assign(paste0("G",i),c(eval(as.symbol(paste0("G",DEN[i,1]))),eval(as.symbol(paste0("G",DEN[i,2])))))
    }
  }
  
  GROUPS <- vector("list",nGroups)
  for (i in 0:(nGroups-1)) {
    GROUPS[[i+1]] <- eval(as.symbol(paste0("G",MAXHEIGHT-i)))
  }
  
  return(GROUPS)
}
#########################################################################



#   GROUP STATISTICS    #################################################
#########################################################################
computeStats <- function(GROUPS) {
  nGroups=length(GROUPS)
  #We create a list of the PISA results, dividing it into the obtained groups
  RESULTS_list <- vector("list",nGroups)
  for(i in 1:nGroups){
    RESULTS_list[[i]] <- a[GROUPS[[i]],1]
  }
  # print(RESULTS_list)
  means<-sapply(RESULTS_list, mean)
  standardDev<-sapply(RESULTS_list, sd)
  return(list(results=RESULTS_list,means=means,sd=standardDev))
}
#########################################################################



#    PERMUTATION TEST    ################################################
#########################################################################
permutationTest <- function(GROUPS) {
  # Compute 2 group mean and SD
  groupStats <- computeStats(GROUPS)
  RESULTS_list <- groupStats$results
  means <- groupStats$means
  standardDev <- groupStats$sd
  
  group1 <- RESULTS_list[[1]]
  group2 <- RESULTS_list[[2]]
  
  n1 <- length(group1)
  n2 <- length(group2)
  total <- n1+n2
  
  null.mean <- means[1]-means[2]
  
  ntests <- 10000
  vect <- c(group1,group2)
  st <- numeric(ntests)
  for(i in 1:ntests) {
    d <- sample(vect,total)
    st[i] <- mean(d[1:n1])-mean(d[(n1+1):total])
  }
  p <- length(st[st>null.mean])/ntests
  
  # print(paste("Mean of group1=",means[1]))
  # print(paste("Mean of group2=",means[2]))
  return(p)
}
#########################################################################



#    PRINT MOST RELEVANT FEATURES    ####################################
#########################################################################
mostRelevant <- function(a,a.ini) {
  relevantFeatures <- c()
  for(i in 2:ncol(a)) {
    for(j in 2:ncol(a.ini)) {
      if(all(a[,i]==a.ini[,j]))
        relevantFeatures <- c(relevantFeatures,j)
    }
  }
  return(relevantFeatures)
}
#########################################################################