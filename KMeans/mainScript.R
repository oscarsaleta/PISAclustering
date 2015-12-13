rm(list=ls())
library(sparcl)
# Change working directory to the source file location
source("clusteringLibrary.R")

# GENERATE FAKE DATA
gendata <- generateData2();
a.ini <- gendata$a
a <- a.ini
y <- gendata$y
scoresSorted <- getSortedScores(a);

# Compute dendogram and bootstrap test of initial data
source("initializeGroups.R")

maxRemovedFeatures <- 6
SIGNIFICANCE <- 0.1
if (p > SIGNIFICANCE) {
  # REPEAT THIS LOOP UNTIL STOPPING CONDITION IS MET
  repeat {
    if (removedFeatures >= maxRemovedFeatures)
      break
    # REMOVE FEATURE
    a.old <- a;
    orderedWeights.old <- orderedWeights;
    a <- removeFeature(a,orderedWeights,nFeature);
    
    # COMPUTE DENDOGRAM
    kmeans <- computeKMeans(a,y);
    orderedWeights <- kmeans$orderedWeights;
    
    # BOOTSTRAP TEST FOR TESTING EQUALITY OF MEANS
    GROUPS <- kmeans$groups
    p <- permutationTest(GROUPS);
    # print(paste("p-value=",p))
    # If p < 0.05 this means groups are different
    if (p < SIGNIFICANCE) {
      break;
    } else {
      if (p > p.old) {
        enworsement <- p-p.old;
        if (enworsement < enworsement.least) {
          lessBadFeature <- nFeature
          enworsement.least <- enworsement
          lessBadOrder <- orderedWeights
        }
        # If p has increased this means result is worse
        # Readd feature and remove next one in next iteration
        a <- a.old
        orderedWeights <- orderedWeights.old
        nFeature <- nFeature + 1
        if (nFeature >= 9-removedFeatures) {
          a <- removeFeature(a,lessBadOrder,lessBadFeature);
          removedFeatures <- removedFeatures+1
          print("Removed one feature")
          nFeature <- 1
        }
        next
      } else {
        # If result is better but not good enough
        # Keep removing features
        removedFeatures <- removedFeatures+1
        print("Removed one feature")
        nFeature <- 1
        p.old <- p
        next
      }#end of else
    }#end of else
  }#end of repeat
}#end of if

# ficar aqui script per mostrar features rellevants
print("Most relevant features:")
print(mostRelevant(a,a.ini))
print("Group1:")
print(GROUPS[[1]])
print("Group2:")
print(GROUPS[[2]])
print(paste("Last p-value =",p))