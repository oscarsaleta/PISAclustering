rm(list=ls())
library(sparcl)
# Change working directory to the source file location
source("clusteringLibrary.R")

# GENERATE FAKE DATA
gendata <- generateData();
a.ini <- gendata$a
a <- a.ini
y <- gendata$y
scoresSorted <- getSortedScores(a);

# Compute dendogram and bootstrap test of initial data
source("initializeDendogram.R")

maxRemovedFeatures <- 6
if (p>0.05) {
  # REPEAT THIS LOOP UNTIL STOPPING CONDITION IS MET
  repeat {
    if (removedFeatures >= maxRemovedFeatures)
      break
    # REMOVE FEATURE
    a.old <- a;
    orderedWeights.old <- orderedWeights;
    a <- removeFeature(a,orderedWeights,nFeature);
    
    # COMPUTE DENDOGRAM
    dendogram <- computeDendogram(a,y);
    MERGE <- dendogram$merge;
    DEN <- dendogram$den;
    HEIGHT <- dendogram$height;
    nElements <- dendogram$nElements;
    orderedWeights <- dendogram$orderedWeights;
    
    # BOOTSTRAP TEST FOR TESTING EQUALITY OF MEANS
    GROUPS <- cutDendogram2(DEN)
    p <- permutationTest(GROUPS);
    # print(paste("p-value=",p))
    # If p < 0.05 this means groups are different
    if (p < 0.05) {
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
dendogram <- computeDendogram(a,y)

# ficar aqui script per mostrar features rellevants
print(mostRelevant(a,a.ini))