rm(list=ls())
library(sparcl)
# Change working directory to the source file location
source("clusteringLibrary.R")

# GENERATE FAKE DATA
rdata <- readData("merged_features.csv");
a.ini <- rdata$a
a <- a.ini
y <- rdata$y
scoresSorted <- rdata$scores;

# Compute dendogram and bootstrap test of initial data
source("initializeDendogram.R")

maxRemovedFeatures <- 150
SIGNIFICANCE <- 0.015
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
        print(paste0("Readd one feature, p=",toString(p)))
        if (nFeature >= ncol(a)-2-removedFeatures) {
          a <- removeFeature(a,lessBadOrder,lessBadFeature);
          removedFeatures <- removedFeatures+1
          print(paste0("Removed one feature, p=",toString(p)))
          nFeature <- 1
        }
        next
      } else {
        # If result is better but not good enough
        # Keep removing features
        removedFeatures <- removedFeatures+1
        print(paste0("Removed one feature, p=",toString(p)))
        nFeature <- 1
        p.old <- p
        next
      }#end of else
    }#end of else
  }#end of repeat
}#end of if

if (removedFeatures == maxRemovedFeatures) {
  #fer que faci de nou el millor dendograma abans d'acabar
}


# ficar aqui script per mostrar features rellevants
print("Most relevant features:")
print(mostRelevant(a,a.ini))
print("Group1:")
print(sort(GROUPS[[1]]))
print("Group2:")
print(sort(GROUPS[[2]]))
print(paste("Last p-value =",p))
