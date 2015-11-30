rm(list=ls())
library(sparcl)
# Change working directory to the source file location

# GENERATE FAKE DATA
source("generateData.R")
# if we change this for a script that reads data we have to make sure the structure is the same
# as in matrix 'a'


# REPEAT THIS LOOP UNTIL STOPPING CONDITION IS MET
repeat {

  # REPEAT INNER LOOP UNTIL THE REMOVED FEATURE IMPROVES THE RESULT
  percentageSuccessPrev <- c(0,0)
  THRESHOLD_PERCENTAGE <- 70 #If all of the clusters achieve this threshold then good features choice
  aPrev <- a
  repeat {
    # COMPUTE DENDOGRAM
    source("computeDendogram.R")
    
    # COMPUTE GROUPS AT A HEIGHT FOR 2 GROUPS
    twoGroupsHeight <- length(DEN[,1])-1
    LimitHeight <- DEN[twoGroupsHeight,3]
    source("cutDendogram.R")
    
    # COMPUTE GROUP STATISTICS
    source("groupStatistics.R")
    
    # BOOTSTRAP TEST FOR TESTING EQUALITY OF MEANS
    source("bootstrapTest.R")
    # Exit if the groups are different
    if (groupsAreDifferent) {
      break
    } else {
      # Check if the result has improved
      source("removeFeature.R")
      if (pvalueImproved) {
        break
      }
    }
    
    # FEATURE REMOVAL AND RESULT COMPARISON
    # source("featureSelection.R")
    
    # Stopping condition
    # if((percentageSuccess[1]>=THRESHOLD_PERCENTAGE && percentageSuccess[2]>=THRESHOLD_PERCENTAGE)||length(WEIGHTSfirst)<5)
      # break
    
  }
}

# ficar aqui script per mostrar features rellevants