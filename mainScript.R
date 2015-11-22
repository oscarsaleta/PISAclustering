rm(list=ls())
library(sparcl)
# Change working directory to the source file location

# Generate fake data
source("generateData.R")

percentageSuccessPrev <- c(0,0)
THRESHOLD_PERCENTAGE <- 70 #If all of the clusters achieve this threshold then good features choice
aPrev <- a
repeat {
  # Compute dendogram
  source("computeDendogram.R")
  
  # Compute groups at a height for 2 groups
  twoGroupsHeight <- length(DEN[,1])-1
  LimitHeight <- DEN[twoGroupsHeight,3]
  source("cutDendogram.R")
  
  # Compute group statistics
  source("groupStatistics.R")
  
  # Feature removal and result comparison
  source("featureSelection.R")
  
  # Stopping condition
  if ( (percentageSuccess[1]>=THRESHOLD_PERCENTAGE && percentageSuccess[2]>=THRESHOLD_PERCENTAGE) || length(WEIGHTS)<7 )
    break
  
}