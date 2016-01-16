# COMPUTE K-MEANS FOR ORIGINAL MATRIX
kmeans <- computeKMeans(a,y);
orderedWeights <- kmeans$orderedWeights;

# BOOTSTRAP TEST FOR TESTING EQUALITY OF MEANS
GROUPS <- kmeans$groups
p <- permutationTest(GROUPS);
print(paste("Initial p-value = ",p))

p.old <- p; a.old <- a; orderedWeights.old <- orderedWeights;
removedFeatures <- 0;
enworsement.least <- 1
nFeature <- 1
