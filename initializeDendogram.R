# COMPUTE DENDOGRAM FOR ORIGINAL MATRIX
dendogram <- computeDendogram(a,y);
MERGE <- dendogram$merge;
DEN <- dendogram$den;
HEIGHT <- dendogram$height;
nElements <- dendogram$nElements;
orderedWeights <- dendogram$orderedWeights;

# BOOTSTRAP TEST FOR TESTING EQUALITY OF MEANS
GROUPS <- cutDendogram2(DEN)
p <- permutationTest(GROUPS);
print(paste("Initial p-value = ",p))

p.old <- p; a.old <- a; orderedWeights.old <- orderedWeights;
removedFeatures <- 0;
enworsement.least <- 1
nFeature <- 1