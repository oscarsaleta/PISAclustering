# SPARSE
# par(mfrow=c(1,1))
# Tuning parameter selection for sparse hierarchical clustering
perm.out <- HierarchicalSparseCluster.permute(a[,-1], wbounds=c(1.5,2:6),nperms=5)
# print(perm.out)
# plot(perm.out)
# Clustering
sparsehc <- HierarchicalSparseCluster(dists=perm.out$dists,wbound=perm.out$bestw, method="complete")
par(mfrow=c(1,2))
plot(sparsehc)
plot(sparsehc$hc, labels=rep("", length(y)))
# print(sparsehc)
# Plot dendogram with colors
par(mfrow=c(1,1))
ColorDendrogram(sparsehc$hc,y=y,main="My Simulated Data",branchlength=1)

# Dendogram matrix
MERGE <- sparsehc$hc$merge
HEIGHT <- sparsehc$hc$height
DEN <-cbind(MERGE,HEIGHT,deparse.level = 0) #Without labels
nElements<-nrow(DEN)