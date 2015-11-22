#Calculates the row which node is closest to the threshold
# LimitHeight <- DEN[8,3] #Then, a loop of heights may be applied to match and optimize statistics
MAXHEIGHT <- which(abs(DEN[,3]-LimitHeight)==min(abs(DEN[,3]-LimitHeight)))
# print(paste0("MAXHEIGHT: ",MAXHEIGHT))
# print(paste0("LimitHeight: ",LimitHeight))


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
print("GROUPS")
print(GROUPS)
