###################################
#           STATISTICS            #
###################################
#We compute the mean and standard deviation for the obtained groups and 1) Between gropus comparison 2) Comparison with PISA results --> Iterate

#We create a list of the PISA results, dividing it into the obtained groups
RESULTS_list <- vector("list",nGroups)
for(i in 1:nGroups){
  RESULTS_list[[i]] <- a[GROUPS[[i]],1]
}
# print(RESULTS_list)
means<-sapply(RESULTS_list, mean)
standardDev<-sapply(RESULTS_list, sd)
