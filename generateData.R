library(sparcl)
set.seed(1)
# CREATE DATA
features=9
observations=10
x <- matrix(c(rnorm(features*observations)),ncol=features)
#Vector to distinguish between relevant features classes
y <- c(rep(1,5),rep(2,5))
#Vector to distinguish between fake relevant features classes
z <- c(rep(1,3),rep(2,2),rep(3,3),rep(4,2))

#Relevant features
x[y==1,1] <- x[y==1,1]+5
x[y==1,2] <- x[y==1,2]+9
x[y==1,3] <- x[y==1,3]+6

x[y==2,1] <- x[y==2,1]+10
x[y==2,2] <- x[y==2,2]+4
x[y==2,3] <- x[y==2,3]+3

#Relevant fake features. Similarities bewteen different classes
x[z==1,4] <- x[z==1,4]+3
x[z==3,4] <- x[z==3,4]+3

x[z==1,5] <- x[z==1,5]+7
x[z==3,5] <- x[z==3,5]+7

x[z==2,6] <- x[z==2,6]+15
x[z==4,6] <- x[z==2,6]+15

#non relevant features. N(different averages and SE, but equivalent for all countries)
x[,7] <- rnorm(10, mean = 2, sd = 1)
x[,8] <- rnorm(10, mean = 10, sd = 0.5)
x[,9] <- rnorm(10, mean = 4, sd = 0.5)

#We create the matrix with the normalization of the features performed.
#The new values will range [0->1]
a <- matrix(c(rnorm(features*observations)),ncol=features)
for (i in 1:9){
  a[,i] <- (x[,i]-min(x[,i]))/(max(x[,i])-min(x[,i]))
  # a[,i] <- (x[,i]-mean(x[,i]))/(sd(x[,i]))
}


#PISA rates: in 2012 results -> max value=613; min value=368
PISA <- x <- matrix(c(rnorm(10,sd=40)),ncol=1)
ranking <-c(rep(1,5),rep(2,5))
PISA[ranking==1,] <- round(PISA[ranking ==1,]+550)
PISA[ranking==2,] <- round(PISA[ranking ==2,]+400)

a<-cbind(PISA,a)
scores <- a[,1]
scoresSorted <- sort(scores,decreasing = FALSE) #From lowest to greatest