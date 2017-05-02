######## Exercise 2 ########

#setwd("C:/Users/Benni/Box Sync/_studium/2. Semester/04_Machine Intelligence II/Exercises/02")


# libraries
library(ggplot2)


#### 2.1 ####

## (a) ##

# load data
pcaData2d <- read.csv("Ex2/pca-data-2d.txt", sep = "", header = FALSE)

# calcualte mean matrix
n <- nrow(pcaData2d)
M_mean <- matrix(data=1, nrow=n) %*% cbind(mean(pcaData2d[,1]),
                                           mean(pcaData2d[,2]))

# subtract mean from data set -> "Difference Matrix"
pcaData2d_cen <- pcaData2d - M_mean

# plot centered matrix
ggplot(pcaData2d_cen, aes(x=pcaData2d_cen$V1, y=pcaData2d_cen$V2)) + geom_point()


## (b) ##

# convert df to matrix
D <- as.matrix(pcaData2d_cen)

# compute Covariance Matrix C
C <- (n-1)^-1 * t(D) %*% D

# Eigenvalues of C, Eigenvectors in P
ev <- eigen(C)
P  <- ev$vectors

# transform D
D_tr <- P %*% t(D)

# in ggplot2 plottable data frame
X_pca <- as.data.frame(t(D_tr)) 

# plot of the transformed set
ggplot(X_pca, aes(x=X_pca$V1, y=X_pca$V2)) + geom_point()


## (c) ##

# plot only PC1 
ggplot(X_pca, aes(x=X_pca$V1, y=0)) + geom_point()

# plot only PC2
ggplot(X_pca, aes(x=0, y= X_pca$V2)) + geom_point()



#### 2.2 ####

## (a) ##

# load data set
pcaData3d <- read.csv("Ex2/pca-data-3d.txt")

# centering
n_3 <- nrow(pcaData3d)
M3_mean <- matrix(data=1, nrow=n_3) %*% colMeans(pcaData3d)

# subtract mean from data set -> "Difference Matrix"
D3 <- pcaData3d - M3_mean

# scatterplot matrix
pairs(D3)


## (b) ##

# convert df to matrix
D3 <- as.matrix(D3)

# compute Covariance Matrix C
C3 <- (n_3-1)^-1 * t(D3) %*% D3

# Eigenvalues of C3, Eigenvectors in P3
ev3 <- eigen(C3)
P3  <- ev3$vectors

# transform D3
D3 <- P3 %*% t(D3)
X_3pca <- as.data.frame(t(D3)) 
pairs(X_3pca)
