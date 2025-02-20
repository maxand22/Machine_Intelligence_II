#a create a toy data set

#create random distribution data set

rnorm2 <- function(n,mean,sd){
  mean+sd*scale(rnorm(n),scale = FALSE)
}

sd = 0.1
mean1 <- c(-0.5, -0.2)
mean2 <- c(0, 0.6)
mean3 <- c(0.5, 0)
nrow <- 30

td <- data.frame(x1 = c(rnorm2(nrow, mean1[1], sd), rnorm2(nrow, mean2[1], sd), rnorm2(nrow, mean3[1], sd)), x2 = c(rnorm2(nrow, mean1[2], sd), rnorm2(nrow, mean2[2], sd), rnorm2(nrow, mean3[2], sd)))

##############################################################
#b
#Apply Kernel PCA using the RBF Kernel Function

rbf <- function (x1, x2, sig){
  return(exp((-(abs(x1-x2)^2))/((2*sig)^2)))
}


#Generate Kernel Matrix and create new feature space with Kernel Function
k <- matrix(nrow = nrow(td), ncol = nrow(td))
#k <- sapply(1:nrow(td), function(i) sapply(1:nrow(td), function(j) rbf(td[i,], td[j,], 0.4)))

for(i in 1:nrow(td)){
  for(j in 1:nrow(td)){
    k[i, j] <- rbf(td[i,], td[j, ], sig = .2)
  }
}

# centering
N <- matrix(1/nrow(k), nrow = nrow(k), ncol = ncol(k))
K_cen <- k - N%*%k - k%*%k + N%*%k%*%N


#################################################################
#d Discuss for which applications Kernel-PCA might be suitable.

# Kernel PCA kan be used to get ride of noise for image analysis (De-Noising in feature space). 
# The more Kernels we use the more non-linear relationships we can cover and
# reporduce in a later stage. Based on this, we can get ride of noise
# occuring in a linear PCA image application. 
# Furthermore, Kernel-PCA is suitable to detect non-linear (complex) patterns for example with SVMs.
# Since Kernel PCA finds non linear patterns in Data, it is used to detect such patterns
# e.g. in scientific data in earth Science, face recognition, active shape models, ECG data.
# It preservs the subspace that contains these patterns and discards the remaining space.


