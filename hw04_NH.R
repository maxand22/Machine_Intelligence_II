# MI2 Sheet 04 (Kernel PCA)


library(ggplot2)
library(kernlab)

### 4.1)

# a)

rnorm2 <- function(n,mean,sd){
    mean+sd*scale(rnorm(n))
}

toy_data <- data.frame(X1 = c(rnorm2(30, -.5, .1), rnorm2(30, 0, .1), rnorm2(30, .5, .1)),
                       X2 = c(rnorm2(30, -.2, .1), rnorm2(30, .6, .1), rnorm2(30, 0, .1)))


# check the result
ggplot(toy_data, aes(x = X1, y = X2)) + 
    geom_point(size = 3) + 
    theme_bw() 



# b)

#kpc <- kpca(~., data=toy_data, kernel="rbfdot", kpar=list(sigma=0.2), features=2)

sqnorm <- function (x1, x2) {
    temp <- x1 - x2
    sum(temp * temp)
} 

rbf <- function(x1, x2, sigma){
    return(exp(-(sqnorm(x1,x2)/2*sigma^2)))
}


# generate empty kernel matrix
K <- matrix(nrow = nrow(toy_data), ncol = nrow(toy_data))


for(i in 1:nrow(toy_data)){
    for(j in 1:nrow(toy_data)){
        K[i, j] <- rbf(toy_data[i,], toy_data[j, ], sigma = .2)
    }
}


# centering
N <- matrix(1/nrow(K), nrow = nrow(K), ncol = ncol(K))

K_cen <- K - N%*%K - K%*%N + N%*%K%*%N


# 1/p*K*a_k  = λ_k*a_k
# --> multiply K_cen by 1/p and extract normalized eigenvectors

p = nrow(K_cen)

eigenvecs <- eigen(1/p*K_cen)$vectors
eigenvals <- eigen(1/p*K_cen)$values

# The coefficients are stored in the object eigenvecs



# c)

