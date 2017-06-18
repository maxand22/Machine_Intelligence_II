##### Exercise 7 #####

setwd("C:/Users/Benjamin/Box Sync/_studium/2. Semester/04_Machine Intelligence II/Exercises/07")
# packages
library(audio)
library(fastICA)
library(psych)

#custom functions
norm_vec <- function(x) sqrt(sum(x^2))

g1 <- function(x) {tanh(x)}

### Ex2 ###

### Load Data
s1 = read.table("sound1.dat", header=FALSE)
s2 = read.table("sound2.dat", header=FALSE)

S = t(as.matrix(data.frame(s1,s2)))

# whiten S
St <- t(S)
covS <- cov(scale(St, center = TRUE, scale = FALSE))
ev <- eigen(covS)$vectors
eval <- eigen(covS)$values
Zt = scale(St, center = TRUE, scale = FALSE) %*% ev %*% diag((eval)^-0.5)
cov(Zt)
# whitened Data: Z
Z <- t(Zt)

# initialize mixing Matrix A and compute Mixed Signal X
set.seed(1234)
A = matrix(runif(4, 0, 1), nrow = 2)

#mixted signals
X = A%*%Z


# one unit alogorithm (nur eine independent component wird extrahiert)

# init random vector with length 1
vec <- matrix(c(0,1), nrow = 2, ncol = 1)

# inital learning rate
eta <- 0.01

for(i in 1:18000){
  
  eta <- eta*0.9999
    
  deltaVec <- eta*(-1) * X[,i] * g1(t(vec)%*%X[,i])
  
  vec <- vec + deltaVec
  
  vec <- vec/norm_vec(vec)
}

#check
shat <- t(vec) %*% X
plot(t(shat), type = 'l')
plot(1:18000, t(s1),type = 'l')
plot(1:18000, t(s2),type = 'l')
play(audioSample(as.matrix(shat), rate = 8192))


##### multiple unit alogorithm (beide independent components sollen extrahiert werden)

# init random matrix (two orthogonal vectors, each with length 1)
set.seed(123)
b1 <- matrix(runif(2, min = 0, max = 1), nrow = 2, ncol = 1)
b1 <- b1/norm_vec(b1)
b2 <- matrix(c(1, (-b1[2]/b1[1])), nrow = 2, ncol = 1)
b2 <- b2/norm_vec(b2)

#initialized unmixing matrix
B <- cbind(b1,b2)

# inital learning rate
eta2 <- 0.01

for(i in 1:18000){
  #learning rate
  eta2 <- eta2*0.9999
  
  #update b1
  deltaB1 <- eta2*(-1) * X[,i] * g1(t(b1)%*%X[,i])
  b1 <- b1 + deltaB1
  
  #update b2
  deltaB2 <- eta2*(-1) * X[,i] * g1(t(b2)%*%X[,i])
  b2 <- b2 + deltaB2
  
  #recalculate B
  B <- cbind(b1,b2)
  
  #normalize B
  maxNorm <- max(norm_vec(b1), norm_vec(b2))
  B <- B/maxNorm
  
  #decorrelate: FUNKTIONIERT NICHT! B danach linear abhängig
  Q <- eigen(cov(B%*%t(B)))$vectors
  EV <- eigen(cov(B%*%t(B)))$values
  Lambda <- diag(EV^-0.5)
  B <- Q %*% Lambda %*% t(Q) %*% B
  
  #normalize B
  #b1 <- B[,1]
  #b2 <- B[,2]
  #maxNorm <- max(norm_vec(b1), norm_vec(b2))
  #B <- B/maxNorm
  #b1 <- b1/norm_vec(b1)
  #b2 <- b2/norm_vec(b2)
  
  #B <- ((B%*%t(B))^-.5) %*% B
}
