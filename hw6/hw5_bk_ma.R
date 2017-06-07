#setwd("C:/Users/Benni/Box Sync/_studium/2. Semester/04_Machine Intelligence II/Exercises/05")
setwd("/Users/maxand22/Google Drive/Humboldt/4. Semester/Machine Intelligence II/Machine_Intelligence_II/hw5")

#packages
library(ggplot2)
library(audio)
library(corrplot)

##### Ex 5.1 -------------------------------------------------------------------------------

### a
# load data sets
sound1 <- t(read.csv("sound1.dat", header = FALSE))
sound2 <- t(read.csv("sound2.dat", header = FALSE))
S <- rbind(sound1,sound2)
# plot data
plot(1:18000, sound1, type = 'l')
plot(1:18000, sound2, type = 'l')
# play sound
sample1 <- audioSample(as.matrix(sound1), rate = 8192)
sample2 <- audioSample(as.matrix(sound2), rate = 8192)
play(sample1)
play(sample2)

### b
# random mixing matrix A
set.seed(22) # 112: "bad" representation -> more randomly mixed
A <- matrix(runif(4),2,2)
X <- A%*%S
# play and plot mix
plot(1:18000, t(X)[,1], type = 'l')
plot(1:18000, t(X)[,2], type = 'l')
play(audioSample(X[1,], rate = 8192))
play(audioSample(X[2,], rate = 8192))

### c
# permute columns of X
X_perm <- X[,sample(ncol(X))]

### d
cor_s <- data.frame(t(S))
colnames(cor_s) <- c("Source1", "Source2")
cor_x <- data.frame(t(X))
colnames(cor_x) <- c("Mix1", "Mix2")
cor(cor_s,cor_x)
corrplot(cor(cor_s,cor_x))

### e
# scaling (works better without)
#X0 <- X
#X <- scale(t(X), center = TRUE, scale = FALSE)

### f
set.seed(234)
W <- matrix(runif(4),2,2)


##### Ex 5.2 -----------------------------------------------------------------------------

# functions needed
fhat <- function(x){
  return(1/(1+exp(-x)))
}
f1 <- function(x){
  return(1-2*fhat(x))
}

### a
e0 <- 100
W_r <- W
for(alpha in 1:18000){
  eta <- e0/alpha
  x <- X[,alpha] 
  W_r <- W_r + eta*(t(solve(W_r)) + f1(W_r%*%cbind(x, x)*t(cbind(x,x))))
}

W_r
solve(A)

### b


### c
S_hat1 <- W_r %*% X
#S_hat1: plots and sounds
plot(1:18000, t(S_hat1)[,1], type = 'l')
plot(1:18000, t(S_hat1)[,2], type = 'l')
play(audioSample(S_hat1[1,], rate = 8192))
play(audioSample(S_hat1[2,], rate = 8192))
