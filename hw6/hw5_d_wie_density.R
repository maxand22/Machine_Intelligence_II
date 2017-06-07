setwd("/Users/maxand22/Google Drive/Humboldt/4. Semester/Machine Intelligence II/Machine_Intelligence_II/hw5")

#install.packages("audio")
library(audio)

#install.packages("ggplot2")
library(ggplot2)

#For every 1000th update w function
lr_func <- function(x){
  result <- 0
  for(i in 1:2){
    for(j in 1:2){
      result <- result + x[i,j]^2
    }
  }
  return(result)
}

# logistic function
f = function(x){
  return(1/(1+exp(-x)))
}


#1-------------------------------------------------------------------------------
#1.a
sound1 <- read.csv("sound1.dat", header = FALSE)
#qplot(1:nrow(sound1), sound1)
source1 <- audioSample(t(as.matrix(sound1)), rate=8192)
#play(source1)

sound2 <- read.csv("sound2.dat", header = FALSE)
#qplot(1:nrow(sound2), sound2)
source2 <- audioSample(t(as.matrix(sound2)), rate=8192)
#play(source2)

#1.b
set.seed(1234)
s <- t(as.matrix(data.frame(sound1, sound2)))
#qplot(1:ncol(s), s[1,])
a <- matrix(runif(4, max = 1, min = 0), 2, 2)
x <- a%*%s
#x = t(scale(t(x), center = TRUE, scale = FALSE))
#qplot(1:ncol(x), x[1,])
#qplot(1:ncol(x), x[2,])

#1.c
#set.seed(123)
x_permute <- x[,sample(ncol(x))]
#qplot(1:ncol(x_permute), x_permute[2,])

#1.c
correlation <- matrix(1, nrow = nrow(x),ncol=nrow(s))

for (i in 1:nrow(x)) {
  for (j in 1:nrow(s)) {
    correlation[i, j] <- cor(s[j,], x_permute[i,]) 
  }
}

#1.d
x_permute_center <- scale(t(x_permute), center = TRUE, scale = FALSE)
#round(colMeans(x_permute_center),2)

#1.f
#w <- matrix(runif(4, max = 1, min = 0), 2, 2)


#2--------------------------------------------------------------

#a

# for random initialization of weights --> see 1.f)
t = 1
eta_zero = 0.1
alpha = 1
set.seed(123)
w <- matrix(runif(4, max = 1, min = 0), 2, 2)
lr_dev1 <- matrix(NA, nrow = 18,ncol = 2)

for(t in 1:18000){
  eta_t = eta_zero/t
  
  X = x[,alpha]
  
  w = w + eta_t*(t(solve(w)) + (1 - 2*f(w%*%cbind(X, X)))*t(cbind(X,X)))
  
  if(alpha %% 1000 == 0){
    lr_dev1[alpha/1000,1] <- lr_func(eta_t*(t(solve(w)) + f(w%*%cbind(X, X)*t(cbind(X,X)))))
  }
  
  alpha = alpha + 1
  if(alpha == 18000){
    alpha = 1
  }
}

vvv = w%*%x
w
solve(a)

sound1_regular <- audioSample(t(as.matrix(vvv[1,])), rate = 8192)
play(sound1_regular)
qplot(1:ncol(vvv), vvv[1,])

sound2_regular <- audioSample(t(as.matrix(vvv[2,])), rate = 8192)
play(sound2_regular)
qplot(1:ncol(vvv), vvv[2,])


#b
t = 1
eta_zero = 50
alpha = 1
set.seed(123)
w <- matrix(runif(4, max = 1, min = 0), 2, 2)
lr_dev2 <- matrix(NA, nrow = 18,ncol = 2)


for(t in 1:18000){
  eta_t = eta_zero/t
  
  X = x[,alpha]
  
  w = w + eta_t*(((-1)*f(w%*%cbind(X, X))*(w%*%t(cbind(X,X)))) + (1 - 2*f(w%*%cbind(X, X)))*(w%*%t(cbind(X,X))))%*%w
  
  if(alpha %% 1000 == 0){
    lr_dev2[alpha/1000,1] <- lr_func(eta_t*(t(solve(w)) + f(w%*%cbind(X, X)*t(cbind(X,X)))))
  }
  
  alpha = alpha + 1
  if(alpha == 18000){
    alpha = 1
  }
}

vvv_natural = w%*%x
w
solve(a)

sound1_natural <- audioSample(t(as.matrix(vvv_natural[1,])), rate = 8192)
play(sound1_natural)
qplot(1:ncol(vvv), vvv_natural[1,])

sound2_natural <- audioSample(t(as.matrix(vvv_natural[2,])), rate = 8192)
play(sound2_natural)
qplot(1:ncol(vvv), vvv_natural[2,])

cor(t(s),t(vvv_natural))


#3 -------------------------------------------------------------------------------------

#a
#Plot & Play (i) the original sounds
qplot(1:nrow(sound1), sound1)
play(source1)

qplot(1:nrow(sound2), sound2)
play(source2)

#(ii) the mixed sources
qplot(1:ncol(x), x[1,])
x1 <- audioSample(t(as.matrix(x[1,])), rate=8192)
play(x1)

qplot(1:ncol(x), x[2,])
x2 <- audioSample(t(as.matrix(x[2,])), rate=8192)
play(x2)

#(ii) the mixed sources (after the data permutation)
qplot(1:ncol(x), x_permute[1,])
x1_permute <- audioSample(t(as.matrix(x_permute[1,])), rate=8192)
play(x1_permute)

qplot(1:ncol(x), x_permute[2,])
x2_permute <- audioSample(t(as.matrix(x_permute[2,])), rate=8192)
play(x2_permute)

#the recovered signals (estimated sources) ˆs = Wx using the unpermuted data.
#regular
sound1_regular <- audioSample(t(as.matrix(vvv[1,])), rate = 8192)
play(sound1_regular)
qplot(1:ncol(vvv), vvv[1,])

sound2_regular <- audioSample(t(as.matrix(vvv[2,])), rate = 8192)
play(sound2_regular)
qplot(1:ncol(vvv), vvv[2,])

#natural
sound1_natural <- audioSample(t(as.matrix(vvv_natural[1,])), rate = 8192)
play(sound1_natural)
qplot(1:ncol(vvv), vvv_natural[1,])

sound2_natural <- audioSample(t(as.matrix(vvv_natural[2,])), rate = 8192)
play(sound2_natural)
qplot(1:ncol(vvv), vvv_natural[2,])

#b
library(corrplot)
# regular gradient
cor(t(s),t(vvv))
corrplot(cor(t(s),t(vvv)))
# natural gradient
cor(t(s),t(vvv_natural))
corrplot(cor(t(s),t(vvv_natural)))

#c
#learning rate for regular gradient
qplot(1:nrow(lr_dev1),lr_dev1[,1]) + geom_line()

#learning rate for natural gradient
qplot(1:nrow(lr_dev2),lr_dev2[,1]) + geom_line()

#for whitening ********


#d like density ;)
#mixed
x = t(scale(t(x), center = TRUE, scale = FALSE))
x1_d <- density(x[1,])
plot(x1_d)

x2_d <- density(x[2,])
plot(x2_d)

#unmixed
vvv1_d <- density(vvv[1,])
plot(vvv1_d)

vvv2_d <- density(vvv[2,])
plot(vvv2_d)

#true signals
s1_d <- density(s[1,])
plot(s1_d)

s2_d <- density(s[2,])
plot(s2_d)
