install.packages("audio")
library(audio)

install.packages("ggplot2")
library(ggplot2)


path1 <- "/Users/maxand22/Google Drive/Humboldt/4. Semester/Machine Intelligence II/Machine_Intelligence_II/hw5/sound1.dat"
path2 <- "/Users/maxand22/Google Drive/Humboldt/4. Semester/Machine Intelligence II/Machine_Intelligence_II/hw5/sound2.dat"

#1-------------------------------------------------------------------------------
#1.a
sound1 <- read.csv(path1, header = FALSE)
#qplot(1:nrow(sound1), sound1)
source1 <- audioSample(t(as.matrix(sound1)), rate=8192)
play(source1)

sound2 <- read.csv(path2, header = FALSE)
#qplot(1:nrow(sound2), sound2)
source2 <- audioSample(t(as.matrix(sound2)), rate=8192)
play(source2)

#1.b
s <- matrix( data= c(t(as.matrix(sound1)), t(as.matrix(sound2))),nrow = 2)
a <- matrix(runif(4, max = 1, min = 0), 2, 2)
x <- a%*%s

#1.c
x_permute <- x[,sample(ncol(x))]
qplot(1:ncol(x_permute), x_permute[2,])

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
w <- matrix(runif(4, max = 1, min = 0), 2, 2)


#2--------------------------------------------------------------

# logistic function
f = function(x){
  return(1/(1+exp(-x)))
}


#a

# for random initialization of weights --> see 1.f)
t = 1
eta_zero = 0.1
alpha = 1
set.seed(2106)
w <- matrix(runif(4, max = 1, min = 0), 2, 2)

for(t in 1:18000){
  eta_t = eta_zero/t
  
  X = x[,alpha]
  
  w = w + eta_t*(t(solve(w)) + (1 - 2*f(w%*%cbind(X, X)))*t(cbind(X,X)))
  
  alpha = alpha + 1
  if(alpha == 18000){
    alpha = 1
  }
}

vvv = w%*%x
#w
#solve(a)

sound1_regular <- audioSample(t(as.matrix(vvv[1,])), rate = 8192/2)
play(sound1_regular)
qplot(1:ncol(vvv), vvv[1,])

sound2_regular <- audioSample(t(as.matrix(vvv[2,])), rate = 8192/2)
play(sound2_regular)
qplot(1:ncol(vvv), vvv[2,])


#b
t = 1
eta_zero = 0.1
alpha = 1
set.seed(2106)
w <- matrix(runif(4, max = 1, min = 0), 2, 2)

for(t in 1:18000){
  eta_t = eta_zero/t
  
  X = x[,alpha]
  
  w = w + eta_t*(((-1)*f(w%*%cbind(X, X))*(w%*%t(cbind(X,X)))) + (1 - 2*f(w%*%cbind(X, X)))*(w%*%t(cbind(X,X))))%*%w
  
  alpha = alpha + 1
  if(alpha == 18000){
    alpha = 1
  }
}

vvv_natural = w%*%x
w
solve(a)

sound1_natural <- audioSample(t(as.matrix(vvv_natural[1,])), rate = 8192/2)
play(sound1_natural)
qplot(1:ncol(vvv), vvv_natural[1,])

sound2_natural <- audioSample(t(as.matrix(vvv_natural[2,])), rate = 8192/2)
play(sound2_natural)
qplot(1:ncol(vvv), vvv_natural[2,])
