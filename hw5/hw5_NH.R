# Sheet 05 ICA, Infomax

library(audio)
library(ggplot2)


#setwd("/Users/maxand22/Google Drive/Humboldt/4. Semester/Machine Intelligence II/Machine_Intelligence_II")

    # Ex. 1 -------------------------------------------------------------------

# a)

s1 = read.table("hw5/sound1.dat", header=FALSE)
s2 = read.table("hw5/sound2.dat", header=FALSE)

S = as.matrix(data.frame(s1,s2))

plot(1:18000, s1$V1, type = 'l')
plot(1:18000, s2$V1, type = 'l')


play(audioSample(t(as.matrix(s1)), rate = 8192))


play(audioSample(t(as.matrix(s2)), rate = 8192))


# b)

set.seed(123)
A = matrix(runif(4, 0, 2), nrow = 2)

X = S%*%A

play(audioSample(t(as.matrix(X[,1])), rate = 8192))
play(audioSample(t(as.matrix(X[,2])), rate = 8192))

# c)

#X = X[sample(nrow(X)), ]

play(audioSample(t(as.matrix(X[,1])), rate = 8192))
play(audioSample(t(as.matrix(X[,2])), rate = 8192))

# d)

cor(S, X)


# e)

X = scale(X, center = TRUE, scale = FALSE)

round(colMeans(X), 8)

# f)

set.seed(2106)
W = matrix(runif(4, 0, 1), ncol = 2)





# Ex. 2 -------------------------------------------------------------------

# logistic function
f = function(x){
    return(1/(1+exp(-x)))
}

fdd_over_fd = function(x){
    return(1 - 2*f(x))
}


# a)

# for random initialization of weights --> see 1.f)
t = 1
eta_zero = 0.1
alpha = 1
set.seed(2106)
W = matrix(runif(4, 0, 1), ncol = 2)

for(t in 1:100){
    eta_t = eta_zero/t
    
    for(alpha in 1:18000){
        
        x = X[alpha,]
        
        W = W + eta_t*(t(solve(W)) + (1 - 2*f(W %*% cbind(x, x)))*t(cbind(x,x)))
        
    }
}

alpha = alpha + 1
if(alpha == 18000){
    alpha = 1
}

#vvv = X%*%solve(A)
vvv = X%*%W
play(audioSample(t(as.matrix(vvv[,1])), rate = 8192))
play(audioSample(t(as.matrix(vvv[,2])), rate = 8192))




W
solve(A)

qplot(1:nrow(vvv), vvv[,1])




# b)

