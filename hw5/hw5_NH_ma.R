# Sheet 05 ICA, Infomax

library(audio)
library(ggplot2)

setwd("/Users/maxand22/Google Drive/Humboldt/4. Semester/Machine Intelligence II/Machine_Intelligence_II/")

# Ex. 1 -------------------------------------------------------------------

# a)

s1 = read.table("hw5/sound1.dat", header=FALSE)
s2 = read.table("hw5/sound2.dat", header=FALSE)

S = t(as.matrix(data.frame(s1,s2)))

plot(1:18000, s1$V1, type = 'l')
plot(1:18000, s2$V1, type = 'l')


play(audioSample(t(as.matrix(s1)), rate = 8192))
play(audioSample(t(as.matrix(s2)), rate = 8192))


# b)

set.seed(1234)
A = matrix(runif(4, 0, 1), nrow = 2)

X = A%*%S

play(audioSample(t(as.matrix(X[1,])), rate = 8192))
play(audioSample(t(as.matrix(X[2,])), rate = 8192))

# c)

X_perm = X[,sample(ncol(X))]

play(audioSample(t(as.matrix(X_perm[1,])), rate = 8192))
play(audioSample(t(as.matrix(X_perm[2,])), rate = 8192))

# d)

cor(S[1,], X[1,])
cor(S[1,], X[2,])
cor(S[2,], X[1,])
cor(S[2,], X[2,])

# e)

X[1,] = X[1,] - mean(X[1,])
X[2,] = X[2,] - mean(X[2,])

round(rowMeans(X), 8)

# f)

set.seed(123)
W = matrix(runif(4, 0, 1), ncol = 2)





# Ex. 2 -------------------------------------------------------------------

# logistic function
f = function(x){
    return(1/(1+exp(-x)))
}

ggplot(data.frame(x = c(-5, 5)), aes(x)) + stat_function(fun = f)


# a)

# for random initialization of weights --> see 1.f)
t = 1
eta_zero = .5
alpha = 1
set.seed(123)
W = matrix(runif(4, 0, 1), ncol = 2)

unmixing_regular = function(W, X, n_steps = 18000){
    for(t in 1:n_steps){
        eta_t = eta_zero/t
        
        x = X[,alpha]
        
        W_inv = t(solve(W))
        
        f_wx = 1 - 2*f(W%*%cbind(x,x))
        
        W_delta = eta_t*(W_inv + f_wx*t(cbind(x,x)))
        
        W = W + W_delta
        
        alpha = alpha + 1
        if(alpha == n_steps){
            alpha = 1
        }
    }
    return(W)
}

W_regular = unmixing_regular(W, X)
S_hat = W_regular%*%X

play(audioSample(t(as.matrix(scale(S_hat[1,]))), rate = 8192))
play(audioSample(t(as.matrix(scale(s1))), rate = 8192))

play(audioSample(t(as.matrix(scale(S_hat[2,]))), rate = 8192))
play(audioSample(t(as.matrix(scale(s2))), rate = 8192))

# b)

t = 1
eta_zero = 1
alpha = 1
set.seed(123)
W = matrix(runif(4, 0, 1), ncol = 2)

unmixing_natural = function(W, X, n_steps = 18000){
    d = matrix(c(0,0,0,0), ncol = 2)
    
    for(i in 1:ncol(X)){
        x = X[,i]
        f_wx = 1 - 2*f(W%*%cbind(x,x))
        wx = W%*%cbind(x,x)
        d = d + f_wx%*%wx
    }
    
    for(t in 1:n_steps){
        eta_t = eta_zero/t
        
        x = X[,alpha]
        
        f_wx = 1 - 2*f(W%*%cbind(x,x))
        
        wx = W%*%cbind(x,x)
        
        k_delta = -1/ncol(X)*d
        
        
        W_delta = eta_t*((k_delta + f_wx%*%wx)%*%W)
        
        W = W + W_delta
        
        alpha = alpha + 1
        if(alpha == n_steps){
            alpha = 1
        }
    }
    return(W)
}

W_natural = unmixing_natural(W, X)

S_hat_natural = W_natural%*%X
play(audioSample(t(as.matrix(S_hat_natural[1,])), rate = 8192))
play(audioSample(t(as.matrix(S_hat_natural[2,])), rate = 8192))



# Ex. 3 -------------------------------------------------------------------


# a)

# original sounds
plot(1:18000, s1$V1, type = 'l')
plot(1:18000, s2$V1, type = 'l')
play(audioSample(t(as.matrix(s1)), rate = 8192))
play(audioSample(t(as.matrix(s2)), rate = 8192))

# mixed
plot(1:18000, X[1,], type = 'l')
plot(1:18000, X[2,], type = 'l')
play(audioSample(t(as.matrix(X[1,])), rate = 8192))
play(audioSample(t(as.matrix(X[2,])), rate = 8192))

# permuted
plot(1:18000, X_perm[1,], type = 'l')
plot(1:18000, X_perm[2,], type = 'l')
play(audioSample(t(as.matrix(X_perm[1,])), rate = 8192))
play(audioSample(t(as.matrix(X_perm[2,])), rate = 8192))

# recovered regular
plot(1:18000, S_hat[1,], type = 'l')
plot(1:18000, S_hat[2,], type = 'l')
play(audioSample(t(as.matrix(X_perm[1,])), rate = 8192))
play(audioSample(t(as.matrix(X_perm[2,])), rate = 8192))

# recovered natural
plot(1:18000, S_hat_natural[1,], type = 'l')
plot(1:18000, S_hat_natural[2,], type = 'l')
play(audioSample(t(as.matrix(X_perm[1,])), rate = 8192))
play(audioSample(t(as.matrix(X_perm[2,])), rate = 8192))



# b)
#cor regular and source
cor(t(S),t(S_hat))
cor(S[1,], S_hat[1,])
cor(S[1,], S_hat[2,])
cor(S[2,], S_hat[1,])
cor(S[2,], S_hat[2,])

#cor natural and source
cor(t(S),t(S_hat_natural))
cor(S[1,], S_hat_natural[1,])
cor(S[1,], S_hat_natural[2,])
cor(S[2,], S_hat_natural[1,])
cor(S[2,], S_hat_natural[2,])

# c)

convergence_speed_regular = function(W, X, n_steps = 18000){
    c_rate = c()
    for(t in 1:n_steps){
        eta_t = eta_zero/t
        
        x = X[,alpha]
        
        W_inv = solve(W)
        
        f_wx = 1 - 2*f(W%*%cbind(x,x))
        
        W_delta = eta_t*(W_inv + f_wx*rbind(x,x))
        
        W = W + W_delta
        
        if(t %% 1000 == 0){
            c_rate = c(c_rate, sum(W^2))
        }
        
        alpha = alpha + 1
        if(alpha == n_steps){
            alpha = 1
        }
    }
    return(c_rate)
}




convergence_speed_natural = function(W, X, n_steps = 18000){
    c_rate = c()
    for(t in 1:n_steps){
        eta_t = eta_zero/t
        
        x = X[,alpha]
        
        W_inv = solve(W)
        
        f_wx = 1 - 2*f(W%*%cbind(x,x))
        
        W_delta = eta_t*((W_inv + f_wx*rbind(x,x))%*%t(W)%*%W)
        
        W = W + W_delta
        
        if(t %% 1000 == 0){
            c_rate = c(c_rate, sum(W^2))
        }
        
        alpha = alpha + 1
        if(alpha == n_steps){
            alpha = 1
        }
    }
    return(c_rate)
}


evec = eigen(cov(t(X)))$vectors
evals = eigen(cov(t(X)))$values

X_whitened_t = t(X) %*% evec %*% diag((evals)^-0.5)
round(cor(X_whitened_t), 5)

X_whitened = t(X_whitened_t)

conv_regular = convergence_speed_regular(W, X)
conv_natural = convergence_speed_natural(W, X)
conv_regular_whitened = convergence_speed_regular(W, X_whitened)
conv_natural_whitened = convergence_speed_natural(W, X_whitened)

qplot(1:length(conv_regular),conv_regular)
qplot(1:length(conv_natural),conv_natural)

qplot(1:length(conv_regular_whitened),conv_regular_whitened)
qplot(1:length(conv_natural_whitened),conv_natural_whitened)

# d)

#mixed
x1_d <- density(X[1,])
plot(x1_d)

x2_d <- density(X[2,])
plot(x2_d)

#unmixed regular
S_hat1_d <- density(S_hat[1,])
plot(S_hat1_d)

S_hat2_d <- density(S_hat[2,])
plot(S_hat2_d)

#unmixed natural
S_hat_natural1_d <- density(S_hat_natural[1,])
plot(S_hat_natural1_d)

S_hat_natural2_d <- density(S_hat_natural[2,])
plot(S_hat_natural2_d)

#true signals
s1_d <- density(S[1,])
plot(s1_d)

s2_d <- density(S[2,])
plot(s2_d)

#you see that a lot of values are around zero (low) for the birds and  
