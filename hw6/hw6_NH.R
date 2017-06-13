# HW06 NH

#install.packages("R.matlab")
library(R.matlab)
library(ggplot2)

l = readMat("hw6/distrib.mat")

s1 = l[[1]]
s2 = l[[2]]
s3 = l[[3]]

# a)
A = matrix(4:1, ncol = 2, byrow = TRUE)

x1 = A%*%s1
x2 = A%*%s2
x3 = A%*%s3


# b)
x1[1,] = x1[1,] - mean(x1[1,])
x1[2,] = x1[2,] - mean(x1[2,])

x2[1,] = x2[1,] - mean(x2[1,])
x2[2,] = x2[2,] - mean(x2[2,])

x3[1,] = x3[1,] - mean(x3[1,])
x3[2,] = x3[2,] - mean(x3[2,])


# c)
pcs1 = eigen(cov(t(x1)))$vectors
pcs2 = eigen(cov(t(x2)))$vectors
pcs3 = eigen(cov(t(x3)))$vectors

evals1 = eigen(cov(t(x1)))$values
evals2 = eigen(cov(t(x2)))$values
evals3 = eigen(cov(t(x3)))$values

proj1 = t(x1) %*% pcs1
proj2 = t(x2) %*% pcs2
proj3 = t(x3) %*% pcs3

round(cov((proj1)), 5)
round(cov((proj2)), 5)
round(cov((proj3)), 5)



# d)
proj1_w = t(x1) %*% pcs1 %*% diag(evals1^(-.5))
proj2_w = t(x2) %*% pcs2 %*% diag(evals2^(-.5))
proj3_w = t(x3) %*% pcs3 %*% diag(evals3^(-.5))

round(cov((proj1_w)), 5)
round(cov((proj2_w)), 5)
round(cov((proj3_w)), 5)


# e)

rotation_matrix = function(theta){
    return(matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), ncol = 2, byrow = TRUE))
}

thetas = seq(0, 2*pi, pi/50)

kurt = function(x){
    k = 1/length(x)*sum(x^4) - 3
    return(k)
}

kurt_rotation = function(x, thetas){
    
    k_mat = matrix(nrow = length(thetas), ncol = 2)
    i = 1
    
    if(nrow(x) > ncol(x)){
        x = t(x)
    }
    
    for(t in thetas){
        R = rotation_matrix(t)
        
        x_theta = t(R%*%x)
        k = apply(x_theta, 2, kurt)
        
        k_mat[i,] = k
        i = i + 1
        
    }
    return(k_mat)
}

k_after_rotation1 = kurt_rotation(proj1_w, thetas)
k_after_rotation2 = kurt_rotation(proj2_w, thetas)
k_after_rotation3 = kurt_rotation(proj3_w, thetas)


# f)
max1 = which.max(k_after_rotation1[,1])
max2 = which.max(k_after_rotation2[,1])
max3 = which.max(k_after_rotation3[,1])

R1 = rotation_matrix(thetas[max1])
R2 = rotation_matrix(thetas[max2])
R3 = rotation_matrix(thetas[max3])

proj1_w_rotated = R1 %*% t(proj1_w)
proj2_w_rotated = R2 %*% t(proj2_w)
proj3_w_rotated = R3 %*% t(proj3_w)


# Plotting

# original

m <- rbind(c(1, 2), c(1, 3))
layout(m, widths = 1)

plot(s1[1,], s1[2,], col = alpha("blue", .3), pch = 16,
     main = "Normal (original sources)", xlab = "s1", ylab = "s2")
hist(s1[1,], main = "Normal s1 original", xlab = "")
hist(s1[2,], main = "Normal s2 original", xlab = "")

plot(proj1_w_rotated[1,], proj1_w_rotated[2,], col = alpha("blue", .3), pch = 16,
     main = "Normal (whitened & rotated)", xlab = "s1", ylab = "s2")
hist(proj1_w_rotated[1,], main = "Normal s1 whitened & rotated", xlab = "")
hist(proj1_w_rotated[2,], main = "Normal s2 whitened & rotated", xlab = "")


plot(s2[1,], s2[2,], col = alpha("blue", .3), pch = 16,
     main = "Laplace (original sources)", xlab = "s1", ylab = "s2")
hist(s2[1,], main = "Laplace s1 original", xlab = "")
hist(s2[2,], main = "Laplace s2 original", xlab = "")

plot(proj2_w_rotated[1,], proj2_w_rotated[2,], col = alpha("blue", .3), pch = 16,
     main = "Laplace (whitened & rotated)", xlab = "s1", ylab = "s2")
hist(proj2_w_rotated[1,], main = "Laplace s1 whitened & rotated", xlab = "")
hist(proj2_w_rotated[2,], main = "Laplace s2 whitened & rotated", xlab = "")


plot(s3[1,], s3[2,], col = alpha("blue", .3), pch = 16,
     main = "Uniform (original sources)", xlab = "s1", ylab = "s2")
hist(s3[1,], main = "Uniform s1 original", xlab = "")
hist(s3[2,], main = "Uniform s2 original", xlab = "")

plot(proj3_w_rotated[1,], proj3_w_rotated[2,], col = alpha("blue", .3), pch = 16,
     main = "Uniform (whitened & rotated)", xlab = "s1", ylab = "s2")
hist(proj3_w_rotated[1,], main = "Uniform s1 whitened & rotated", xlab = "")
hist(proj3_w_rotated[2,], main = "Uniform s2 whitened & rotated", xlab = "")



