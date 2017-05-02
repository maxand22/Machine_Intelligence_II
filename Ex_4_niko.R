get_patch <- function(matrix, h, w, return_vector = FALSE){
    
    h <- h-1
    w <- w-1
    
    n <- nrow(matrix)
    p <- ncol(matrix)
    
    h_sample <- sample(1:n, 1, FALSE)
    w_sample <- sample(1:p, 1, FALSE)
    
    if((h_sample + h) > n) {
        h_patch <- (h_sample - h):h_sample
    }else{
        h_patch <- h_sample:(h_sample + h)
    }
    
    if((w_sample + w) > p) {
        w_patch <- (w_sample - w):w_sample
    }else{
        w_patch <- w_sample:(w_sample + w)
    }
    
    if(return_vector == TRUE){
        return(matrix[h_patch, w_patch])
    }else{
        return(as.vector(matrix[h_patch, w_patch]))
    }
}






#install.packages("imager")
library(jpeg)

#install.packages("foreign")
library(foreign)

#install.packages("gridExtra")
library(gridExtra)

#install.packages("ggplot2")
library(ggplot2)

#install.packages("reshape2")
library(reshape2)

#install.packages("colorRamps")
library(colorRamps)

###############################################################################

#2.4.a Buildings

path <- "/Users/Niko/Desktop/Uni/Statistik Master/courses/machine intelligence 2/mi2_homework/mi2_homework/Ex2/imgpca"

f <- file.path(path, c("b1.jpg","b2.jpg", "b3.jpg", "b4.jpg", "b5.jpg", "b6.jpg", "b7.jpg", "b8.jpg", "b9.jpg", "b10.jpg"))

d <- lapply(f, readJPEG)


total_matrix_b <- matrix(nrow = 0, ncol = 256)

for(i in 1:length(d)){
    #set.seed(123)
    tmp1 <- t(replicate(500, get_patch(d[[i]], 16, 16, return_vector = TRUE), simplify = "vector"))
    #tmp2 <- replicate(10, get_patch(d[[i]], 16, 16, return_vector = TRUE))
    total_matrix_b <- rbind(total_matrix, tmp1)
}



#2.4.a Nature

path <- "/Users/Niko/Desktop/Uni/Statistik Master/courses/machine intelligence 2/mi2_homework/mi2_homework/Ex2/imgpca"

f <- file.path(path, c("n1.jpg","n2.jpg", "n3.jpg", "n4.jpg", "n5.jpg", "n6.jpg", "n7.jpg", "n8.jpg", "n9.jpg", "n10.jpg"))

nat <- lapply(f, readJPEG)


total_matrix_n <- matrix(nrow = 0, ncol = 256)

for(i in 1:length(nat)){
    #set.seed(123)
    tmp1 <- t(replicate(500, get_patch(nat[[i]], 16, 16, return_vector = TRUE), simplify = "vector"))
    #tmp2 <- replicate(10, get_patch(nat[[i]], 16, 16, return_vector = TRUE))
    total_matrix_n <- rbind(total_matrix, tmp1)
}




# b)









# c)

pcs_b <- prcomp(total_matrix_b)
eigenvalues_b <- pcs_b$sdev^2
plot(1:256, eigenvalues_b)

cov_b <- cov(total_matrix_b)


pcs_n <- prcomp(total_matrix_n)
eigenvalues_n <- pcs_n$sdev^2
plot(1:256, eigenvalues_n)
