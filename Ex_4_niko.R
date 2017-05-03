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
    
    if(return_vector == FALSE){
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
    total_matrix_b <- rbind(total_matrix_b, tmp1)
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
    total_matrix_n <- rbind(total_matrix_n, tmp1)
}




# b)

total_matrix_n_centered <- scale(total_matrix_n, center = TRUE, scale = FALSE)
total_matrix_b_centered <- scale(total_matrix_b, center = TRUE, scale = FALSE)

pcs_b <- eigen(cov(total_matrix_b_centered))$vectors
pcs_n <- eigen(cov(total_matrix_n_centered))$vectors


amount_pca <- 24
pca_patchesn <- pca_patchesb <- vector("list", length = amount_pca)

for(i in 1:24){
    
    pca_patchesn[[i]] <- melt(matrix(pcs_n[, i], 16, 16))
    pca_patchesb[[i]] <- melt(matrix(pcs_b[, i], 16, 16))
    
}


heatmap_custom <- function(matrix){
    g1 <- ggplot(data = matrix, aes(x=Var1, y=Var2, fill=value)) + 
        geom_tile() + 
        scale_fill_continuous(low = "white", high = "black") + 
        guides(fill = FALSE) +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())
                                   
    
    return(g1)
}

plotlist_b <- lapply(pca_patchesb, heatmap_custom)
plotlist_n <- lapply(pca_patchesn, heatmap_custom)

do.call("grid.arrange", c(plotlist_b, ncol=6))
do.call("grid.arrange", c(plotlist_n, ncol=6))


# One can see that the PCs of both image groups are rather similar, in terms of
# the variance they account for (i.e. about 65% by the first PC in both cases).
# However, the second PC of the nature buildings accounts for about 10.7% of the
# variance while the second PC from the nature images only accounts for about
# 5.32%



# c)

#pcs_b <- prcomp(total_matrix_b)
eigenvalues_b <- eigen(cov(total_matrix_b_centered))$values
plot(1:256, eigenvalues_b)

cov_b <- cov(total_matrix_b)


#pcs_n <- prcomp(total_matrix_n)
eigenvalues_n <- eigen(cov(total_matrix_n_centered))$values
plot(1:256, eigenvalues_n)




# d)

no_pcs_ <- c(1, 2, 4, 8, 16, 100)

# buildings

# randomly select three images
set.seed(12345)
buildings_sample <- d[sample(1:10, 3)]

y <- buildings_sample[[1]][1:256, 1] %*% pcs_b[,1:256] %*% t(pcs_b[,1:256])
yy <- buildings_sample[[1]][2:257, 1] %*% pcs_b[,1] %*% t(pcs_b[,1])


ggplot(data = melt(matrix(buildings_sample[[1]][1:256, 1])), aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile() + 
    scale_fill_continuous(low = "white", high = "black")

