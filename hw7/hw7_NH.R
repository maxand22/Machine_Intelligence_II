#install.packages("fastICA")
library(fastICA)
library(jpeg)
library(ggplot2)
library(reshape2)
library(gridExtra)

f_b = file.path("hw7/imgpca/buildings", list.files("hw7/imgpca/buildings"))
f_n = file.path("hw7/imgpca/nature", list.files("hw7/imgpca/nature"))
f_t = file.path("hw7/imgpca/text", list.files("hw7/imgpca/text"))

b_img = lapply(f_b, readJPEG)
n_img = lapply(f_n, readJPEG)
t_img = lapply(f_t, readJPEG)

P = 20000
N = 144
N_root = sqrt(N)


# helper function for patches

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


# a) ----------------------------------------------------------------------

patches_b = matrix(nrow = P, ncol = N)
j = 1

for(i in 1:P){
    
    
    patches_b[i,] = get_patch(b_img[[j]], N_root, N_root, return_vector = TRUE)
    
    if (j == length(b_img)){
        j = 1
    }
    
}


patches_n = matrix(nrow = P, ncol = N)
j = 1

for(i in 1:P){
    
    
    patches_n[i,] = get_patch(n_img[[j]], N_root, N_root, return_vector = TRUE)
    
    if (j == length(n_img)){
        j = 1
    }
    
}

patches_t = matrix(nrow = P, ncol = N)
j = 1

for(i in 1:P){
    
    
    patches_t[i,] = get_patch(t_img[[j]], N_root, N_root, return_vector = TRUE)
    
    if (j == length(t_img)){
        j = 1
    }
    
}



# b) ----------------------------------------------------------------------

# perform fastICA with log cosh function
ic_b = fastICA(patches_b, n.comp = 144, fun = "logcosh")
ic_n = fastICA(patches_n, n.comp = 144, fun = "logcosh")
ic_t = fastICA(patches_t, n.comp = 144, fun = "logcosh")

# whitening:
# use pre-whitening matrix that projects data onto the first n.comp principal components
proj_b = patches_b %*% ic_b$K
proj_n = patches_n %*% ic_n$K
proj_t = patches_t %*% ic_t$K

# check results

# plot heatmaps
ggplot(data = melt(cov(proj_b)), aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile() + 
    scale_fill_gradient(high = "darkred", low = "white") + 
    ggtitle("Cov matrix: buildings after whitening")

ggplot(data = melt(cov(proj_n)), aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile() + 
    scale_fill_gradient(high = "darkred", low = "white") + 
    ggtitle("Cov matrix: nature after whitening")

ggplot(data = melt(cov(proj_t)), aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile() + 
    scale_fill_gradient(high = "darkred", low = "white") + 
    ggtitle("Cov matrix: text after whitening")




# c) ----------------------------------------------------------------------

indep_comp_b = ic_b$A[, 1:20]
indep_comp_n = ic_n$A[, 1:20]
indep_comp_t = ic_t$A[, 1:20]


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


ica_patchest <- ica_patchesn <- ica_patchesb <- vector("list", length = 20)

for(i in 1:20){
    ica_patchesb[[i]] <- melt(matrix(indep_comp_b[, i], 12, 12))
    ica_patchesn[[i]] <- melt(matrix(indep_comp_n[, i], 12, 12))
    ica_patchest[[i]] <- melt(matrix(indep_comp_t[, i], 12, 12))
}

plotlist_b <- lapply(ica_patchesb, heatmap_custom) 
plotlist_n <- lapply(ica_patchesn, heatmap_custom)
plotlist_t <- lapply(ica_patchesn, heatmap_custom)

do.call("grid.arrange", c(plotlist_b, ncol=5))
do.call("grid.arrange", c(plotlist_n, ncol=5))
do.call("grid.arrange", c(plotlist_t, ncol=5))



# d) ----------------------------------------------------------------------

pc_b = prcomp(patches_b)
pc_n = prcomp(patches_n)
pc_t = prcomp(patches_t)

prin_comp_b = pc_b$rotation[, 1:20]
prin_comp_n = pc_n$rotation[, 1:20]
prin_comp_t = pc_t$rotation[, 1:20]


pca_patchest <- pca_patchesn <- pca_patchesb <- vector("list", length = 20)

for(i in 1:20){
    pca_patchesb[[i]] <- melt(matrix(prin_comp_b[, i], 12, 12))
    pca_patchesn[[i]] <- melt(matrix(prin_comp_n[, i], 12, 12))
    pca_patchest[[i]] <- melt(matrix(prin_comp_t[, i], 12, 12))
}

plotlist_b_prin <- lapply(pca_patchesb, heatmap_custom) 
plotlist_n_prin <- lapply(pca_patchesn, heatmap_custom)
plotlist_t_prin <- lapply(pca_patchest, heatmap_custom)

do.call("grid.arrange", c(plotlist_b_prin, ncol=5))
do.call("grid.arrange", c(plotlist_n_prin, ncol=5))
do.call("grid.arrange", c(plotlist_t_prin, ncol=5))
