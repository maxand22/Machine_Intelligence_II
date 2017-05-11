# MI2 Sheet 03 (on-line PCA)


library(ggplot2)
library(dplyr)
library(reshape2)

# 3.1)

# a)

pca2 <- read.csv("hw3/data/pca2.csv", sep = ",", header = TRUE)

# center the data
pca2_centered <- scale(pca2, center = TRUE, scale = FALSE)

# compute covariance matrix of centered data
C = 1/(nrow(pca2_centered) - 1) * t(pca2_centered) %*% pca2_centered

# PCs = eigenvectors of COV matrix 
pca2_pcs <- eigen(C)$vectors


# split plotting screen
par(mfrow = c(1,2))

plot(as.matrix(pca2_centered) %*% pca2_pcs, xlim = c(-7, 7), ylim = c(-13, 13), xlab = "PC1", ylab = "PC2")


# b)

# now: remove outliers
pca2_cleaned <- pca2[-c(17, 157), ]

# center the data
pca2_cleaned_centered <- scale(pca2_cleaned, center = TRUE, scale = FALSE)

# compute covariance matrix of centered data
C_cleaned = 1/(nrow(pca2_cleaned_centered) - 1) * t(pca2_cleaned_centered) %*% pca2_cleaned_centered

# PCs = eigenvectors of COV matrix 
pca2_cleaned_pcs <- eigen(C_cleaned)$vectors

plot(as.matrix(pca2_cleaned_centered) %*% pca2_cleaned_pcs, xlim = c(-7, 7), ylim = c(-13, 13), xlab = "PC1", ylab = "PC2")

par(mfrow = c(1,1))



# 3.2)

# a)
pca4 <- read.csv("hw3/data/pca4.csv", sep = ",", header = TRUE)

# visual detection of outliers
pairs(pca4)

par(mfrow = c(2,2))
boxplot(pca4$X1, main = "V1")
boxplot(pca4$X2, main = "V2")
boxplot(pca4$X3, main = "V3")
boxplot(pca4$X4, main = "V4")

par(mfrow = c(1,1))

# Especially feature 3 and 4 have strong outliers


# b)

# center the data 
pca4_centered <- scale(pca4, center = TRUE, scale = FALSE)

# compute cov matrix

cov_pca4 <- cov(pca4_centered)

pca4_pcs <- eigen(cov_pca4)$vectors

pca4_eigenvals <- eigen(cov_pca4)$values


# scree plot

plot(pca4_eigenvals)

# clearly, the fourth PC can be ommitted since it does not provide any 
# additional information to the first three PCs


# c)

# "Whiten" the data, i.e. use Mahalanobis transformation for zero mean and unit sd

Z = pca4_centered %*% pca4_pcs %*% diag((pca4_eigenvals)^-0.5)

# check the transformation:

# means
round(colMeans(Z), digits = 8)

# standard deviations
apply(Z, 2, var)


# d)

# 1st heatplot: covariance matrix

heatmap_data1 <- melt(cov(pca4_centered))

# plot heatmap 1
ggplot(data = heatmap_data1, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile() + 
    scale_fill_continuous(low = "white", high = "#b30000") + 
    ggtitle("Covariance matrix of 'pca4' data")


# 2nd heatplot: covariance matrix of data projected onto PC1,...,PC4

projections <- pca4_centered %*% pca4_pcs

heatmap_data2 <- melt(cov(projections))

# plot heatmap 2
ggplot(data = heatmap_data2, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile() + 
    scale_fill_continuous(low = "white", high = "#b30000") + 
    ggtitle("Covariance matrix of projections")


# 3rd heatplot: covariance matrix of whitened variables

heatmap_data3 <- melt(cov(Z))

# plot heatmap 3
ggplot(data = heatmap_data3, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile() + 
    scale_fill_continuous(low = "white", high = "#b30000") + 
    ggtitle("Covariance matrix of whitened data")
