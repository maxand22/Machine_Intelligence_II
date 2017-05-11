# MI2 Sheet 03 (on-line PCA)


library(ggplot2)
library(dplyr)


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

pca4 <- read.csv("hw3/data/pca4.csv", sep = ",", header = TRUE)

# visual detection of outliers
pairs(pca4)

