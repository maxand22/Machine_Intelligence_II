##### Exercise 3 MI #####

# working directory
setwd("C:/Users/Benni/Box Sync/_studium/2. Semester/04_Machine Intelligence II/Exercises/03")
source("C:/Users/Benni/Documents/R/MI_II/Machine_Intelligence_II/helperfunctions.R")

# packages
library(ggplot2)
library(plotly)

### 3.1 ### -------------------------------------------------------------

## (a) ##
pca2 <- read.csv("pca2.csv", header = TRUE)
plot(pc_trans(pca2, 2))
# --> strong influence of outlieres deforms pca creation

## (b) ##
pca2_clean <- pca2[-c(17,157),]
plot(as.data.frame(pc_trans(pca2_clean,2)))
tr <- pc_trans(pca2_clean, 2)
# --> much less influence of outlieres


### 3.2 ### -------------------------------------------------------------

## (a) ##
pca4 <- read.csv("pca4.csv")
pairs(pca4)
# --> cleary observable outliers in the x1,x3; x1,x4; x2,x3; x2,x4 and x3,x4 space

## (b) ##
# why reasonable subset? --> do it for all?
# scree plot
plot(pce(pca4, eValues = TRUE)) 
# looks like first two are enough
# do pca with two pcs
P4 <- pc_trans(pca4, 2)
plot(P4)

## (c) ##
# Extract Eigenvectors using pce
E <- pce(pca4)
# Center Data using ct
X <- ct(pca4)
# Eigenvalues (using pce) in diagonal Matrix
# Apply the "^-0.5" now, otherwise you get INF for the zeros
L <- diag((pce(pca4, eValues = TRUE))^-0.5)
# Whiten Data
Z <- X %*% E %*% L
# check correlations
cor(Z) # nearly zero

## (d) ##
# (i): Covariance matrix of standard data
plot_ly(z = cm(pca4), type = "heatmap")
#plot_ly(z = cov(pca4), type = "heatmap") --> same, just for check with inbuilt function
# (ii): Covariance matrix of transformed data
plot_ly(z = cm(pc_trans(pca4, 4)), type = "heatmap")
# (iii): Covariance matrix of the whitend data
plot_ly(z = cm(Z), type = "heatmap")
