##### Exercise 3 MI #####

# working directory
setwd("C:/Users/Benni/Box Sync/_studium/2. Semester/04_Machine Intelligence II/Exercises/03")
source("C:/Users/Benni/Documents/R/MI_II/Machine_Intelligence_II/helperfunctions.R")

# packages
library(ggplot2)
library(plotly)
library(RColorBrewer)

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


### 3.3 ### -------------------------------------------------------------


### 3.4 ### -------------------------------------------------------------

onlinePCA <- read.csv("data-onlinePCA.txt")

## (a) ##
# indicate time index by extra column
onlinePCA[1:200,4] <- 1
onlinePCA[201:400,4] <- 2
onlinePCA[401:600,4] <- 3
onlinePCA[601:800,4] <- 4
onlinePCA[801:1000,4] <- 5
onlinePCA[1001:1200,4] <- 6
onlinePCA[1201:1400,4] <- 7
onlinePCA[1401:1600,4] <- 8
onlinePCA[1601:1800,4] <- 9
onlinePCA[1801:2000,4] <- 10
# convert to factor
onlinePCA[,4] <- as.factor(onlinePCA$V4)
ggplot(onlinePCA, aes(x = V1, y = V2, color=V4)) + geom_point() + scale_color_brewer(palette="Spectral")

## (b) ##
# subsetting data the following way:
# opca1 <- subset(onlinePCA, onlinePCA$V4 == 1, select=c("V1","V2"))
pc1 <- pce(subset(onlinePCA, onlinePCA$V4 == 1, select=c("V1","V2")))[,1]
pc2 <- pce(subset(onlinePCA, onlinePCA$V4 == 2, select=c("V1","V2")))[,1]
pc3 <- pce(subset(onlinePCA, onlinePCA$V4 == 3, select=c("V1","V2")))[,1]
pc4 <- pce(subset(onlinePCA, onlinePCA$V4 == 4, select=c("V1","V2")))[,1]
pc5 <- pce(subset(onlinePCA, onlinePCA$V4 == 5, select=c("V1","V2")))[,1]
pc6 <- pce(subset(onlinePCA, onlinePCA$V4 == 6, select=c("V1","V2")))[,1]
pc7 <- pce(subset(onlinePCA, onlinePCA$V4 == 7, select=c("V1","V2")))[,1]
pc8 <- pce(subset(onlinePCA, onlinePCA$V4 == 8, select=c("V1","V2")))[,1]
pc9 <- pce(subset(onlinePCA, onlinePCA$V4 == 9, select=c("V1","V2")))[,1]
pc10 <- pce(subset(onlinePCA, onlinePCA$V4 == 10, select=c("V1","V2")))[,1]

# plot pc1 to pc10 in scatter plot
p <- ggplot(onlinePCA, aes(x = V1, y = V2, color=onlinePCA$V4)) + geom_point() + scale_color_brewer(palette="Spectral")

p_a <- (p 
+ geom_segment(aes(x=0, y=0), xend=pc1[1], yend=pc1[2], arrow=arrow(), size=1.5, color="black")
+ geom_segment(aes(x=0, y=0), xend=pc2[1], yend=pc2[2], arrow=arrow(), size=1.5, color="black")
+ geom_segment(aes(x=0, y=0), xend=pc3[1], yend=pc3[2], arrow=arrow(), size=1.5, color="black")
+ geom_segment(aes(x=0, y=0), xend=pc4[1], yend=pc4[2], arrow=arrow(), size=1.5, color="black")
+ geom_segment(aes(x=0, y=0), xend=pc5[1], yend=pc5[2], arrow=arrow(), size=1.5, color="black")
+ geom_segment(aes(x=0, y=0), xend=pc6[1], yend=pc6[2], arrow=arrow(), size=1.5, color="black")
+ geom_segment(aes(x=0, y=0), xend=pc7[1], yend=pc7[2], arrow=arrow(), size=1.5, color="black")
+ geom_segment(aes(x=0, y=0), xend=pc8[1], yend=pc8[2], arrow=arrow(), size=1.5, color="black")
+ geom_segment(aes(x=0, y=0), xend=pc9[1], yend=pc9[2], arrow=arrow(), size=1.5, color="black")
+ geom_segment(aes(x=0, y=0), xend=pc10[1], yend=pc10[2], arrow=arrow(), size=1.5, color="black"))
p_a

## (c) ##

