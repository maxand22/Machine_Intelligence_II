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





# 3.4.1

online_pca <- read.csv("hw3/data/data-onlinePCA.txt", sep = ",", header = TRUE)

#online_pca <- online_pca[sample(1:2000, 2000, FALSE),]

online_pca$group <- factor(rep(1:10, each = 200))

cols <- c("#313695", "#4575b4", "#74add1", "#abd9e9", "#e0f3f8", "#fee090", "#fdae61", "#f46d43", "#d73027", "#a50026")

p <- ggplot(online_pca, aes(x = V1, y = V2, col = group)) + 
    scale_color_manual(values = cols) +
    labs(col   = "Time index (s)",
         x     = "V1",
         y     = "V2",
         title = "") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.key       = element_rect(colour = "black"),
          plot.title       = element_text(face = "bold", hjust = 0.5))

p + geom_point(size = 1.5)

# 3.4.2


pc1 <- pce(subset(online_pca, online_pca$group == 1, select=c("V1","V2")))[,1]
pc2 <- pce(subset(online_pca, online_pca$group == 2, select=c("V1","V2")))[,1]
pc3 <- pce(subset(online_pca, online_pca$group == 3, select=c("V1","V2")))[,1]
pc4 <- pce(subset(online_pca, online_pca$group == 4, select=c("V1","V2")))[,1]
pc5 <- pce(subset(online_pca, online_pca$group == 5, select=c("V1","V2")))[,1]
pc6 <- pce(subset(online_pca, online_pca$group == 6, select=c("V1","V2")))[,1]
pc7 <- pce(subset(online_pca, online_pca$group == 7, select=c("V1","V2")))[,1]
pc8 <- pce(subset(online_pca, online_pca$group == 8, select=c("V1","V2")))[,1]
pc9 <- pce(subset(online_pca, online_pca$group == 9, select=c("V1","V2")))[,1]
pc10 <- pce(subset(online_pca, online_pca$group == 10, select=c("V1","V2")))[,1]

# plot pc1 to pc10 in scatter plot
#p <- ggplot(online_pca, aes(x = V1, y = V2, color=online_pca$group)) + geom_point() + scale_color_brewer(palette="Spectral")

p_a <- (p + geom_point(size = 1.5, alpha = .5) 
        + geom_segment(aes(x=0, y=0), xend=pc1[1], yend=pc1[2], arrow=arrow(), size=1, color=cols[1])
        + geom_segment(aes(x=0, y=0), xend=pc2[1], yend=pc2[2], arrow=arrow(), size=1, color=cols[2])
        + geom_segment(aes(x=0, y=0), xend=pc3[1], yend=pc3[2], arrow=arrow(), size=1, color=cols[3])
        + geom_segment(aes(x=0, y=0), xend=pc4[1], yend=pc4[2], arrow=arrow(), size=1, color=cols[4])
        + geom_segment(aes(x=0, y=0), xend=pc5[1], yend=pc5[2], arrow=arrow(), size=1, color=cols[5])
        + geom_segment(aes(x=0, y=0), xend=pc6[1], yend=pc6[2], arrow=arrow(), size=1, color=cols[6])
        + geom_segment(aes(x=0, y=0), xend=pc7[1], yend=pc7[2], arrow=arrow(), size=1, color=cols[7])
        + geom_segment(aes(x=0, y=0), xend=pc8[1], yend=pc8[2], arrow=arrow(), size=1, color=cols[8])
        + geom_segment(aes(x=0, y=0), xend=pc9[1], yend=pc9[2], arrow=arrow(), size=1, color=cols[9])
        + geom_segment(aes(x=0, y=0), xend=pc10[1], yend=pc10[2], arrow=arrow(), size=1, color=cols[10]))

p_a 



# 3.4.3

# initialize dataframe to save the weight vector at each iteration (time point)
w_df <- data.frame(matrix(nrow = nrow(online_pca), ncol = 2))

# initialize weight vector with length 1
w = c(.70711,.70711)

# set learning rate
e = 0.002


# apply Oja's rule

for (i in 1:nrow(online_pca)){
    x = unlist(online_pca[i, 2:3])
    y = t(x) %*% w
    
    # wi(t + 1) = wi(t) + εy(t) [xi(t) − y(t)wi(t)]
    
    w_df[i, ] <- w
    w = w + e*y * (x - y*w)
}


w_df$time <- online_pca$X
w_df$group <- online_pca$group

ggplot(w_df) + 
    scale_color_manual(values = cols) +
    labs(col   = "Time index (s)",
         x     = "V1",
         y     = "V2",
         title = "") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.key       = element_rect(colour = "black"),
          plot.title       = element_text(face = "bold", hjust = 0.5)) + 
    geom_point(aes(x = X1, y = X2, col = group)) 








online_pca <- data.frame("time" = rep(w_df$time, times = 2),
                         "group" = rep(online_pca$group, times = 2),
                         "V1" = c(online_pca$V1, w_df$X1),
                         "V2" = c(online_pca$V2, w_df$X2))

online_pca$shape = factor(rep(1:2, each = nrow(online_pca)/2))
online_pca$alpha = rep(c(.3, 1), each = nrow(online_pca)/2)

ggplot(online_pca) + 
    scale_color_manual(values = cols) +
    labs(col   = "Time index (s)",
         x     = "V1",
         y     = "V2",
         title = "") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.key       = element_rect(colour = "black"),
          plot.title       = element_text(face = "bold", hjust = 0.5)) + 
    geom_point(aes(x = V1, y = V2, col = group, shape = shape, alpha = alpha)) + 
    scale_alpha_identity()





# Testing -----------------------------------------------------------------


a = rnorm(10000, 0, 1)
b = a + rnorm(10000, 0, 0.5)
test <- data.frame(
    a,
    b
)


w_df <- data.frame(matrix(nrow = nrow(test), ncol = 2))
w = c(.70711,.70711)
e = 0.002

for (i in 1:nrow(test)){
    x = unlist(test[i, ])
    y = t(x) %*% w
    
    # wi(t + 1) = wi(t) + εy(t) [xi(t) − y(t)wi(t)]
    
    w_df[i, ] <- w
    w = w + e*y * (x - y*w)
}

w_df$time <- 1:nrow(test)
w_df$group <- factor(rep(1:10, each = 100))


ggplot(w_df, aes(x = X1, y = X2, col = group)) + 
    scale_color_manual(values = cols) +
    labs(col   = "Time index (s)",
         x     = "V1",
         y     = "V2",
         title = "") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.key       = element_rect(colour = "black"),
          plot.title       = element_text(face = "bold", hjust = 0.5)) + 
    geom_point(size = 1.5)


prcomp(test)

w_df[nrow(w_df), ]
