# libraries
library(ggplot2)
library(reshape2)

#### 2.3 ####
## (a) ##

# load data
expDat <- read.csv("Ex2/expDat.txt", sep = ",", header = TRUE)

# calculate mean matrix
n <- nrow(expDat[2:ncol(expDat)])
M_mean <- matrix(data=1, nrow=n) %*% colMeans(expDat[2:ncol(expDat)])
# subtract mean from data set 
expDat_cen <- expDat[2:ncol(expDat)] - M_mean

# convert df to matrix
D <- as.matrix(expDat_cen)
# compute Covariance Matrix C
C <- (n-1)^-1 * t(D) %*% D
# Eigenvalues of C, Eigenvectors in P
ev <- eigen(C)
P  <- ev$vectors


## (b) ##

# transform D
D_tr <- P %*% t(D)
# in ggplot2 plottable data frame
X_pca <- as.data.frame(t(D_tr)) 

#scatter plot
ggplot(X_pca, aes(x=X_pca$V1, y=X_pca$V2)) + scale_colour_gradient(low = "red", high = "blue") + geom_point(aes(colour=1:100))


#line plot
df <- data.frame(expDat[,1], X_pca$V1, X_pca$V2)
colnames(df)[1] <- "time_index"

ggplot(df, aes(x=time_index, y=value)) + scale_colour_gradient(low = "red", high = "blue") + geom_line(aes(y=X_pca$V1, colour=1:100)) + geom_line(aes(y=X_pca$V2,colour=1:100))


## (c)##

newDat <- apply(expDat[2:21], 2, sample)

## (d) ##
# shuffled dataset
# calculate mean matrix of new data set
n_new <- nrow(newDat)
M_mean_new <- matrix(data=1, nrow=n_new) %*% colMeans(newDat)
# subtract new mean from shuffled data set
Dat_cen_new <- newDat - M_mean_new
# compute Covariance Matrix C_new 
C_new <- (n_new-1)^-1 * t(Dat_cen_new) %*% Dat_cen_new



# convert cov matrizes to data frame for easier plotting
C_melted <- melt(C)
C_melted_new <- melt(C_new)

# plot heatmaps
ggplot(data = C_melted, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

ggplot(data = C_melted_new, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

#spree plot of original data set

pcs <- prcomp(expDat[2:ncol(expDat)])
#compute standard deviation of each principal component
std_dev <- pcs$sdev
#compute variance
pr_var <- std_dev^2
#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)

#scree plot 
plot(prop_varex, type="b")  

#spree plot of shuffled data set

pcs_new <- prcomp(newDat)
#compute standard deviation of each principal component
std_dev_new <- pcs_new$sdev
#compute variance
pr_var_new <- std_dev_new^2

#proportion of variance explained
prop_varex_new <- pr_var_new/sum(pr_var_new)

plot(prop_varex_new, type = "b")



eigenvalues_original <- eigen(scale(C, center = TRUE))$values
eigenvalues_scrambled <- eigen(scale(C_new, center = TRUE))$values

par(mfrow = c(1, 2))
plot(1: length(eigenvalues_original), eigenvalues_original, 
     main = "Original data", xlab = "eigenvalue number", ylab = "eigenvalue")
plot(1: length(eigenvalues_scrambled), eigenvalues_scrambled, 
     main = "Scrambled data", xlab = "eigenvalue number", ylab = "eigenvalue")

dev.off()


# e) 

# Shuffeling the data rowwise in the same sequence for all columns does not affect
# the resulting covariance matrix. This is easy to see without programming, since
# both, variances and covariances are based on element-wise sums of squared errors. 
# For summing over a number of elements, the order is not relevant. Therefore, the 
# variances remain unchanged even if the data was not shuffeled in the same sequence
# since they do not depend on the values of other variables.
# However, for the covariances this is important because otherwise the relationship
# between the features would change. But here, since the rows 
# are shuffeled in the same way for all columns (i.e. features), the relationships 
# (covariances) between the features remain the same as well.


