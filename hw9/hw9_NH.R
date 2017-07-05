### hw9 NH

# 9.3 soft K-means

library(e1071)

cluster_data <- read.table("hw9/cluster.dat")

cluster_data <- as.data.frame(t(cluster_data))


# a) ----------------------------------------------------------------------

# number of initial prototypes
K = 8

data_mean <- data.frame(m1 = rep(mean(cluster_data$V1), K), m2 = rep(mean(cluster_data$V2), K))

# set initial prototypes
initial_prototypes <- data_mean + matrix(runif(16, -1.5,1.5), nrow = K)
#initial_prototypes <- matrix(runif(16, -3, 3), ncol = 2)

# plot initial prototypes
#plot(cluster_data, pch = 16, main = "Initial prototypes")
#points(initial_prototypes, pch = 16, col = "red", cex = 1.5)


# convergence tolerance
gamma = 0.001



# b) ----------------------------------------------------------------------

a_probs = matrix(nrow = nrow(cluster_data), ncol = K)

betas <- seq(from = 1.2, to = 20, by = .2)
beta = 1

w = as.matrix(initial_prototypes)


data_points = as.matrix(cluster_data)

assignment_probs = matrix(nrow = nrow(data_points), ncol = K)

diff = w

while(any(diff > gamma)){
    
    for(alpha in 1:nrow(data_points)){
        
        z = exp(-beta/2*(rowSums((data_points[alpha, ] - w)^2)))
        n = sum(exp(-beta/2*(rowSums((data_points[alpha, ] - w)^2))))
        
        assignment_probs[alpha, ] = z/n
        
    }
    
    w_new = (t(assignment_probs)%*%data_points)/colSums(assignment_probs)
    
    diff = abs(w_new - w)
    
    w = w_new
}

#plot(cluster_data, pch = 16, main = "Initial prototypes", xlim = c(-3,3), ylim = c(-3,3))
#points(initial_prototypes[,1], initial_prototypes[,2], pch = 16, col = "red", cex = 1.5)
#points(w_new[,1], w_new[,2], pch = 16, col = "blue", cex = 1.5)



# c) ----------------------------------------------------------------------

cols = c("#313695", "#238b45", "#f0027f", "#abd9e9", "#fdae61", "#beaed4", "#ffff99", "#a50026")

par(mfrow = c(2, 3))

for(beta in betas){
    
    cl = cmeans(data_points, dist = "euclidean", centers = initial_prototypes, m = beta)
    
    plot(cluster_data, pch = 16, 
         main = paste("Initial prototypes vs. clustering solution (beta = ", beta, ")", sep = ""), 
         xlim = c(-3,3), ylim = c(-3,3),
         col = alpha(cols[cl$cluster], brightness), cex = 1.2)
    points(initial_prototypes[,1], initial_prototypes[,2], pch = 15, col = "black", cex = 1.5)
    points(cl$centers[,1], cl$centers[,2], pch = 17, col = "black", cex = 1.5)
    legend(x = .5, y = -.5, legend = c("original centers", "new centers"), pch = c(15, 17), 
           cex = 1, bty = "n")
}

par(mfrow = c(1,1))






