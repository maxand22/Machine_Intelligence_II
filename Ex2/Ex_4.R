install.packages("imager")
library(imager)

install.packages("foreign")
library(foreign)

install.packages("gridExtra")
library("gridExtra")

install.packages("ggplot2")
library(ggplot2)

install.packages("reshape2")
library(reshape2)

install.packages("colorRamps")
library(colorRamps)

###############################################################################

#2.4.a Buildings
path <- "/Users/maxand22/Google Drive/Humboldt/4. Semester/Machine Intelligence II/Machine_Intelligence_II/Ex2/imgpca"

f <- file.path(path, c("b1.jpg","b2.jpg", "b3.jpg", "b4.jpg", "b5.jpg", "b6.jpg", "b7.jpg", "b8.jpg", "b9.jpg", "b9_2.jpg","b9_4.jpg","b9_8.jpg","b9_16.jpg","b10.jpg"))

d <- lapply(f, load.image)

#patches matrix with 7000 rows and 289 variables (pixel)
p_amount <- 500
wx_pixel <- 16
wy_pixel <- 16
patches_matrix_length <- length(f)*p_amount
patches_matrix <- matrix(0, patches_matrix_length, (wx_pixel+1)*(wy_pixel+1))
patches <- vector("list", length = length(f))

i<-1
for(i in 1:length(f)){
  patches[[i]] <- extract_patches(d[[i]], cy = runif(p_amount,1,ncol(d[[i]])), cx = runif(p_amount,1,nrow(d[[i]])), wx = wx_pixel, wy = wy_pixel)
  }

i <- 1
j <- 1
k <- 1

for(i in 1:length(f)){
  h <- patches[[i]]
  
  for(j in 1:p_amount){
    hh <- as.data.frame(h[[j]])
    k<-j+((i-1)*p_amount)
    patches_matrix[k,] <- hh$value
  }
  
  j<-1
}


#2.4.a Nature
pathn <- "/Users/maxand22/Google Drive/Humboldt/4. Semester/Machine Intelligence II/Machine_Intelligence_II/Ex2/imgpca"

fn <- file.path(pathn, c("n1.jpg","n2.jpg", "n3.jpg", "n4.jpg", "n5.jpg", "n6.jpg", "n7.jpg", "n8.jpg", "n9.jpg", "n10.jpg","n11.jpg","n12.jpg","n13.jpg"))

dn <- lapply(fn, load.image)

#patches matrix with 7000 rows and 289 variables (pixel)
patches_matrix_lengthn <- length(fn)*p_amount
patches_matrixn <- matrix(0, patches_matrix_lengthn, (wx_pixel+1)*(wy_pixel+1))
patchesn <- vector("list", length = length(fn))

i<-1
for(i in 1:length(fn)){
  patchesn[[i]] <- extract_patches(dn[[i]], cy = runif(p_amount,1,ncol(dn[[i]])), cx = runif(p_amount,1,nrow(dn[[i]])), wx = wx_pixel, wy = wy_pixel)
}

i <- 1
j <- 1
k <- 1

for(i in 1:length(fn)){
  h <- patchesn[[i]]
  
  for(j in 1:p_amount){
    hh <- as.data.frame(h[[j]])
    k<-j+((i-1)*p_amount)
    patches_matrixn[k,] <- hh$value
  }
  
  j<-1
}



#2.4.b
amount_pca <- 24
pca_matrix <- prcomp(patches_matrix)
pca_matrix_eigenvectors <- pca_matrix$rotation[,1:amount_pca]

pca_patches <- vector("list", length = amount_pca)
i <- 1
for(i in 1:amount_pca){
  pca_patches[[i]] <- matrix(pca_matrix$rotation[,i], wx_pixel+1, wy_pixel+1)
}

pca_patches_cimg <- vector("list", length = amount_pca)
i <- 1
for(i in 1:amount_pca){
  pca_patches_cimg[[i]] <- as.cimg(pca_patches[[i]])
}

h = "white"
l = "black"
plot_patches <- function(patch){
  p_matrix <- matrix(patch, ncol = sqrt(length(patch)))
  p_melted <- melt(p_matrix)

  g1 <- ggplot(data = p_melted, aes(y=Var1, x=Var2, fill=value)) + 
    geom_tile() + scale_fill_gradient(low = l, high = h) 
  
  return(g1)
}

# list object containing all 24 plots
plotlist <- lapply(pca_patches_cimg, plot_patches)

#Just Printing three and Scaling is missing
n <- length(plotlist)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(plotlist[1:3], ncol=nCol))



#2.4.b Nature
amount_pca <- 24
pca_matrixn <- prcomp(patches_matrixn)
pca_matrix_eigenvectorsn <- pca_matrixn$rotation[,1:amount_pca]

pca_patchesn <- vector("list", length = amount_pca)
i <- 1
for(i in 1:amount_pca){
  pca_patchesn[[i]] <- matrix(pca_matrixn$rotation[,i], wx_pixel+1, wy_pixel+1)
}

pca_patches_cimgn <- vector("list", length = amount_pca)
i <- 1
for(i in 1:amount_pca){
  pca_patches_cimgn[[i]] <- as.cimg(pca_patchesn[[i]])
}


# list object containing all 24 plots
plotlistn <- lapply(pca_patches_cimgn, plot_patches)

#Just Printing three and Scaling is missing
nn <- length(plotlistn)
nColn <- floor(sqrt(nn))
do.call("grid.arrange", c(plotlistn[1:3], ncol=nColn))

#compare pc's (rounding 2 decimal)
pca_matrix_nature <- round(pca_matrix_eigenvectorsn, 2)
pca_matrix_build <- round(pca_matrix_eigenvectors, 2)

c <- (pca_matrix_nature == pca_matrix_build)
similar <- table(c)["TRUE"]/table(c)["FALSE"]

#Differences between the PCs for buildings vs. nature exist
#1. Just 5% of values of PCA are similar. So 95% differences in Values of the PCA's.
#2. Moreover, 14 Building Images vs. 13 Nature Images. So the Matrix of Nature Images has 500 observationes less
#3. The PCs correlate more based on picture 9 of the building set.Picture 9 is several times part of the observations


#2.3.c
#calculating eigenvalues
ev <- data.frame(
  value = pca_matrix$sdev^2,
  id = seq.int(1,ncol(pca_matrix$rotation))
)

# Printing the first 24 PC's because of overview
ggplot(data=ev[1:24,], aes(x=ev$id[1:24],y=ev$value[1:24], group=1)) +
  geom_line() +
  geom_point()

compressionratio <- 3/ncol(pca_matrix$rotation)

#Based on the scree plot we would choose the first 3 PC's for Building Picture
#Because of 
#high eigenvalue and before the elobow of the Curve. 
#Furthermore, they are higher as 1 (Kaiser-Criteria)
#Interpretation afterwards is "hard"

#2.3.c Nature
#calculating eigenvalues
evn <- data.frame(
  value = pca_matrixn$sdev^2,
  id = seq.int(1,ncol(pca_matrixn$rotation))
)

# Printing the first 24 PC's because of overview
ggplot(data=evn[1:24,], aes(x=evn$id[1:24],y=evn$value[1:24], group=1)) +
  geom_line() +
  geom_point()

compressionratio <- 3/ncol(pca_matrixn$rotation)

#Based on the scree plot (Nature) we would choose the first 3 PC's
#Because of 
#based on the Kaiserkriterium the first PC is enough, but this would be not sufficent for futher analysis
#Choosing three PC's is based on the high eigenvalue and their position before the elobow of the Curve. 
#However, the Interpretation afterwards is "hard"

#comparessionratio is 0.01 (buildings) vs. 0.01 (Nature)



