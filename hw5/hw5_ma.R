install.packages("audio")
library(audio)
setWavPlayer("playwave")

install.packages("ggplot2")
library(ggplot2)


path1 <- "/Users/maxand22/Google Drive/Humboldt/4. Semester/Machine Intelligence II/Machine_Intelligence_II/hw5/sound1.dat"
path2 <- "/Users/maxand22/Google Drive/Humboldt/4. Semester/Machine Intelligence II/Machine_Intelligence_II/hw5/sound2.dat"

#1.a
sound1 <- read.csv(path1, header = FALSE)
#qplot(1:nrow(sound1), sound1)
source1 <- audioSample(t(as.matrix(sound1)), rate=8192)
play(source1)

sound2 <- read.csv(path2, header = FALSE)
#qplot(1:nrow(sound2), sound2)
source2 <- audioSample(t(as.matrix(sound2)), rate=8192)
play(source2)

#1.b
s <- matrix( data= c(t(as.matrix(sound1)), t(as.matrix(sound2))),nrow =  ncol(a))
a <- matrix(runif(4, max = 1, min = 0), 2, 2)
x <- a%*%s

#1.c
x_permute <- x[,sample(ncol(x))]

#1.c
correlation <- matrix(1, nrow = nrow(x),ncol=nrow(s))

for (i in 1:nrow(x)) {
  for (j in 1:nrow(s)) {
    correlation[i, j] <- cor(s[j,], x_permute[i,]) 
  }
}

#1.d
x_permute_center <- scale(t(x_permute), center = TRUE, scale = FALSE)
#round(colMeans(x_permute_center),2)

#1.f
w <- matrix(runif(4, max = 1, min = 0), 2, 2)


#2.a

