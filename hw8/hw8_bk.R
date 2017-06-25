#### Exercise 8 ####
# packages
library(Matrix)
library(ggplot2)
library(hier.part)

### 8.1 Simulated Annealing ###
## functions

E <- function(W, s){
  summe <- 0
  for(i in 1:6){
    for(j in 1:6){
      summe <- summe + W[i,j]*s[i]*s[j]
    }
  }
  return(-0.5*summe)
}

E_of_si <- function(W, s, i, nachbarn){
  summe <- 0
    for(j in nachbarn){
      summe <- summe + W[i,j]*s[i]*s[j]
    }
  return(-0.5*summe)
}

P_func <- function(b, dE){
  return(1/(1+exp(b*dE)))
}

## Initialization
beta0 <- 1/10
tau <- 1.1
t_max <- 100

# state update loop iterations iterations
M1 <- 1
M2 <- 500

# set s randomly
set.seed(123)
s0 <- sample(c(-1,1),6, replace = TRUE)

# random weight matrix (symmetric, diagonal: 0)
set.seed(509)
W <- matrix(rnorm(36), 6)
W <- forceSymmetric(W)
# set diagonal elements to zero
for(i in 1:6){
  for(j in 1:6){
    if(i==j){
      W[i,j] <- 0
    }
  }
}

## Optimization
neighborhelper <- 1:6
EnergyM1 <- matrix(nrow = 1, ncol = t_max)
EnergyM2 <- matrix(nrow = 1, ncol = t_max)

# State Update Loop with M = 1 Iterations
for(i in 1:t_max){
  if(i == 1){
    s <- s0
    beta <- beta0}
  for(j in 1:M1){
    node <- sample(c(1:6),1)
    neighbors <- neighborhelper[-node]
    
    E_si <- E_of_si(W, s, node, neighbors)
    
    E_minus_si <- -E_si
    
    delta_E <- E_minus_si - E_si
    
    if(P_func(beta, delta_E)>=runif(1,0,1)){
      s[node] <- (-1)*s[node]} 
  } # end state update loop
  beta <- beta*tau
  EnergyM1[i] <- E(W, s)
} #end annealing loop
sM1 <- s
plot(1:t_max, EnergyM1, type = 'l')


# State update loop with M2 = 500 iterations
for(i in 1:t_max){
  if(i == 1){
    s <- s0
    beta <- beta0}
  for(j in 1:M2){
    node <- sample(c(1:6),1)
    neighbors <- neighborhelper[-node]
    
    E_si <- E_of_si(W, s, node, neighbors)
    
    E_minus_si <- -E_si
    
    delta_E <- E_minus_si - E_si
    
    if(P_func(beta, delta_E)>=runif(1,0,1)){
      s[node] <- (-1)*s[node]} 
  } # end state update loop
  beta <- beta*tau
  EnergyM2[i] <- E(W, s)
} #end annealing loop
sM2 <- s
plot(1:t_max, EnergyM2, type = 'l')

# compare results
E(W, s0)
E(W, sM1)
E(W, sM2)


# Plots
# values for beta for every iteration
betas <- matrix(nrow = t_max, ncol = 1)
betas[1] <- beta0
for(i in 1:(t_max-1)){
  betas[i+1] <- betas[i]*tau
}

# M1 plot
ggplot() + geom_line(data = as.data.frame(t(EnergyM1)), aes(x = 1:t_max, y = V1, color = "red")) + geom_line(data = as.data.frame(betas), aes(x = 1:t_max, y = 1/betas, color = "blue"))

# M2 plot
ggplot() + geom_line(data = as.data.frame(t(EnergyM2)), aes(x = 1:t_max, y = V1, color = "red")) + geom_line(data = as.data.frame(betas), aes(x = 1:t_max, y = 1/betas, color = "blue"))

# Matrix with all states of s1,...s6
States <- rbind(rep(0,6), combos(6)$binary) # combos-function from package hier.part
States[States == 0] <- -1
# fill in Energy of states
E_all <- matrix(nrow = 64, ncol = 1)

for(i in 1:length(E_all)){
  E_all[i] <- E(W, States[i,])
}

# fill in Probability of chosing a state with fixed beta
P_all <- matrix(nrow = 64, ncol = 1)
Z <- 0
for(i in 1:64){
  Z <- Z + exp(-beta0*E(W, States[i,]))
}
for(i in 1:length(P_all)){
  P_all[i] <- (exp(-beta0*E(W, States[i,])))/Z
}

# bar plot of Energy of all states and its Probability
barplot(E_all, beside = TRUE, names.arg = 1:64)
barplot(P_all, beside = TRUE, names.arg = 1:64)

