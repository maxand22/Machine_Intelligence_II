## helperfunctions ##

# contents:
# ct(df): returns centered Matrix
# cm(df): returns Covariance Matrix from Data frame (using ct)
# pce(df): extracts principal components from as a matrix (using cm, ct) [default].
#           can also extract eigenvalues if statement "eValues = TRUE" is set
#           CAUTION: Not ordered in terms of eigenvalues (so far)
# pc_trans(df, x): transforms the data to the x PCs

ct <- function(df){
  n <- nrow(df)
  m <- ncol(df)
  # create mean matrix 
  mean_v <- matrix(nrow = 1, ncol = m)
  for(i in 1:m){
    mean_v[i] <- mean(df[,i])
  }
  df_M_mean <- matrix(data=1, nrow=n) %*% mean_v
  # center matrix
  centered <- as.matrix(df - df_M_mean)
  return(centered)
}

cm <- function(df){
  # center df and convert to matrix using ct function
  M_cen <- ct(df)
  n <- nrow(df)
  C <- (n-1)^-1 * t(M_cen) %*% M_cen
  return(C)
}

pce <- function(df, eValues = FALSE){
  # missing: ordering of eigenvectors in descending order of eigenvalues
  if(!eValues){
  C <- cm(df)
  eigens <- eigen(C)
  pc <- eigens$vectors
  return(pc)}
  else{
  C <- cm(df)
  eigens <- eigen(C)
  pc <- eigens$values
  return(pc)}
}

pc_trans <- function(df, x){
  pcs <- pce(df)
  ev_values <- pce(df, eValues = TRUE)
  # sort pcs decreasing by their eigenvalues
  pcs_sorted <- pcs[, sort.int(ev_values, index.return = TRUE, decreasing = TRUE)$ix]
  # extract x PCs
  #x_pcs <- matrix(data=0, nrow=ncol(df), ncol = ncol(df))
  #x_pcs[,1:x] <- pcs_sorted[,1:x]
  # transform data to x pcs
  P <- t(pcs_sorted %*% t(ct(df)))
  # extract x PCs
  P <- P[,1:x]
  return(as.data.frame(P))
}