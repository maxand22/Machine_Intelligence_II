### MI2 Blatt 2 ###

# Ex. 3)

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

