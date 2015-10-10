# adapted from R Documentation for prcomp function

# perform PCA on the data USArrests

result <- prcomp(USArrests)

# by default, prcomp will automatically calculate the new principal component variables

# view the first principal component values for each data point

PC1 <- result$x[,1]

PC1
