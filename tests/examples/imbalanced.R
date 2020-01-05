# Generate a highly imbalanced dataset with uniform dist in one cluster
# and a small number of outliers in a gaussian in a separate cluster
inliers = data.frame(x=c(runif(10000, 0, 1), rnorm(10, 2, 0.5)),
                     y=c(runif(10000, 0, 1), rnorm(10, 2, 0.5)))

outliers = data.frame(x=c(runif(1000, 0, 1), rnorm(5, -2, 0.5)))
