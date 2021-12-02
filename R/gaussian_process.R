library(MASS)
library(kernlab)

## generate covariance matrix for points in `x` using given kernel function
cov_matrix <- function(x, kernel_fn, ...) {
    apply(x, 1, function(y)apply(x, 1, function(z)kernel_fn(y, z, ...)))
}

## given x coordinates, take N draws from kernel function at those points
draw_samples <- function(x, N, Sigma) {
    Y <- matrix(NA, nrow = nrow(x), ncol = N)
    for (n in 1:N) {
        beta <- rnorm(n = ncol(x))
        Y[, n] <- mvrnorm(1, mu = x %*% beta, Sigma = Sigma)
    }
    Y
}

se_kernel <- function(x, y, eta = 1, rho = 1) {
    eta * exp(- rho * sum((x - y)^2))
}


