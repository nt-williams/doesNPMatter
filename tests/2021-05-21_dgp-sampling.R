library(DirichletReg)
library(volesti)

gamma <- 20 # bound on maximum stabilized weight
eta <- 0 # confounding bias (very slow to get a single sample for eta >= .8)
k <- 16 # cardinality of the support of the covariates
tol <- .01 # tolerance around eta

dgp_sample <- Vectorize(function(k, gamma, eta, tol = .01) {
  
  repeat {
    # Sample the marginal covariate distribution p
    p <- as.numeric(rdirichlet(1, rep(1, k)))
    
    # Sample the propensity score pi
    A_pi <- rbind(-diag(k), diag(k), t(matrix(rep(p, k), k, k)) - gamma * diag(k), 
                                gamma * diag(k) - t(matrix(rep(p, k), k, k)))
    b_pi <- c(rep(0, k), rep(1, k), rep(0, k), rep(gamma - 1, k))
    poly_pi <- Hpolytope$new(A = A_pi, b = b_pi)
    pi <- as.numeric(sample_points(poly_pi, 1))
    
    # Check if eta is feasible
    bias_U <- sum(pmax(p * (pi - as.numeric(p %*% pi)) / as.numeric(p %*% pi), 0) - pmin(p * (1 - pi - as.numeric(p %*% (1 - pi))) / as.numeric(p %*% (1 - pi)), 0))
    bias_L <- sum(pmin(p * (pi - as.numeric(p %*% pi)) / as.numeric(p %*% pi), 0) - pmax(p * (1 - pi - as.numeric(p %*% (1 - pi))) / as.numeric(p %*% (1 - pi)), 0))
    
    if (eta >= bias_L & eta <= bias_U) {
      break
    }
  }
  
  # Sample the outcome conditional probability given treatment and covariates q = (q_11, q_12, ..., q_1k, q_01, q_02, ..., q_0k)
  A_q <- rbind(-diag(2 * k), diag(2 * k),
              c(p * (pi - as.numeric(p %*% pi)) / as.numeric(p %*% pi),
                -p * (1 - pi - as.numeric(p %*% (1 - pi))) / as.numeric(p %*% (1 - pi))),
              c(-p * (pi - as.numeric(p %*% pi)) / as.numeric(p %*% pi),
                p * (1 - pi - as.numeric(p %*% (1 - pi))) / as.numeric(p %*% (1 - pi)))
              )
  b_q <- c(rep(0, 2 * k), rep(1, 2 * k), eta + tol, -(eta - tol)) # Here I'm constraining the bias to be within 0.01 of eta. With smaller tolerances, the q probabilities tend to start concentrating toward zero for some reason.
  poly_q <- Hpolytope$new(A = A_q, b = b_q)
  q <- as.numeric(sample_points(poly_q, 1))
  
  # Compute the exact bias
  ATE <- (q[1:k] - q[(k + 1):(2 * k)]) %*% p
  diff_in_means <- q[1:k] %*% (p * pi) / as.numeric(p %*% pi) - q[(k + 1):(2 * k)] %*% (p * (1 - pi)) / as.numeric(p %*% (1 - pi))
  bias <- diff_in_means - ATE
  
  return(list(p = p, pi = pi, q = q, bias = bias))
})
