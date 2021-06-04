box::use(data.table[...], utils[flush.console])

#' Sample a DGP with constraints on the amount of confounding bias and positivity.
#'
#' @param xs The number of binary covariates. Used to calculate k, the cardinality of the support of the covariates.
#' @param eta Confounding bias.
#' @param gamma Bound on the maximum stabilized weights.
#' @param tol Tolerance around constructing a DGP with bias eta.
#'
#' @export
#' 
#' @author Caleb Miles
dgp <- function(xs = 2, eta, gamma, tol = 0.01) {
  k <- 2^xs
  search <- TRUE
  iter <- 0
  while (search) {
    p <- DirichletReg::rdirichlet(1, rep(1:k))[1, ] # Sampling P(x)
    
    # Sampling P(t|x)
    A_pi <- rbind(
      -diag(k), 
      diag(k), 
      matrix(rep(p, k), k, k, byrow = TRUE) - gamma * diag(k), 
      gamma * diag(k) - matrix(rep(p, k), k, k, byrow = TRUE)
    )
    
    b_pi <- c(rep(0, k), rep(1, k), rep(0, k), rep(gamma - 1, k))
    poly_pi <- volesti::Hpolytope(A = A_pi, b = b_pi)
    pi <- as.numeric(volesti::sample_points(poly_pi, 1))
    
    bias_U <- sum(pmax(p * (pi - as.numeric(p %*% pi)) / as.numeric(p %*% pi), 0) - pmin(p * (1 - pi - as.numeric(p %*% (1 - pi))) / as.numeric(p %*% (1 - pi)), 0))
    bias_L <- sum(pmin(p * (pi - as.numeric(p %*% pi)) / as.numeric(p %*% pi), 0) - pmax(p * (1 - pi - as.numeric(p %*% (1 - pi))) / as.numeric(p %*% (1 - pi)), 0))
    
    cat("\r", "Searching...", iter, "iterations")
    flush.console()
    iter <- iter + 1
    
    if (eta >= bias_L & eta <= bias_U) {
      search <- FALSE
    }
  }
  
  # Sampling P(Y = 1 | t, x)
  A_q <- rbind(
    -diag(2 * k), 
    diag(2 * k),
    c(p * (pi - as.numeric(p %*% pi)) / as.numeric(p %*% pi), -p * (1 - pi - as.numeric(p %*% (1 - pi))) / as.numeric(p %*% (1 - pi))),
    c(-p * (pi - as.numeric(p %*% pi)) / as.numeric(p %*% pi), p * (1 - pi - as.numeric(p %*% (1 - pi))) / as.numeric(p %*% (1 - pi)))
  )
  
  b_q <- c(rep(0, 2 * k), rep(1, 2 * k), eta + tol, -(eta - tol)) # Here I'm constraining the bias to be within 0.01 of eta. With smaller tolerances, the q probabilities tend to start concentrating toward zero for some reason.
  poly_q <- volesti::Hpolytope(A = A_q, b = b_q)
  q <- as.numeric(volesti::sample_points(poly_q, 1))
  
  truth <- (q[1:k] - q[(k + 1):(2 * k)]) %*% p
  unadj <- q[1:k] %*% (p * pi) / as.numeric(p %*% pi) - q[(k + 1):(2 * k)] %*% (p * (1 - pi)) / as.numeric(p %*% (1 - pi))

  cat("\n")
  list(
    xs = xs,
    px = p, 
    pi = pi, 
    q = q, 
    truth = truth[1],
    unadj = unadj[1],
    bias = unadj[1] - truth[1]
  )
}

#' Create a look-up table for the cells of a DGP with the probability for belonging to a given cell
#' 
#' @param dgp a DGP created with \code{dgp}.
#' 
#' @export
dgp_lookup <- function(dgp) {
  px <- dgp$px
  pi <- dgp$pi
  q <- dgp$q
  k <- 2^dgp$xs
  ncats <- 2 * 2 * k
  y <- alt_01(1:ncats, 2)
  trt <- alt_01(1:ncats, 1)
  cnf_meta <- alt_01(1:ncats, 4, 2^dgp$xs) + 1
  cnf <- sapply(1:dgp$xs, function(x) alt_01(cnf_meta, 2^(x - 1)))
  colnames(cnf) <- paste0("cnf", 1:(ncol(cnf)))
  out <- data.table(trt, cnf, y)[order(y, trt)]
  out[, p := c(px * (1 - pi) * (1 - q[(k + 1):(2 * k)]), 
               px * pi * (1 - q[1:k]),
               px * (1 - pi) * q[(k + 1):(2 * k)],
               px * pi * q[1:k])][]
}

#' Sample data from a DGP look-up table.
#' 
#' @param dgp A look-up table created with \code{dgp_lookup}.
#' @param n number of observations to draw.
#' 
#' @export
sample_dgp <- function(dgp, n) {
  dgpc <- copy(dgp)
  out <- dgpc[sample(1:nrow(dgpc), size = n, replace = TRUE, prob = dgpc$p), ]
  out[, p := NULL][]
}

alt_01 <- function(meta, x, cats = 2) {
  ((meta - 1) %/% x) %% cats
}

#' Calculate true ATE from DGP look-up table.
#' 
#' This function isn't used for the truth calculations in simulations. 
#'  Instead it serves as a convenience function for testing.
#'  
#' @param dgp A look-up table created with \code{dgp_lookup}.
#' 
#' @export
truth <- function(dgp) {
  dgpc <- copy(dgp)
  nms_cnf <- grep("^cnf", names(dgp), value = TRUE)
  dgpc[, prob_w := sum(p), by = nms_cnf]
  dgpc[, prob_y := sum(y * p) / sum(p), by = c('trt', nms_cnf)]
  out <- dgpc[y == 1, .(pr = sum(prob_y * prob_w, na.rm = TRUE)), by = 'trt']
  return(out[trt == 1, ]$pr - out[trt == 0, ]$pr)
}
