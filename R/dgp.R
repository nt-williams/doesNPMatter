sample_dgp <- function(dgp, n) {
  dgpc <- data.table::copy(dgp$x_ret)
  out <- dgpc[sample(1:nrow(dgpc), size = n, replace = TRUE, prob = dgpc$p), ]
  out[, p := NULL][]
}

#' Generate covariance matrix for points in `x` using given kernel function
cov_matrix <- function(x, kernel_fn, ...) {
  apply(x, 1, function(y) apply(x, 1, function(z) kernel_fn(y, z, ...)))
}

#' Given `x` coordinates, take `N` draws from kernel function at those points
draw_samples <- function(x, N, Sigma) {
  Y <- matrix(NA, nrow = nrow(x), ncol = N)
  for (n in 1:N) {
    beta <- rnorm(n = ncol(x))
    Y[, n] <- MASS::mvrnorm(1, mu = x %*% beta, Sigma = Sigma)
  }
  Y
}

se_kernel <- function(x, y, eta = 1, rho = 1) {
  eta * exp(- rho * sum((x - y)^2))
}

#' @param eta larger values mean larger deviations from linear model
#' @param rho smaller values mean smoother functions
dgp <- function(n_bin = 2, n_num = 2, inter_order = 2, hte = TRUE, conf_bias = 0.01,
                pos_bound = 100, npoints = 50, eta = 1, rho = 1, tol = 0.01) {
  
  k <- sum(sapply(0:inter_order, function(i) choose(n_bin, i)))
  if (n_num > 0) {
    # Design matrices
    xnum_vals <- seq(0, 1, length.out = npoints)
    
    xx_num <- as.matrix(expand.grid(replicate(n_num, xnum_vals, simplify = FALSE)))
    colnames(xx_num) <- paste0('x_num', 1:ncol(xx_num))
    
    # Draw Gaussian process for treatment
    Sigma <- cov_matrix(xx_num, se_kernel, eta = eta, rho = rho)
    gauss_process <- plogis(draw_samples(xx_num, k, Sigma = Sigma))
    colnames(gauss_process) <- paste0('x_funs', 1:k)
    
    xbin_vals <- expand.grid(replicate(n_bin, 0:1, simplify = FALSE))
    names(xbin_vals) <- paste0('x_bin', 1:ncol(xbin_vals))
    
    if (inter_order > 1) {
      xx_bin <- model.matrix(formula(paste0('~ .^', inter_order)), data = xbin_vals)
    } else {
      xx_bin <- model.matrix(~ ., data = xbin_vals)
    }

    card_x <- 2^n_bin * npoints^n_num
    
    # Matrix to generate data with interactions and Gaussian processes
    xx <- merge(xx_bin, gauss_process)
    xx[, grep("^x_funs[0-9]+$", colnames(xx))] <- xx[, 1:k] * xx[, grep("^x_funs[0-9]+$", colnames(xx))]
    xx <- as.matrix(xx)
    
    # # Code to plot a gaussian process function, leaving here for now just to have it somewhere
    # plot(range(xx_num), range(gauss_process), xlab = "x", ylab = "y", type = "n",
    #      main = "SE kernel, length = 0.2")
    # for (n in 1:ncol(gauss_process)) {
    #   lines(xx_num, gauss_process[, n], col = n, lwd = 1.5)
    # }
  } else {
    xbin_vals <- expand.grid(replicate(n_bin, 0:1, simplify = FALSE))
    names(xbin_vals) <- paste0('x_bin', 1:ncol(xbin_vals))
    
    if (inter_order > 1) {
      xx_bin <- model.matrix(formula(paste0('~ .^', inter_order)), data = xbin_vals)
    } else {
      xx_bin <- model.matrix(~ ., data = xbin_vals)
    }
    
    card_x <- 2^n_bin
    
    # Matrix to generate data with interactions and Gaussian processes
    xx <- xx_bin
    xx <- as.matrix(xx)
  }
  
  search <- TRUE
  iter <- 1
  
  while (search && iter < 1000) {
    # Sampling P(X=x)
    px <- DirichletReg::rdirichlet(1, rep(1, card_x))[1, ]
    
    # Sampling coefficients in a linear model for P(T=1|X=x)
    constraint_matrix <- rbind(
      # Probability constraints (0,1)
      - xx, xx,
      
      # Constraints on P(T=1)/P(T=1|X=x)
      matrix(rep(px, card_x), card_x, card_x, byrow = TRUE) %*% xx - pos_bound * xx,
      
      # Constraints on P(T=0)/P(T=0|X=x)
      pos_bound * xx - matrix(rep(px, card_x), card_x, card_x, byrow = TRUE) %*% xx
    )
    
    constraint_vector <- c(rep(0, card_x), rep(1, card_x), rep(0, card_x), rep(pos_bound - 1, card_x))
    poly_alpha <- volesti::Hpolytope(A = constraint_matrix, b = constraint_vector)
    alpha <- as.numeric(volesti::sample_points(poly_alpha, 1))
    
    # Saturated linear model probabilities P(T=1 | x)
    pt <- xx %*% alpha
    
    # Check that the P(T = 1| X = x) probs. are feasible, i.e., they can yield a bias conf_bias (check Caleb's write-up)
    bias_U <- sum(pmax(px * (pt - as.numeric(px %*% pt)) /
                         as.numeric(px %*% pt), 0) -
                    pmin(px * (1 - pt - as.numeric(px %*% (1 - pt))) /
                           as.numeric(px %*% (1 - pt)), 0))
    bias_L <- sum(pmin(px * (pt - as.numeric(px %*% pt)) /
                         as.numeric(px %*% pt), 0) -
                    pmax(px * (1 - pt - as.numeric(px %*% (1 - pt))) /
                           as.numeric(px %*% (1 - pt)), 0))
    
    cat("\r", "Searching...", iter, "iterations")
    flush.console()
    iter <- iter + 1
    
    if (conf_bias >= bias_L & conf_bias <= bias_U) {
      search <- FALSE
    }
  }
  
  if (search == TRUE) return("Failed!")
  
  # Matrix to return with only basic covariates
  x_ret <- xx_bin[, grep("^x_bin[0-9]+$", colnames(xx_bin))]
  
  if (hte) {
      if (inter_order > 1) {
          formY <- formula(paste0('~ t * (. - t)^', inter_order))
      } else {
          formY <- ~ t * (. - t)
      }
      l <- 2 * k
      expr <- "(^x_funs[0-9]+$)"
  } else {
      if (inter_order > 1) {
          formY <- formula(paste0('~ t + (. - t)^', inter_order))
      } else {
          formY <- ~ t + (. - t)
      }
    l <- k
    expr <- "(^x_funs[0-9]+$)|(\\bt\\b)"
  }

  if (n_num > 0) {
    x_ret <- merge(xbin_vals, xx_num)
    x_ret <- cbind(x_ret, pt, px)
    x_ret <- cbind(rbind(x_ret, x_ret), t = c(rep(0, nrow(x_ret)), rep(1, nrow(x_ret))))
    x_ret[x_ret[, 't'] == 0, 'pt'] <- 1 - x_ret[x_ret[, 't'] == 0, 'pt']
    
    # Gaussian process for outcome model
    gauss_process <- plogis(draw_samples(xx_num, l, Sigma = Sigma))
    colnames(gauss_process) <- paste0('x_funs', 1:l)
    tmp <- merge(xbin_vals, gauss_process)
    tmp <- cbind(rbind(tmp, tmp), t = c(rep(0, nrow(tmp)), rep(1, nrow(tmp))))
    
    # Design matrix
    xxt_bin <- model.matrix(formY,
                            data = tmp[, c(colnames(tmp)[grep("^x_bin[0-9]+$", colnames(tmp))], 't')])
    
    xxt <- cbind(xxt_bin, tmp[, grep("^x_funs[0-9]+$", colnames(tmp))])

    xxt[, grep("^x_funs[0-9]+$", colnames(xxt))] <-
      xxt[, -grep(expr, colnames(xxt))] * xxt[, grep("^x_funs[0-9]+$", colnames(xxt))]
    
  } else {
    colnames(pt) <- "pt"
    x_ret <- cbind(x_ret, pt, px)
    x_ret <- cbind(rbind(x_ret, x_ret), t = c(rep(0, nrow(x_ret)), rep(1, nrow(x_ret))))
    x_ret[x_ret[, 't'] == 0, 'pt'] <- 1 - x_ret[x_ret[, 't'] == 0, 'pt']
    x_ret <- as.data.frame(x_ret)
    xxt <- model.matrix(
      formula(paste0('~ .^', n_bin + 1)),
      data = x_ret[, c(colnames(x_ret)[grep("^x_bin[0-9]+$", colnames(x_ret))], 't')]
    )
  }
  
  xxt <- as.matrix(xxt)
  
  # Sampling P(Y = 1 | t, x)
  probt1 <- as.numeric(px %*% pt)
  probt0 <- 1 - probt1
  foo_vec  <- with(x_ret, (2 * t - 1) * px * (pt / (t * probt1 + (1 - t) * probt0) - 1))
  
  constraint_matrix <- rbind(-xxt, xxt, foo_vec %*% xxt, -foo_vec %*% xxt)
  
  card_tx <- 2 * card_x
  
  constraint_vector <- c(rep(0, card_tx), rep(1, card_tx), conf_bias + tol, -(conf_bias - tol))
  poly_beta <- volesti::Hpolytope(A = constraint_matrix, b = constraint_vector)
  beta <- as.numeric(volesti::sample_points(poly_beta, 1))
  
  # Saturated linear model probabilities P(Y = 1 | t, x)
  py1 <- xxt %*% beta
  
  i0 <- xxt[, 't'] == 0
  i1 <- xxt[, 't'] == 1
  truth <- (py1[i1] - py1[i0]) %*% px
  unadj <- py1[i1] %*% (px * pt) / as.numeric(px %*% pt) - py1[i0] %*% (px * (1 - pt)) / as.numeric(px %*% (1 - pt))
  x_ret <- cbind(rbind(x_ret, x_ret), y = c(rep(1, nrow(x_ret)), rep(0, nrow(x_ret))), py = c(py1, 1 - py1))
  x_ret$p <-  x_ret$px * x_ret$pt * x_ret$py
  # x_ret$px <- NULL
  # x_ret$pt <- NULL
  x_ret$py <- NULL
  
  list(
    x_ret = data.table::as.data.table(x_ret),
    n_bin = n_bin,
    truth = truth[1],
    unadj = unadj[1],
    bias = unadj[1] - truth[1],
    pos_bound = pos_bound, 
    eta = eta, 
    rho = rho, 
    inter_order = inter_order
  )
}
