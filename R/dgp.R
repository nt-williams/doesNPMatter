box::use(data.table[...], utils[flush.console], stats[weighted.mean])
#' Sample a DGP with constraints on the amount of
#' confounding bias and positivity.
#'
#' @param xs The number of binary covariates. Used to calculate k,
#'   the cardinality of the support of the covariates.
#' @param eta Confounding bias.
#' @param pos_bound Bound on the maximum stabilized weights.
#' @param tol Tolerance around constructing a DGP with bias conf_bias.
#'
#' @export
#'
#' @author Caleb Miles

## Code to plot a gaussian process function, leaving here for now just to have it somewhere
## plot(range(xx_num), range(gauss_process), xlab = "x", ylab = "y", type = "n",
##      main = "SE kernel, length = 0.2")
## for (n in 1:ncol(gauss_process)) {
##     lines(xx_num, gauss_process[, n], col = n, lwd = 1.5)
## }

dgp <- function(n_bin = 2, n_num = 2, conf_bias, pos_bound, npoints = 50, eta, rho, tol = 0.01) {

    k <- 2^n_bin

    if(n_num > 0) {
        ## Design matrices
        xnum_vals <- seq(0, 1, length.out = npoints)

        xx_num <- as.matrix(expand.grid(replicate(n_num, xnum_vals, simplify = FALSE)))
        colnames(xx_num) <- paste0('x_num', 1:ncol(xx_num))

        ## Draw Gaussian process for treatment
        Sigma <- cov_matrix(xx_num, se_kernel, rho = rho, eta = eta)
        gauss_process <- plogis(draw_samples(xx_num, k, Sigma = Sigma))
        colnames(gauss_process) <- paste0('x_funs', 1:k)

        xbin_vals <- expand.grid(replicate(n_bin, 0:1, simplify = FALSE))
        names(xbin_vals) <- paste0('x_bin', 1:ncol(xbin_vals))

        xx_bin <- model.matrix(formula(paste0('~ .^', n_bin)), data = xbin_vals)
        card_x <- k * npoints^n_num

        ## Matrix to generate data with interactions and Gaussian processes
        xx <- merge(xx_bin, gauss_process)
        xx[, grep("^x_funs[1-9]$", colnames(xx))] <- xx[, 1:k] * xx[, grep("^x_funs[1-9]$", colnames(xx))]
        xx <- as.matrix(xx)

    } else {

        xbin_vals <- expand.grid(replicate(n_bin, 0:1, simplify = FALSE))
        names(xbin_vals) <- paste0('x_bin', 1:ncol(xbin_vals))

        xx_bin <- model.matrix(formula(paste0('~ .^', n_bin)), data = xbin_vals)

        card_x <- k

        ## Matrix to generate data with interactions and Gaussian processes
        xx <- xx_bin
        xx <- as.matrix(xx)

    }

    search <- TRUE
    iter <- 1

    while (search && iter < 1000) {

        px <- DirichletReg::rdirichlet(1, rep(1:card_x))[1, ] # Sampling P(X=x)

        ## Sampling coefficients in a linear saturated model for P(T=1|X=x)
        constraint_matrix <- rbind(
            ## Probability constraints (0,1)
            - xx, xx,
            ## Constraints on P(T=1)/P(T=1|X=x)
            matrix(rep(px, card_x), card_x, card_x, byrow = TRUE) %*% xx - pos_bound * xx,
            ## Constraints on P(T=0)/P(T=0|X=x)
            pos_bound * xx - matrix(rep(px, card_x), card_x, card_x, byrow = TRUE) %*% xx
        )

        constraint_vector <- c(rep(0, card_x), rep(1, card_x), rep(0, card_x), rep(pos_bound - 1, card_x))
        poly_alpha <- volesti::Hpolytope(A = constraint_matrix, b = constraint_vector)
        alpha <- as.numeric(volesti::sample_points(poly_alpha, 1))

        ## Saturated linear model probabilities P(T=1 | x)
        pt <- xx %*% alpha

        ## Check that the P(T=1|X=x) probs. are feasible, i.e., they can yield a bias conf_bias (check Caleb's write-up)
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

    ## Matrix to return with only basic covariates
    x_ret <- xx_bin[, grep("^x_bin[1-9]$", colnames(xx_bin))]

    if(n_num > 0) {

        x_ret <- merge(xbin_vals, xx_num)
        x_ret <- cbind(x_ret, pt, px)
        x_ret <- cbind(rbind(x_ret, x_ret), t = c(rep(0, nrow(x_ret)), rep(1, nrow(x_ret))))
        x_ret[x_ret[, 't'] == 0, 'pt'] <- 1 - x_ret[x_ret[, 't'] == 0, 'pt']

        ## Gaussian process for outcome model
        gauss_process <- plogis(draw_samples(xx_num, 2 * k, Sigma = Sigma))
        colnames(gauss_process) <- paste0('x_funs', 1:(2 * k))
        tmp <- merge(xbin_vals, gauss_process)
        tmp <- cbind(rbind(tmp, tmp), t = c(rep(0, nrow(tmp)), rep(1, nrow(tmp))))
        ## Design matrix
        xxt_bin <- model.matrix(formula(paste0('~ .^', n_bin + 1)),
                            data = tmp[, c(colnames(tmp)[grep("^x_bin[1-9]$", colnames(tmp))], 't')])

        xxt <- cbind(xxt_bin, tmp[, grep("^x_funs[1-9]$", colnames(tmp))])
        xxt[, grep("^x_funs[1-9]$", colnames(xxt))] <- xxt[, 1:(2 * k)] * xxt[, grep("^x_funs[1-9]$", colnames(xxt))]

    } else {

        x_ret <- cbind(x_ret, pt, px)
        x_ret <- cbind(rbind(x_ret, x_ret), t = c(rep(0, nrow(x_ret)), rep(1, nrow(x_ret))))
        x_ret[x_ret[, 't'] == 0, 'pt'] <- 1 - x_ret[x_ret[, 't'] == 0, 'pt']
        xxt <- model.matrix(formula(paste0('~ .^', n_bin + 1)),
                                data = x_ret[, c(colnames(x_ret)[grep("^x_bin[1-9]$", colnames(x_ret))], 't')])

    }

    xxt <- as.matrix(xxt)

    ## Sampling P(Y = 1 | t, x)
    probt1 <- as.numeric(px %*% pt)
    probt0 <- 1 - probt1
    foo_vec  <- with(x_ret, (2 * t - 1) * px * (pt / (t * probt1 + (1 - t) * probt0) - 1))

    constraint_matrix <- rbind(-xxt, xxt, foo_vec %*% xxt, -foo_vec %*% xxt)

    card_tx <- 2 * card_x

    constraint_vector <- c(rep(0, card_tx), rep(1, card_tx), conf_bias + tol, -(conf_bias - tol))
    poly_beta <- volesti::Hpolytope(A = constraint_matrix, b = constraint_vector)
    beta <- as.numeric(volesti::sample_points(poly_beta, 1))

    ## Saturated linear model probabilities P(Y=1 | t, x)
    py1 <- xxt %*% beta

    i0 <- xxt[, 't'] == 0
    i1 <- xxt[, 't'] == 1
    truth <- (py1[i1] - py1[i0]) %*% px
    unadj <- py1[i1] %*% (px * pt) / as.numeric(px %*% pt) -
        py1[i0] %*% (px * (1 - pt)) / as.numeric(px %*% (1 - pt))

    cat("\n")

    x_ret <- cbind(rbind(x_ret, x_ret), y = c(rep(1, nrow(x_ret)), rep(0, nrow(x_ret))),
                   py = c(py1, 1 - py1))
    x_ret$p <-  x_ret$px * x_ret$pt * x_ret$py
    x_ret$px <- NULL
    x_ret$pt <- NULL
    x_ret$py <- NULL

    return(list(x_ret = as.data.table(x_ret),
                n_bin = n_bin,
                truth = truth[1],
                unadj = unadj[1],
                bias = unadj[1] - truth[1],
                pos_bound = pos_bound))
}



#' Create a look-up table for the cells of a DGP with the probability
#' for belonging to a given cell
#'
#' @param dgp a DGP created with \code{dgp}.
#'
#' @export
dgp_lookup <- function(dgp) {
  px <- dgp$px
  pt <- dgp$pt
  q <- dgp$q
  k <- 2^dgp$n_bin
  ncats <- 2 * 2 * k
  y <- alt_01(1:ncats, 2)
  trt <- alt_01(1:ncats, 1)
  cnf_meta <- alt_01(1:ncats, 4, 2^dgp$n_bin) + 1
  cnf <- sapply(1:dgp$n_bin, function(x) alt_01(cnf_meta, 2^(x - 1)))
  colnames(cnf) <- paste0("cnf", 1:(ncol(cnf)))
  out <- data.table(trt, cnf, y)[order(y, trt)]
  out[, p := c(px * (1 - pt) * (1 - q[(k + 1):(2 * k)]),
               px * pt * (1 - q[1:k]),
               px * (1 - pt) * q[(k + 1):(2 * k)],
               px * pt * q[1:k])][]
}

#' Sample data from a DGP look-up table.
#'
#' @param dgp A look-up table created with \code{dgp_lookup}.
#' @param n number of observations to draw.
#'
#' @export
sample_dgp <- function(dgp, n) {
  dgpc <- copy(dgp$x_ret)
  out <- dgpc[sample(1:nrow(dgpc), size = n, replace = TRUE, prob = dgpc$p), ]
  out[, p := NULL][]
}

alt_01 <- function(meta, x, cats = 2) {
  ((meta - 1) %/% x) %% cats
}

#' Calculate true ATE from DGP look-up table.
#'
#' This function isn't used for the truth calculations in simulations.
#' Instead it serves as a convenience function for testing.
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
