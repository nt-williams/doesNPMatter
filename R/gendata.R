#' Generate simulation data
#'
#' @param n sample size.
#' @param size number of categories in Y, default is 2.
#' @param rho logistic distribution informative prior, default is 0.
#' @param binary_cnf number of binary confounders, NULL means none.
#' @param cont_cnf number of continuous confounders, NUL means none.
#' @param mu 
#' @param sigma 
#' @param randomized simulate an randomized controlled trial (i.e., probability of treatment is 0.5)?
#'
#' @return A `data.table` of the simulated data.
#' 
#' @author Nicholas Williams and Ivan Dìaz
gendata <- function(n, size = 2, rho = 0, binary_cnf = NULL, cont_cnf = NULL, 
                    mu = NULL, sigma = NULL, randomized = FALSE) {
  mixture <- rbinom(1, 1, rho)
  nip_y <- rdirichlet(1, rep(1, size^2))[1, ]
  
  binary_cnf <- ifelse(is.null(binary_cnf), 0, binary_cnf)
  cont_cnf <- ifelse(is.null(cont_cnf), 1, cont_cnf)
  
  if (!randomized) {
    nip_a <- rdirichlet(1, rep(1, 2))[1, ] # Ivan had originally selected the column and not the row? 
  } else {
    nip_a <- rep(1/2, 2)
  }
  
  nip <- c()
  for (i in 1:(size^2)) {
    if (!randomized) {
      for(j in seq_along(nip_a)) {
        nip <- c(nip_y[i] * nip_a[j] * rdirichlet(1, rep(1, 2^binary_cnf * cont_cnf))[1, ], nip)
      }
    } else {
      nip_w <- rdirichlet(1, rep(1, 2^binary_cnf * cont_conf))[1, ]
      for(j in seq_along(nipa)) {
        nip <- c(nip_y[i] * nip_a[j] * nip_w, nip)
      }
    }
  }
  
  meta <- rcat(n, prob = nip)
  y0  <- alt_01(meta, cont_cnf * size * 2^(binary_cnf + 1), size)
  y1  <- alt_01(meta, cont_cnf * 2^(binary_cnf + 1), size)
  trt <- alt_01(meta, cont_cnf * 2^binary_cnf)

  cnf <- cbind(mod_op(meta, cont_cnf))
  if (binary_cnf > 0) {
    cnf_cat <- mod_op(meta, cont_cnf * 2^binary_cnf)
    cnf <- cbind(cnf, sapply(1:binary_cnf, function(x) alt_01(cnf_cat, cont_cnf * 2^(x - 1))))
  }
  
  colnames(cnf) <- paste0("cnf", 1:(binary_cnf + 1))
  
  if (mixture == 0) {
    outcome <- trt * y1 + (1 - trt) * y0
  } else if (mixture == 1) {
    # going to require some metaprogramming
    # ip <- mvrnorm(1, mu = mu, Sigma = sigma)
    # outcome <- rbinom(n, 1, 
    #                   plogis(ip[1] + 
    #                            ip[2] * trt + 
    #                            ip[3] * cnf + 
    #                            ip[4] * trt * cnf))
  }
  
  data.table(trt, cnf, outcome)
}
