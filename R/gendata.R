#' Generate simulation data
#'
#' @param n The sample size.
#' @param reps How many datasets should be generated from the DGM.
#' @param size Number of categories in Y, default is 2.
#' @param rho Logistic distribution informative prior, default is 0.
#' @param binary_cnf Number of binary confounders.
#' @param cont_cnf Size of a continuous confounder.
#' @param randomized Simulate an randomized controlled trial (i.e., probability of treatment is 0.5)?
#' @param seed Seed for reproducibility.
#'
#' @return A `data.table` of the simulated data.
#' 
#' @author Nicholas Williams and Iván Díaz
gendata <- function(n, reps, size = 2, rho = 0, binary_cnf, cont_cnf, 
                    randomized = FALSE, seed) {
  mixture <- rbinom(1, 1, rho)
  nip_y <- rdirichlet(1, rep(1, size^2))[1, ]
  
  cont_cnf <- ifelse(cont_cnf == 0, 1, cont_cnf)
  
  if (!randomized) {
    nip_a <- rdirichlet(1, rep(1, 2))[1, ]
  } else {
    nip_a <- rep(1/2, 2)
  }
  
  nip <- c()
  for (i in 1:(size^2)) {
    if (!randomized) {
      for (j in seq_along(nip_a)) {
        nip <- c(nip_y[i] * nip_a[j] * rdirichlet(1, rep(1, 2^binary_cnf * cont_cnf))[1, ], nip)
      }
    } else {
      nip_w <- rdirichlet(1, rep(1, 2^binary_cnf * cont_cnf))[1, ]
      for (j in seq_along(nip_a)) {
        nip <- c(nip_y[i] * nip_a[j] * nip_w, nip)
      }
    }
  }
  
  if (mixture == 1) {
    ip <- gen_inform_prior(size, binary_cnf + ifelse(cont_cnf == 1, 0, 1), seed)
  }

  replicate(reps, {
    meta <- rcat(n, prob = nip)
    y0  <- alt_01(meta, cont_cnf * size * 2^(binary_cnf + 1), size)
    y1  <- alt_01(meta, cont_cnf * 2^(binary_cnf + 1), size)
    trt <- alt_01(meta, cont_cnf * 2^binary_cnf)
    
    cnf <- cbind(mod_op(meta, cont_cnf))
    if (binary_cnf > 0) {
      cnf_cat <- mod_op(meta, cont_cnf * 2^binary_cnf)
      if (cont_cnf > 1) {
        cnf <- cbind(cnf, sapply(1:binary_cnf, function(x) alt_01(cnf_cat, cont_cnf * 2^(x - 1))))
        colnames(cnf) <- paste0("cnf", 1:(binary_cnf + 1))
      } else {
        cnf <- sapply(1:binary_cnf, function(x) alt_01(cnf_cat, cont_cnf * 2^(x - 1)))
        colnames(cnf) <- paste0("cnf", 1:(binary_cnf))
      }
    } else {
      colnames(cnf) <- "cnf1"
    }
    
    if (mixture == 0) {
      outcome <- trt * y1 + (1 - trt) * y0
    } else if (mixture == 1) {
      outcome <- inform_prior(ip, trt, cnf, size)
    }
    
    data.table(trt, cnf, outcome)
  }, simplify = FALSE)
}
