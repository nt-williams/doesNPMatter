truth_ordinal <- function(probs, size) {
  a1 <- rev(marginal_1(probs, size))
  a0 <- rev(marginal_0(probs, size))
  out <- vector("numeric", size-1)
  for (i in 1:(size-1)) {
    out[i] <- (sum(a1[1:i]) / sum(setdiff(a1, a1[1:i]))) / 
      (sum(a0[1:i]) / sum(setdiff(a0, a0[1:i])))
  }
  log(mean(out))
}

marginal_1 <- function(x, size) {
  out <- vector("numeric", size)
  for (i in 1:size) {
    out[i] <- sum(x[seq(i, by = size, length.out = size)])
  }
  out
}

marginal_0 <- function(x, size) {
  seqs <- seq_along(x)
  as.vector(tapply(x, rep(seqs, each = size)[seqs], FUN = sum))
}

truth_binary <- function(probs) {
  sum(probs[c(1,3)]) - sum(probs[c(1,2)])
}

truth_tte <- function(probs, time, tau) {
  h1 <- rev(marginal_1(probs, tau))
  h0 <- rev(marginal_0(probs, tau))
  (1 - sum(h1[1:time]))# - (1 - sum(h0[1:time]))
}

# for now just going to assume we only can have a single continuous confounder and no other variables.
variation_norm <- function(nip, size, binary_cnf, cont_cnf) {
  meta <- 1:(cont_cnf * size * 2 * 2)
  y0  <- alt_01(meta, cont_cnf * size * 2^(binary_cnf + 1), size)
  y1  <- alt_01(meta, cont_cnf * 2^(binary_cnf + 1), size)
  trt <- alt_01(meta, cont_cnf * 2^binary_cnf)
  cnf <- cbind(mod_op(meta, cont_cnf))
  proby1 <- sapply(1:cont_cnf, function(x) sum(nip[y1 == 1 & cnf[, 1] == x]) / sum(nip[cnf[, 1] == x]))
  proby0 <- sapply(1:cont_cnf, function(x) sum(nip[y0 == 1 & cnf[, 1] == x]) / sum(nip[cnf[, 1] == x]))
  vn1 <- mean(abs(diff(proby1)))
  vn0 <- mean(abs(diff(proby0)))
  list(vn1 = vn1, vn0 = vn0)
}
