meta <- 1:(cont_cnf * size * 2 * 2)
meta <- 1:128
y0  <- alt_01(meta, cont_cnf * size * 2^(binary_cnf + 1), size)
y1  <- alt_01(meta, cont_cnf * 2^(binary_cnf + 1), size)
trt <- alt_01(meta, 1)
cnf_meta <- alt_01(meta, 2, 2^binary_cnf * cont_cnf) + 1

  cnf_bin <- NULL

  cnf_cont <- mod_op(cnf_meta, cont_cnf)
  cnf <- cbind(cnf_cont, cnf_bin)
colnames(cnf) <- paste0("cnf", 1:(ncol(cnf)))

proby1 <- sapply(1:cont_cnf, function(x) sum(nip[y1 == 1 & cnf[, 1] == x]) / sum(nip[cnf[, 1] == x]))
proby0 <- sapply(1:cont_cnf, function(x) sum(nip[y0 == 1 & cnf[, 1] == x]) / sum(nip[cnf[, 1] == x]))
#P(A=1|W)

probA1 <- sapply(1:cont_cnf, function(x) sum(nip[trt == 1 & cnf[, 1] == x]) / sum(nip[cnf[, 1] == x]))
probW <- sapply(1:cont_cnf, function(x) sum(nip[cnf[, 1] == x]))
p1 <- max(probW / probA1)
p0 <- max(probW / (1 - probA1))
