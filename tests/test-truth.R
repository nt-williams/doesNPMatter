library(devtools)

load_all()

odds <- function(p) {
  p / (1 - p)
}

odr <- function(x, y) {
  odds(x) / odds(y)
}

trt_1 <- function(x, size) {
  out <- vector("numeric", size)
  for (i in 1:size) {
    out[i] <- sum(x[seq(i, by = size, length.out = size)])
  }
  out
}

trt_0 <- function(x, size) {
  seqs <- seq_along(x)
  as.vector(tapply(x ,rep(seqs, each = size)[seqs], FUN = sum))
}

truth_ordinal <- function(probs, size) {
  a1 <- rev(trt_1(probs, size))
  a0 <- rev(trt_0(probs, size))
  out <- vector("numeric", size-1)
  for (i in 1:(size-1)) {
    out[i] <- (sum(a1[1:i]) / sum(setdiff(a1, a1[1:i]))) / 
      (sum(a0[1:i]) / sum(setdiff(a0, a0[1:i])))
  }
  log(mean(out))
}

test <- gendata(1e5, 1, 20, 1, 0, TRUE, 4534211, TRUE)
gendata(1e5, 1, 2, 0, 100, TRUE, 5432623, FALSE)

bias_tte_truth(test$data[[1]], 0)
truth_tte(test$nip, 14, 20)  

aor(test[[2]], 8)
bias_ordinal_tmle(test[[1]])
bias_ordinal_param(test[[1]])


y_12 <- sum(test[[2]][c(1, 4, 7)])
y_02 <- sum(test[[2]][c(1, 2, 3)])
y_11 <- sum(test[[2]][c(2, 5, 8)])
y_01 <- sum(test[[2]][c(4, 5, 6)])
y_10 <- sum(test[[2]][c(3, 6, 9)])
y_00 <- sum(test[[2]][c(7, 8, 9)])

or2 <- (y_12 / sum(y_11, y_10)) / (y_02 / sum(y_01, y_00))
or1 <- (sum(y_12, y_11) / y_10) / (sum(y_02, y_01) / y_00)

1 / mean(c(or2, or1))

y_12 <- sum(test[[2]][c(3, 6, 9)])
y_02 <- sum(test[[2]][c(7, 8, 9)])
y_11 <- sum(test[[2]][c(2, 5, 8)])
y_01 <- sum(test[[2]][c(4, 5, 6)])
y_10 <- sum(test[[2]][c(1, 4, 7)])
y_00 <- sum(test[[2]][c(1, 2, 3)])

or2 <- (y_12 / sum(y_11, y_10)) / (y_02 / sum(y_01, y_00))
or1 <- (sum(y_12, y_11) / y_10) / (sum(y_02, y_01) / y_00)

mean(c(or2, or1))

sum(test[[2]][c(1,3)]) - sum(test[[2]][c(1,2)])

exp(bias_ordinal_truth(test[[1]][[1]], 5))
exp(bias_ordinal_tmle(test[[1]]))
exp(bias_ordinal_param(test[[1]]))


meta <- 1:(cont_cnf * size * 2 * 2)
y0  <- alt_01(meta, cont_cnf * size * 2^(binary_cnf + 1), size)
y1  <- alt_01(meta, cont_cnf * 2^(binary_cnf + 1), size)
trt <- alt_01(meta, cont_cnf * 2^binary_cnf)
cnf <- cbind(mod_op(meta, cont_cnf))
## P(Y(1) = 1 | X = x) for x = 1,...,100
proby1 <- sapply(1:cont_cnf, function(x) sum(nip[y1 == 1 & cnf[, 1] == x]) / sum(nip[cnf[, 1] == x]))
## P(Y(0) = 1 | X = x) for x = 1,...,100
proby0 <- sapply(1:cont_cnf, function(x) sum(nip[y0 == 1 & cnf[, 1] == x]) / sum(nip[cnf[, 1] == x]))
px <- sapply(1:cont_cnf, function(x) sum(nip[cnf[, 1] == x]))
variation_norm1 <- mean(abs(diff(proby1)))
variation_norm0 <- mean(abs(diff(proby0)))
true_ate <- sum((proby1 - proby0) * px)
## equal to
sum(nip_y[c(1,3)]) - sum(nip_y[c(1,2)])
