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

truth_binary <- function(probs) {
  sum(probs[c(1,3)]) - sum(probs[c(1,2)])
}

truth_tte <- function(probs, time, tau) {
  h1 <- rev(trt_1(probs, tau))
  h0 <- rev(trt_0(probs, tau))
  cat((1 - sum(h1[1:time])), (1 - sum(h0[1:time])))
  (1 - sum(h1[1:time])) - (1 - sum(h0[1:time]))
}
