library(lmtp)
library(sl3)
library(progressr)

devtools::load_all()

b <- bias("binary", 4352346, 1e5, 1, 2, 4, 0, randomized = FALSE)
b2 <- bias("binary", 4352346, 1e5, 1, 2, 4, 0, randomized = TRUE)


bias_binary_truth(gendata(1e5, 1, 2, 4, 0, FALSE, 76846, FALSE)$data[[1]], 2)
