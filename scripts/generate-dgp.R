# Nick Williams
# Research Biostatistician
# Department of Population Health Sciences
# Weill Cornell Medicine

# Generates and saves data generating processes
# Confounding bias is drawn from a Uniform distribution with support -0.5 and 0.5

box::use(DGP = ../R/dgp)

K <- 5000
xs <- 5
gamma <- 20

set.seed(214345)
etas <- runif(K, min = -0.5, max = 0.5)
gammas <- rep(1000, K)

dgps <- purrr::map2(etas, gammas, ~ DGP$dgp(xs, .x, .y))

saveRDS(dgps, "./data/DGP_5000_5bin_BiasUniform_PosUniform.rds")
