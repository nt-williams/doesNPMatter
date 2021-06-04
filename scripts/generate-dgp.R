# Nick Williams
# Research Biostatistician
# Department of Population Health Sciences
# Weill Cornell Medicine

# Generates and saves data generating processes
# Confounding bias is drawn from a Uniform distribution with support -0.5 and 0.5

box::use(DGP = ../R/dgp)

K <- 1000
xs <- 5
gamma <- 20

set.seed(214345)
etas <- runif(K, min = -0.5, max = 0.5)

dgps <- lapply(etas, function(eta) {
  DGP$dgp(xs, eta, gamma)
})

saveRDS(dgps, "./data/DGP_5_uniform_20.rds")
