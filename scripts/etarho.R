suppressPackageStartupMessages(library(tidyverse))
library(patchwork)
source("R/dgp.R")
source("R/utils.R")

xnum_vals <- seq(0, 1, length.out = 100)

xx_num <- as.matrix(expand.grid(replicate(1, xnum_vals, simplify = FALSE)))
colnames(xx_num) <- paste0('x_num', 1:ncol(xx_num))

eta <- runif(1, 0.1, 10)
rho <- runif(1, 0.1, 10)

# Draw Gaussian process for treatment
Sigma <- cov_matrix(xx_num, se_kernel, eta = eta, rho = rho)

comb <- expand.grid(eta = c(0, 10, 100), 
            rho = c(0, 10, 100))

plots <- pmap(comb, function(eta, rho) {
  pts <- 0.75
  ts <- 7
  tmp <- dgp(n_bin = 1, n_num = 1, inter_order = 1, hte = FALSE, conf_bias = 0.01,
             pos_bound = 100, npoints = 100, eta = eta, rho = rho, tol = 0.01)
  
  filter(tmp$x_ret, t == 1, y == 1, x_bin1 == 1) |> 
    ggplot(aes(x = x_num1, y = pt)) + 
    geom_line(size = pt_to_mm(0.9)) + 
    labs(x = "X", y = "P[T=1|X]", 
         subtitle = glue::glue("eta: {eta}, rho:{rho}")) + 
    theme_classic() + 
    theme(
      text = element_text(size = ts),
      axis.text = element_text(size = ts, colour = "black"),
      axis.line = element_line(size = pt_to_mm(pts)),
      axis.ticks = element_line(size = pt_to_mm(pts))
    )
})

width <- 6.99866
height <- width / 1.5

ragg::agg_png(glue::glue("figs/eta_rho_propensity.png"), width = width, height = height, units = "in", res = 400)
wrap_plots(plots, ncol = 3)
dev.off()
