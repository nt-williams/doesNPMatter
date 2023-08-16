suppressPackageStartupMessages({
  library(tidyverse)
  library(patchwork)
  library(glue)
  library(ragg)
})

source("R/utils.R")

dgp <- "DGP_5_1_3_TRUE"
res <- readRDS(glue("data/sims/res_{dgp}.rds"))
res2 <- readRDS(glue("data/sims/res_cvtmle_{dgp}.rds"))
res3 <- readRDS(glue("data/sims/res_tci_{dgp}.rds"))

var1 <- group_by(res, id, n) |> 
  summarise(across(ends_with("psi"), var))

var2 <- group_by(res2, id, n) |> 
  summarise(across(ends_with("psi"), var))

var3 <- group_by(res3, id, n) |> 
  summarise(across(ends_with("psi"), var))

width <- 6.99866
height <- width / 3

agg_png(glue("figs/var_{dgp}.png"), width = width, height = height, units = "in", res = 400)
print({
  map2(c(100, 500, 1000), c("N = 100", "500", "1000"),
       \(N, sub) ecdf_plot(filter(var1, n == N), 
                           filter(var2, n == N), 
                           filter(var3, n == N), 
                           "Variance", sub, c(0, 0.018), 0.005)) |>
    reduce(`+`)
})
dev.off()
