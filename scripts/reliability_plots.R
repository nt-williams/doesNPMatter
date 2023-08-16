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

bias <- map(list(res, res2, res3), function(data) {
  group_by(data, id, n) |> 
    summarise(across(ends_with("psi"), \(x) mean(x - truth)))
})

mse <- map(list(res, res2, res3), function(data) {
  group_by(data, id, n) |> 
    summarise(across(ends_with("psi"), \(x) mean((x - truth)^2)))
})

quantile_at_x <- function(vals, x) {
  f <- ecdf(vals)
  f(x)
}

filter(bias[[1]], n == 1000) |> 
  ungroup() |> 
  summarise(across(cbps.psi:tmle.psi, ~ 1 - quantile_at_x(.x, 0.1)))

width <- 6.99866
height <- width / 3

agg_png(glue("figs/bias_{dgp}.png"), width = width, height = height, units = "in", res = 400)
print({
  map2(c(100, 500, 1000), c("N = 100", "500", "1000"),
       \(N, sub) ecdf_plot(filter(bias[[1]], n == N), 
                           filter(bias[[2]], n == N), 
                           filter(bias[[3]], n == N), 
                           "|Bias|", sub, c(0, 0.25))) |>
    reduce(`+`)
})
dev.off()

agg_png(glue("figs/mse_{dgp}.png"), width = width, height = height, units = "in", res = 400)
print({
  map2(c(100, 500, 1000), c("N = 100", "500", "1000"),
       \(N, sub) ecdf_plot(filter(mse[[1]], n == N), 
                           filter(mse[[2]], n == N), 
                           filter(mse[[3]], n == N),
                           "MSE", sub, c(0, 0.06), 0.01)) |>
    reduce(`+`)
})
dev.off()

# stratifying by positivity -----------------------------------------------

res <- map(list(res, res2, res3), function(data) {
  group_by(data, id) |> 
    nest(thetas = c(-dgp, -id, -n, -truth, -bias, -eta, -rho, -max_ipw)) |> 
    mutate(positivity_violation = 
             cut(max_ipw, 
                 c(0, 50, 100, 2000), 
                 labels = c("minimal", "moderate", "severe"), 
                 include.lowest = TRUE)) |> 
    unnest(thetas)
})

bias <- map2(bias, res, function(x, y) {
  left_join(x, {
    select(y, id, positivity_violation) |>
      distinct() |>
      ungroup()
  })
})

mse <- map2(mse, res, function(x, y) {
  left_join(x, {
    select(y, id, positivity_violation) |>
      distinct() |>
      ungroup()
  })
})

agg_png(glue("figs/bias_positivity_{dgp}.png"), width = width, height = height, units = "in", res = 400)
print({
  map2(c("minimal", "moderate", "severe"),
       c("Pos. violation: minimal", "moderate", "severe"),
       \(pos, sub) ecdf_plot(filter(bias[[1]], n == 1000, positivity_violation == pos),
                             filter(bias[[2]], n == 1000, positivity_violation == pos),
                             filter(bias[[3]], n == 1000, positivity_violation == pos),
                             "|Bias|", sub, c(0, 0.25))) |>
    reduce(`+`)
})
dev.off()

agg_png(glue("figs/mse_positivity_{dgp}.png"), width = width, height = height, units = "in", res = 400)
print({
  map2(c("minimal", "moderate", "severe"), 
       c("Pos. violation: minimal", "moderate", "severe"),
       \(pos, sub) ecdf_plot(filter(mse[[1]], n == 1000, positivity_violation == pos), 
                             filter(mse[[2]], n == 1000, positivity_violation == pos),
                             filter(mse[[3]], n == 1000, positivity_violation == pos),
                             "MSE", sub, c(0, 0.05), 0.01)) |> 
    reduce(`+`) 
})
dev.off()
