suppressPackageStartupMessages({
  library(tidyverse)
  library(patchwork)
  library(glue)
  library(ragg)
})

source("R/utils.R")

dgp <- "DGP_5_1_3_TRUE"
res <- readRDS(glue("data/sims/res_{dgp}.rds"))

bias <- group_by(res, id, n) |> 
  summarise(across(ends_with("psi"), \(x) mean(x - truth)))

mse <- group_by(res, id, n) |> 
  summarise(across(ends_with("psi"), \(x) mean((x - truth)^2)))

quantile_at_x <- function(vals, x) {
  f <- ecdf(vals)
  f(x)
}

filter(bias, n == 1000) |> 
  ungroup() |> 
  summarise(across(cbps.psi:tmle.psi, ~ 1 - quantile_at_x(.x, 0.1)))

width <- 6.99866
height <- width / 3

agg_png(glue("figs/bias_{dgp}.png"), width = width, height = height, units = "in", res = 400)
print({
  map2(c(100, 500, 1000), c("N = 100", "500", "1000"),
       \(N, sub) ecdf_plot(filter(bias, n == N), "|Bias|", sub, c(0, 0.25))) |>
    reduce(`+`)
})
dev.off()

agg_png(glue("figs/mse_{dgp}.png"), width = width, height = height, units = "in", res = 400)
print({
  map2(c(100, 500, 1000), c("N = 100", "500", "1000"),
       \(N, sub) ecdf_plot(filter(mse, n == N), "MSE", sub, c(0, 0.06), 0.01)) |>
    reduce(`+`)
})
dev.off()

# stratifying by positivity -----------------------------------------------

res <- group_by(res, id) |> 
  nest(thetas = cbps.psi:tmle.conf.high) |> 
  mutate(positivity_violation = 
           cut(max_ipw, 
               c(0, 50, 100, 2000), 
               labels = c("minimal", "moderate", "severe"), 
               include.lowest = TRUE)) |> 
  unnest(thetas)

bias <- left_join(bias, {
  select(res, id, positivity_violation) |>
    distinct() |>
    ungroup()
})

mse <- left_join(mse, {
  select(res, id, positivity_violation) |> 
    distinct() |> 
    ungroup()
})

agg_png(glue("figs/bias_positivity_{dgp}.png"), width = width, height = height, units = "in", res = 400)
print({
  map2(c("minimal", "moderate", "severe"),
       c("Pos. violation: minimal", "moderate", "severe"),
       \(pos, sub) ecdf_plot(filter(bias, n == 1000, positivity_violation == pos),
                             "|Bias|", sub, c(0, 0.25))) |>
    reduce(`+`)
})
dev.off()

agg_png(glue("figs/mse_positivity_{dgp}.png"), width = width, height = height, units = "in", res = 400)
print({
  map2(c("minimal", "moderate", "severe"), 
       c("Pos. violation: minimal", "moderate", "severe"),
       \(pos, sub) ecdf_plot(filter(mse, n == 1000, positivity_violation == pos), 
                             "MSE", sub, c(0, 0.05), 0.01)) |> 
    reduce(`+`) 
})
dev.off()
