suppressPackageStartupMessages({
  library(tidyverse)
  library(glue)
  library(patchwork)
  library(ragg)
})

source("R/utils.R")

dgp <- "DGP_5_1_2_FALSE"
res <- readRDS(glue("data/sims/res_{dgp}.rds"))
res2 <- readRDS(glue("data/sims/res_cvtmle_{dgp}.rds"))

Var <- function(x) {
  sum((x - mean(x))^2) / (length(x) - 1)
}

MSE <- function(x, truth) {
  sum((x - truth)^2) / length(x)
}

mcse_bias <- function(x) {
  n <- length(x) 
  sqrt(Var(x) / n)
}

mcse_var <- function(x) {
  n <- length(x)
  Var(x) * sqrt(2 / (n - 1))
}

mcse_mse <- function(x, truth) {
  n <- length(x)
  sqrt(sum(((x - truth)^2 - MSE(x, truth))^2) / (n * (n - 1)))
}

res <- select(res, dgp, n, id, truth, ends_with("psi")) |> 
  group_by(dgp, n, id) |> 
  nest(psi = ends_with('psi'))

res2 <- select(res2, dgp, n, id, truth, ends_with("psi")) |> 
  group_by(dgp, n, id) |> 
  nest(psi = ends_with('psi'))

mcse1 <- select(res, dgp, n, id) |> 
  ungroup() |> 
  mutate(mcse_bias = map_dfr(res$psi, \(x) summarise(x, across(everything(), mcse_bias))) |> 
           pmap_dbl(max), 
         mcse_var = map_dfr(res$psi, \(x) summarise(x, across(everything(), mcse_var))) |> 
           pmap_dbl(max), 
         mcse_mse = map2_dfr(res$psi, res$truth, \(x, y) summarise(x, across(everything(), ~ mcse_mse(.x, y)))) |> 
           pmap_dbl(max))

mcse2 <- mutate(res2, 
                mcse_bias = map_dbl(psi, \(x) mcse_bias(x$cvtmle.psi)), 
                mcse_var = map_dbl(psi, \(x) mcse_var(x$cvtmle.psi)), 
                mcse_mse = map2_dbl(psi, truth, \(x, y) mcse_mse(x$cvtmle.psi, y))) |> 
  select(-psi, -truth) |> 
  ungroup()

mcse <- left_join(mcse1, mcse2, by = c("dgp", "n", "id"))

mcse <- mutate(mcse, 
       mcse_bias = pmax(mcse_bias.x, mcse_bias.y, na.rm = TRUE), 
       mcse_var = pmax(mcse_var.x, mcse_var.y, na.rm = TRUE), 
       mcse_mse = pmax(mcse_mse.x, mcse_mse.y, na.rm = TRUE)) |> 
  select(-ends_with(".x"), -ends_with(".y"))

width <- 6.99866 + 1
height <- width - 1
agg_png(glue("figs/mscse.png"), width = width, height = height, units = "in", res = 400)
pts <- 0.75
ts <- 7
map2(c("mcse_bias", "mcse_var", "mcse_mse"), 
     c("bias", "variance", "MSE"), 
     function(x, label) {
       mutate(mcse, n = factor(glue("n = {n}"), levels = c("n = 100", "n = 500", "n = 1000"))) |> 
       ggplot(aes(.data[[x]])) + 
         geom_histogram(col = "black", fill = "black") + 
         labs(
           x = glue("Maximum MCSE {label}"), 
           y = NULL
         ) + 
         facet_grid(cols = vars(n)) + 
         theme_classic() + 
         theme(strip.background = element_blank(), 
               text = element_text(size = ts), 
               axis.text = element_text(size = ts, colour = "black"),
               axis.line = element_line(size = pt_to_mm(pts)),
               axis.ticks = element_line(size = pt_to_mm(pts)), 
               strip.text.x = element_text(size = ts, colour = "black"),
               axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))
     }) |> 
  reduce(`/`) |> 
  print()
dev.off()
