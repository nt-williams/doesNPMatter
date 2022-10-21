suppressPackageStartupMessages(library(tidyverse))
library(patchwork)

source("R/utils.R")

res <- map_dfr(list.files("data/sims", pattern = "res_(.+).rds", full.names = TRUE), 
               readRDS)

res <- 
  group_by(res, dgp, id) |> 
  nest(data = n:tmle.conf.high) |> 
  ungroup()

hist(res$rho)
hist(res$truth)
hist(res$bias)
hist(res$eta)

ecdf_plot <- function(data, xlab, subtitle) {
  ggplot(data) +
    geom_line(aes(x = abs(cbps), y = 1 - ..y.., color = "blue"), 
              stat = 'ecdf', alpha = 0.65, size = 0.2) +
    geom_line(aes(x = abs(gcomp), y = 1 - ..y.., color = "red"), 
              stat = 'ecdf', alpha = 0.65, size = 0.2) + 
    geom_line(aes(x = abs(bart), y = 1 - ..y.., color = "green"), 
              stat = 'ecdf', alpha = 0.65, size = 0.2) + 
    geom_line(aes(x = abs(cvtmle), y = 1 - ..y.., color = "orange"), 
              stat = 'ecdf', alpha = 0.65, size = 0.2) + 
    scale_color_identity(
      breaks = c("blue", "red", "green", "orange"),
      labels = c("CBPS", "G-comp.", "BART", "TMLE"),
      guide = "legend"
    ) + 
    labs(
      x = xlab, 
      y = "P[X>x]", 
      colour = "Estimator", 
      subtitle = subtitle
    ) + 
    theme_light(base_size = 4, 
                base_line_size = 0.2,
                base_rect_size = 0.2) + 
    theme(legend.text = element_text(size = 3), 
          legend.key.size = unit(0.15, "cm")) + 
    guides(guide_legend(override.aes = list(size = 0.25)))
}

filter(res, between(rho, 8, 10)) |> 
  unnest(cols = c(data)) |> 
  group_by(dgp, id, n) |> 
  summarise(across(c("cbps", "gcomp", "bart", "cvtmle"), \(x) mean(x - truth))) |> 
  ungroup() |> 
  filter(n == 1000) |> 
  ecdf_plot("|Bias|", "fme")

filter(res, between(eta, 8, 10)) |> 
  unnest(cols = c(data)) |> 
  group_by(dgp, id, n) |> 
  summarise(across(c("cbps", "gcomp", "bart", "cvtmle"), \(x) mean(x - truth))) |> 
  ungroup() |> 
  filter(n == 1000) |> 
  ecdf_plot("|Bias|", "fme")
