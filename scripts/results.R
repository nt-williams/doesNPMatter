library(tidyverse)
library(patchwork)

source("R/utils.R")

read_zip <- function(tar) {
  files <- unzip(tar, list = TRUE)$Name
  p <- progressr::progressor(along = 1:length(files))
  purrr::map(files, function(file) {
    p()
    con <- unz(tar, file)
    read.csv(con)
  })
}

dgp <- "DGP_5_1_3_FALSE"

DGPs <- map_dfr(1:250, function(id) {
  meta <- read_zipped_dgp(dgp, id)
  data.frame(
    id = id,
    truth = meta$truth, 
    bias = meta$bias,
    eta = meta$eta, 
    rho = meta$rho
  )
})

res <-
  read_zip(glue::glue("data/sims/sim_{dgp}.zip") )|> 
  bind_rows() |> 
  left_join(DGPs)

bias <- group_by(res, id, n) |> 
  summarise(across(c("cbps", "gcomp", "bart", "cvtmle"), \(x) mean(x - truth)))

mse <- group_by(res, id, n) |> 
  summarise(across(c("cbps", "gcomp", "bart", "cvtmle"), \(x) mean((x - truth)^2)))

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

ragg::agg_png(glue::glue("figs/bias_{dgp}.png"), width = 8, height = 4.5, units = "cm", res = 400)
map2(c(100, 500, 1000), c("N = 100", "500", "1000"),
    \(N, sub) ecdf_plot(filter(bias, n == N), "|Bias|", sub)) |> 
  reduce(`+`) + 
  plot_layout(guides = "collect")
dev.off()

ragg::agg_png(glue::glue("figs/mse_{dgp}.png"), width = 8, height = 4.5, units = "cm", res = 400)
map2(c(100, 500, 1000), c("N = 100", "500", "1000"), 
    \(N, sub) ecdf_plot(filter(mse, n == N), "MSE", sub)) |> 
  reduce(`+`) + 
  plot_layout(guides = "collect")
dev.off()
