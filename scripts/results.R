suppressPackageStartupMessages(library(tidyverse))
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

dgp <- "DGP_5_1_1_FALSE"

calc_max_ipw <- function(dgp) {
  dgp$x_ret |> 
    group_by(across(starts_with("x"))) |> 
    mutate(px = sum(p)) |> 
    ungroup() |> 
    group_by(across(starts_with("x")), t) |> 
    mutate(ptandx = sum(p)) |> 
    ungroup() |> 
    group_by(across(starts_with("x"))) |> 
    mutate(pt = ptandx / px) |> 
    ungroup() |> 
    (\(x) max(1 / x$pt))()
}

DGPs <- map_dfr(1:500, function(id) {
  meta <- try(read_dgp(dgp, id))
  if (inherits(meta, "try-error")) return(NULL)
  data.frame(
    id = id,
    truth = meta$truth, 
    bias = meta$bias,
    eta = meta$eta, 
    rho = meta$rho, 
    max_ipw = calc_max_ipw(meta)
  )
})

res <-
  read_zip(glue::glue("data/sims/sim_{dgp}.zip")) |> 
  bind_rows() |> 
  left_join(DGPs)

saveRDS(res, glue::glue("data/sims/res_{dgp}.rds"))

covered <- \(l, u, truth) between(truth, l, u)

coverage <- group_by(res, id, n) |> 
  summarise(cbps = mean(pmap_lgl(list(cbps.conf.low, cbps.conf.high, truth), covered)), 
            gcomp = mean(pmap_lgl(list(gcomp.conf.low, gcomp.conf.high, truth), covered)), 
            bart = mean(pmap_lgl(list(bart.conf.low, bart.conf.high, truth), covered)), 
            tmle = mean(pmap_lgl(list(tmle.conf.low, tmle.conf.high, truth), covered)))

coverage <- select(res, id, eta, rho, max_ipw) |> 
  distinct() |> 
  left_join(coverage)

saveRDS(coverage, glue::glue("data/sims/coverage_{dgp}.rds"))

# WORK FROM HERE
dgp <- "DGP_5_1_3_FALSE"
res <- readRDS(glue::glue("data/sims/res_{dgp}.rds"))

bias <- group_by(res, id, n) |> 
  summarise(across(ends_with("psi"), \(x) mean(x - truth)))

mse <- group_by(res, id, n) |> 
  summarise(across(ends_with("psi"), \(x) mean((x - truth)^2)))

pt_to_mm <- function(pt) {
  pt / ggplot2::.pt
}

ecdf_plot <- function(data, xlab, subtitle, xlim, seq = 0.05) {
  pts <- 0.75
  ts <- 7
  ggplot(data) +
    geom_line(aes(x = abs(cbps.psi), y = 1 - ..y.., color = "#271F30"), 
              stat = 'ecdf', alpha = 0.65, size = pt_to_mm(0.9)) +
    geom_line(aes(x = abs(gcomp.psi), y = 1 - ..y.., color = "#548C64"), 
              stat = 'ecdf', alpha = 0.65, size = pt_to_mm(0.9)) + 
    geom_line(aes(x = abs(bart.psi), y = 1 - ..y.., color = "#3C7A89"), 
              stat = 'ecdf', alpha = 0.65, size = pt_to_mm(0.9)) + 
    geom_line(aes(x = abs(tmle.psi), y = 1 - ..y.., color = "#C03221"), 
              stat = 'ecdf', alpha = 0.65, size = pt_to_mm(0.9)) + 
    scale_color_identity(
      breaks = c("#271F30", "#548C64", "#3C7A89", "#C03221"),
      labels = c("CBPS", "G-comp.", "BART", "TMLE"),
      guide = "legend"
    ) + 
    labs(
      x = xlab, 
      y = "P[X>x]", 
      colour = "Estimator", 
      subtitle = subtitle
    ) + 
    theme_classic(
      # base_size = 4, 
      # base_line_size = 0.2,
      # base_rect_size = 0.2
      ) + 
    coord_cartesian(xlim = xlim) +
    scale_x_continuous(expand = c(0.01, 0), 
                       breaks = seq(0, xlim[2], by = seq)) + 
    scale_y_continuous(expand = c(0.01, 0)) + 
    theme(text = element_text(size = ts),
          legend.position = {
            if (subtitle == "1000") c(0.85, 0.9)
            else "none"
          },
          legend.background = element_rect(fill = "white",
                                           color = "black",
                                           size = pt_to_mm(pts)),
          axis.text = element_text(size = ts, colour = "black"),
          axis.line = element_line(size = pt_to_mm(pts)),
          axis.ticks = element_line(size = pt_to_mm(pts)),
          legend.text = element_text(size = ts), 
          legend.key.size = unit(0.2, 'cm')) + 
    guides(guide_legend(override.aes = list(size = 0.1)))
}

width <- 6.99866
height <- width / 3

ragg::agg_png(glue::glue("figs/bias_{dgp}.png"), width = width, height = height, units = "in", res = 400)
print({
  map2(c(100, 500, 1000), c("N = 100", "500", "1000"),
       \(N, sub) ecdf_plot(filter(bias, n == N), "|Bias|", sub, c(0, 0.1))) |> 
    reduce(`+`) 
  # + 
  #   plot_layout(guides = "collect")
})
dev.off()

ragg::agg_png(glue::glue("figs/mse_{dgp}.png"), width = width, height = height, units = "in", res = 400)
print({
  map2(c(100, 500, 1000), c("N = 100", "500", "1000"),
       \(N, sub) ecdf_plot(filter(mse, n == N), "MSE", sub, c(0, 0.06), 0.01)) |> 
    reduce(`+`) 
  # + 
  #   plot_layout(guides = "collect")
})
dev.off()
