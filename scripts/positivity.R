suppressPackageStartupMessages(library(tidyverse))
library(patchwork)
library(glue)

dgp <- "DGP_5_1_1_FALSE"
dat <- read_rds(glue("data/sims/res_{dgp}.rds"))

res <- group_by(dat, id) |> 
  nest(thetas = cbps.psi:tmle.conf.high)

res <- mutate(res, positivity_violation = 
         cut(max_ipw, 
             c(0, 50, 100, 2000), 
             labels = c("minimal", "moderate", "severe"), 
             include.lowest = TRUE)) |> 
  unnest(thetas)

bias <- group_by(res, id, n, positivity_violation) |> 
  summarise(across(ends_with("psi"), \(x) mean(x - truth))) |> 
  ungroup()

ecdf_plot <- function(data, xlab, subtitle) {
  ggplot(data) +
    geom_line(aes(x = abs(cbps.psi), y = 1 - ..y.., color = "blue"), 
              stat = 'ecdf', alpha = 0.65, size = 0.2) +
    geom_line(aes(x = abs(gcomp.psi), y = 1 - ..y.., color = "red"), 
              stat = 'ecdf', alpha = 0.65, size = 0.2) + 
    geom_line(aes(x = abs(bart.psi), y = 1 - ..y.., color = "green"), 
              stat = 'ecdf', alpha = 0.65, size = 0.2) + 
    geom_line(aes(x = abs(tmle.psi), y = 1 - ..y.., color = "orange"), 
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
    theme_light(base_size = 2, 
                base_line_size = 0.2,
                base_rect_size = 0.2) + 
    theme(legend.text = element_text(size = 2), 
          legend.key.size = unit(0.05, "cm")) + 
    guides(guide_legend(override.aes = list(size = 0.25)))
}

ragg::agg_png(glue::glue("figs/bias_positivity_{dgp}.png"), width = 8, height = 4.5, units = "cm", res = 400)
map(c(100, 500, 1000), 
    function(n) {
      map(c("minimal", "moderate", "severe"), 
          function(amount) {
            ecdf_plot(bias[bias$n == n & bias$positivity_violation == amount, ], 
                      "|Bias|", glue::glue("N = {n}, {amount} pos. violation"))
          })
    }) |> 
  unlist(recursive = FALSE) |> 
  reduce(`+`) |> 
  plot_layout(guides = "collect", ncol = 3, nrow = 3) |> 
  print()
dev.off()
