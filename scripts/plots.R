# Nick Williams
# Research Biostatistician
# Department of Population Health Sciences
# Weill Cornell Medicine

library(ggplot2)
library(grid)
library(gridExtra)

devtools::load_all()

files <- fs::dir_ls("./plots", regex = ".rds$")
plts <- map(files, readRDS)
names(plts) <- gsub(".rds", "", fs::path_file(files), ".rds$")

# observational -----------------------------------------------------------

leg <- g_legend(plts$bias_binary_f1o_0_100)
leg$vp$x <- unit(.9, 'npc')
leg$vp$y <- unit(.875, 'npc')

png(here::here("plots", "bias_binary_obs.png"), width = 14, 
    height = 7, units = "in", res = 600, pointsize = 4)
arrangeGrob(
  textGrob("N = 100,000", 
           gp = gpar(fontsize = 9, 
                     fontface = "italic")), 
  textGrob("N = 2,500", 
           gp = gpar(fontsize = 9, 
                     fontface = "italic")),
  textGrob("N = 500", 
           gp = gpar(fontsize = 9, 
                     fontface = "italic")), 
  textGrob("N = 250", 
           gp = gpar(fontsize = 9, 
                     fontface = "italic")),
  zeroGrob(),
  plts$bias_binary_ao_0_100 + theme(legend.position = "none"),
  plts$bias_binary_f1o_0_100 + theme(legend.position = "none"), 
  plts$bias_binary_f2o_0_100 + theme(legend.position = "none"), 
  plts$bias_binary_f3o_0_100 + theme(legend.position = "none"), 
  textGrob("Scenario 1", 
           rot = -90, 
           gp = gpar(fontsize = 9, 
                     fontface = "italic")),
  plts$bias_binary_ao_4_0 + theme(legend.position = "none"), 
  plts$bias_binary_f1o_4_0 + theme(legend.position = "none"), 
  plts$bias_binary_f2o_4_0 + theme(legend.position = "none"),
  plts$bias_binary_f3o_4_0 + theme(legend.position = "none"),
  textGrob("Scenario 2", 
           rot = -90, 
           gp = gpar(fontsize = 9, 
                     fontface = "italic")),
  nrow = 3, 
  heights = c(0.1, 1, 1), 
  widths = c(1, 1, 1, 1, 0.1)
) %>% 
  grid.arrange()
grid.draw(leg)
dev.off()

# MSE
leg <- g_legend(plts$mse_binary_fo_0_100)
leg$vp$x <- unit(.85, 'npc')
leg$vp$y <- unit(.875, 'npc')

png(here::here("plots", "mse_binary_obs.png"), width = 12, 
    height = 7, units = "in", res = 600, pointsize = 4)
arrangeGrob(
  textGrob("N = 2,500", 
           gp = gpar(fontsize = 9, 
                     fontface = "italic")),
  textGrob("N = 500", 
           gp = gpar(fontsize = 9, 
                     fontface = "italic")), 
  textGrob("N = 250", 
           gp = gpar(fontsize = 9, 
                     fontface = "italic")), 
  zeroGrob(),
  plts$mse_binary_f1o_0_100 + theme(legend.position = "none"), 
  plts$mse_binary_f2o_0_100 + theme(legend.position = "none"), 
  plts$mse_binary_f3o_0_100 + theme(legend.position = "none"),
  textGrob("Scenario 1", 
           rot = -90, 
           gp = gpar(fontsize = 9, 
                     fontface = "italic")),
  plts$mse_binary_f1o_4_0 + theme(legend.position = "none"), 
  plts$mse_binary_f2o_4_0 + theme(legend.position = "none"),
  plts$mse_binary_f3o_4_0 + theme(legend.position = "none"),
  textGrob("Scenario 2", 
           rot = -90, 
           gp = gpar(fontsize = 9, 
                     fontface = "italic")),
  nrow = 3, 
  heights = c(0.1, 1, 1), 
  widths = c(1, 1, 1, 0.1)
) %>% 
  grid.arrange()
grid.draw(leg)
dev.off()

# rct ---------------------------------------------------------------------

# 0_100
leg <- g_legend(plts$bias_binary_ar_0_100)
leg$vp$x <- unit(.9, 'npc')
leg$vp$y <- unit(.9, 'npc')

png(here::here("plots", "bias_0_100_rct.png"), width = 12, 
    height = 7, units = "in", res = 600, pointsize = 4)
arrangeGrob(
  textGrob("N = 100,000", 
           gp = gpar(fontsize = 9, 
                     fontface = "italic")), 
  textGrob("N = 2,500", 
           gp = gpar(fontsize = 9, 
                     fontface = "italic")), 
  textGrob("N = 500", 
           gp = gpar(fontsize = 9, 
                     fontface = "italic")),
  textGrob("N = 250", 
           gp = gpar(fontsize = 9, 
                     fontface = "italic")),
  zeroGrob(), 
  plts$bias_binary_ar_0_100 + theme(legend.position = "none"), 
  plts$bias_binary_f1r_0_100 + theme(legend.position = "none"), 
  plts$bias_binary_f2r_0_100 + theme(legend.position = "none"), 
  plts$bias_binary_f3r_0_100 + theme(legend.position = "none"), 
  textGrob("Binary", rot = -90, gp = gpar(fontsize = 9, 
                                          fontface = "italic")),
#   plts$bias_ordinal_ar_0_100 + theme(legend.position="none"), 
#   zeroGrob(), 
#   plts$bias_ordinal_fr_0_100 + theme(legend.position = "none"), 
#   textGrob("Ordinal", rot = -90, gp = gpar(fontsize = 9, 
#                                            fontface = "italic")),
  ncol = 5, 
  #heights = c(0.1, 1, 1),
  heights = c(0.1, 1),
  widths = c(1, 1, 1, 1, 0.1)
) %>% 
  grid.arrange()
grid.draw(leg)
dev.off()

# 4_0
leg <- g_legend(plts$bias_binary_ar_4_0)
leg$vp$x <- unit(.9, 'npc')
leg$vp$y <- unit(.9, 'npc')

png(here::here("plots", "bias_4_0_rct.png"), width = 12, 
    height = 7, units = "in", res = 600, pointsize = 4)
arrangeGrob(
  textGrob("N = 100,000", 
           gp = gpar(fontsize = 9, 
                     fontface = "italic")), 
  textGrob("N = 2,500", 
           gp = gpar(fontsize = 9, 
                     fontface = "italic")), 
  textGrob("N = 500", 
           gp = gpar(fontsize = 9, 
                     fontface = "italic")),
  zeroGrob(), 
  plts$bias_binary_ar_4_0 + theme(legend.position="none"), 
  plts$bias_binary_f2r_4_0 + theme(legend.position="none"), 
  plts$bias_binary_fr_4_0 + theme(legend.position="none"), 
  textGrob("Binary", rot = -90, gp = gpar(fontsize = 9, 
                                          fontface = "italic")),
  plts$bias_ordinal_ar_4_0 + theme(legend.position="none"), 
  zeroGrob(), 
  plts$bias_ordinal_fr_4_0 + theme(legend.position="none"), 
  textGrob("Ordinal", rot = -90, gp = gpar(fontsize = 9, 
                                           fontface = "italic")),
  ncol = 4, 
  heights = c(0.1, 1, 1),
  widths = c(1, 1, 1, 0.1)
) %>% 
  grid.arrange()
grid.draw(leg)
dev.off()

# MSE
leg <- g_legend(plts$mse_binary_fr_0_100)
leg$vp$x <- unit(.85, 'npc')
leg$vp$y <- unit(.875, 'npc')

png(here::here("plots", "mse_0_100_rct.png"), width = 12, 
    height = 7, units = "in", res = 600, pointsize = 4)
arrangeGrob(
  textGrob("N = 2,500", 
           gp = gpar(fontsize = 9, 
                     fontface = "italic")),
  textGrob("N = 500", 
           gp = gpar(fontsize = 9, 
                     fontface = "italic")), 
  zeroGrob(),
  plts$mse_binary_f2r_0_100 + theme(legend.position = "none"), 
  plts$mse_binary_fr_0_100 + theme(legend.position = "none"), 
  textGrob("Binary", 
           rot = -90, 
           gp = gpar(fontsize = 9, 
                     fontface = "italic")),
  zeroGrob(), 
  plts$mse_ordinal_fr_0_100 + theme(legend.position = "none"),
  textGrob("Ordinal", 
           rot = -90, 
           gp = gpar(fontsize = 9, 
                     fontface = "italic")),
  nrow = 3, 
  heights = c(0.1, 1, 1), 
  widths = c(1, 1, 0.1)
) %>% 
  grid.arrange()
grid.draw(leg)
dev.off()

leg <- g_legend(plts$mse_binary_fr_4_0)
leg$vp$x <- unit(.85, 'npc')
leg$vp$y <- unit(.875, 'npc')

png(here::here("plots", "mse_4_0_rct.png"), width = 12, 
    height = 7, units = "in", res = 600, pointsize = 4)
arrangeGrob(
  textGrob("N = 2,500", 
           gp = gpar(fontsize = 9, 
                     fontface = "italic")),
  textGrob("N = 500", 
           gp = gpar(fontsize = 9, 
                     fontface = "italic")), 
  zeroGrob(),
  plts$mse_binary_f2r_4_0 + theme(legend.position = "none"), 
  plts$mse_binary_fr_4_0 + theme(legend.position = "none"), 
  textGrob("Binary", 
           rot = -90, 
           gp = gpar(fontsize = 9, 
                     fontface = "italic")),
  zeroGrob(), 
  plts$mse_ordinal_fr_0_100 + theme(legend.position = "none"),
  textGrob("Ordinal", 
           rot = -90, 
           gp = gpar(fontsize = 9, 
                     fontface = "italic")),
  nrow = 3, 
  heights = c(0.1, 1, 1), 
  widths = c(1, 1, 0.1)
) %>% 
  grid.arrange()
grid.draw(leg)
dev.off()
