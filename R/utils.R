suppressPackageStartupMessages(library(tidyverse))

read_zipped_dgp <- function(dir_name, id) {
  tar <- file.path("data/dgps", dir_name, paste0(dir_name, ".zip"))
  file <- unzip(tar, list = TRUE)$Name[id]
  tryCatch({
    con = gzcon(unz(tar, file))
    readRDS(con)
  }, finally = {
    close(con)
  })
}

read_dgp <- function(dir_name, id) {
  to_read <- file.path("data/dgps", dir_name, paste0(id, ".rds"))
  readRDS(to_read)
}

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

pt_var <- function(dgp) {
  pt <- dgp$x_ret |> 
    group_by(across(starts_with("x"))) |> 
    mutate(px = sum(p)) |> 
    ungroup() |> 
    group_by(across(starts_with("x")), t) |> 
    mutate(ptandx = sum(p)) |> 
    ungroup() |> 
    group_by(across(starts_with("x"))) |> 
    mutate(pt = ptandx / px) |> 
    filter(t == 1 & y == 1)
  var(pt$pt)
}

read_zip <- function(tar) {
  files <- unzip(tar, list = TRUE)$Name
  p <- progressr::progressor(along = 1:length(files))
  purrr::map(files, function(file) {
    p()
    con <- unz(tar, file)
    read.csv(con)
  })
}

pt_to_mm <- function(pt) {
  pt / ggplot2::.pt
}

ecdf_plot <- function(data, data2, xlab, subtitle, xlim, seq = 0.05) {
  pts <- 0.75
  ts <- 7
  ggplot(data) +
    geom_line(aes(x = abs(cbps.psi), y = 1 - ..y.., color = "#271F30"), 
              stat = 'ecdf', alpha = 0.65, size = pt_to_mm(0.9)) +
    geom_line(aes(x = abs(gcomp.psi), y = 1 - ..y.., color = "#548C64"), 
              stat = 'ecdf', alpha = 0.65, size = pt_to_mm(0.9)) + 
    geom_line(aes(x = abs(bart.psi), y = 1 - ..y.., color = "#D5A021"), 
              stat = 'ecdf', alpha = 0.65, size = pt_to_mm(0.9)) + 
    geom_line(aes(x = abs(tmle.psi), y = 1 - ..y.., color = "#C03221"), 
              stat = 'ecdf', alpha = 0.65, size = pt_to_mm(0.9)) + 
    {
      if (nrow(data2) > 0) {
        geom_line(data = data2, aes(x = abs(cvtmle.psi), y = 1 - ..y.., color = "#B47EB3"), 
                  stat = 'ecdf', alpha = 0.65, size = pt_to_mm(0.9))
      }
    } + 
    scale_color_identity(
      breaks = c("#271F30", "#548C64", "#D5A021", "#C03221", "#B47EB3"),
      labels = c("IPTW-CBPS", "G-comp.", "BART", "TMLE", "CV-TMLE"),
      guide = "legend"
    ) + 
    labs(
      x = xlab, 
      y = "P[X>x]", 
      colour = "Estimator", 
      subtitle = subtitle
    ) + 
    theme_classic() + 
    coord_cartesian(xlim = xlim) +
    scale_x_continuous(expand = c(0.01, 0), 
                       breaks = seq(0, xlim[2], by = seq)) + 
    scale_y_continuous(expand = c(0.01, 0)) + 
    theme(text = element_text(size = ts),
          legend.position = {
            if (subtitle == "1000" || subtitle == "severe") c(0.76, 0.9)
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
