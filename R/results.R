read_results <- function(context, regex) {
  files <- find_files(context, regex)
  out <- purrr::map(files, function(file) {
    x <- readRDS(file)
    if (inherits(x, "try-error")) {
      return(NULL)
    }
    if (length(x$param) > 1) {
      x$tmle_mse <- mse(x$tmle, x$truth)
      x$param_mse <- mse(x$param, x$truth)
      x$param <- mean(x$param, na.rm = TRUE)
      x$tmle <- mean(x$tmle, na.rm = TRUE)
    }
    data.table(truth = x$truth, pos = x$pos, 
               vn1 = x$vn1, vn0 = x$vn0, vn_cate = x$vn_cate,
               param = x$param, tmle = x$tmle, 
               tmle_mse = x$tmle_mse, param_mse = x$param_mse)
  })
  setDT(out)
  out[, `:=`(tmle_bias = tmle - truth, 
             param_bias = param - truth)][]
}

mse <- function(x_i, x_bar) {
  mean((x_i - x_bar)^2)
}

find_files <- function(context, regex) {
  here::here("data", "res", context, grep(regex, list.files(here::here("data", "res", context)), value = TRUE))
}

bias_cdf <- function(data, limits = NULL, file = NULL) {
  out <- 
    ggplot(data) +
    geom_line(aes(x = abs(tmle_bias), y = 1 - ..y.., color = "blue"), stat = 'ecdf', 
              alpha = 0.65) +
    geom_line(aes(x = abs(param_bias), y = 1 - ..y.., color = "red"), stat = 'ecdf', 
              alpha = 0.65) + 

    labs(x = NULL, 
         y = NULL, 
         color = NULL) + 
    scale_x_continuous(expand = c(0.025, 0), limits = limits) + 
    scale_y_continuous(expand = c(0.01, 0.01)) + 
    theme_classic(base_family = "Helvetica")
    #theme(legend.box.background = element_rect(color = "black")) 
  
  if (!is.null(data$tmlep_bias)) {
    out <- out + 
      geom_line(aes(x = abs(tmlep_bias), y = 1 - ..y.., color = "darkgreen"), 
                stat = 'ecdf', alpha = 0.65) + 
      scale_color_identity(breaks = c("blue", "red", "darkgreen"),
                           labels = c("TMLE+DA", "G-comp.", "TMLE+GLM"),
                           guide = "legend")
  } else {
    out <- out + 
      scale_color_identity(breaks = c("blue", "red"),
                           labels = c("TMLE", "G-comp."),
                           guide = "legend")
  }
  
  if (!is.null(file)) {
    ggsave(here::here("plots", paste0("bias_", file, ".png")), 
           plot = out,
           dpi = 600, 
           width = 6, 
           height = 3)
    saveRDS(out, here::here("plots", paste0("bias_", file, ".rds")))
  }
  out
}

mse_cdf <- function(data, limits = NULL, file = NULL) {
  out <- 
    ggplot(data) +
    geom_line(aes(x = abs(tmle_mse), y = 1 - ..y.., color = "blue"), stat = 'ecdf', 
              alpha = 0.65) +
    geom_line(aes(x = abs(param_mse), y = 1 - ..y.., color = "red"), stat = 'ecdf', 
              alpha = 0.65) + 
    
    labs(x = NULL, 
         y = NULL, 
         color = NULL) + 
    scale_x_continuous(expand = c(0.025, 0), limits = limits) + 
    scale_y_continuous(expand = c(0.01, 0.01)) + 
    theme_classic(base_family = "Helvetica")
  #theme(legend.box.background = element_rect(color = "black")) 
  
  if (!is.null(data$tmlep_mse)) {
    out <- out + 
      geom_line(aes(x = abs(tmlep_mse), y = 1 - ..y.., color = "darkgreen"), 
                stat = 'ecdf', alpha = 0.65) + 
      scale_color_identity(breaks = c("blue", "red", "darkgreen"),
                           labels = c("TMLE+DA", "G-comp.", "TMLE+GLM"),
                           guide = "legend")
  } else {
    out <- out + 
      scale_color_identity(breaks = c("blue", "red"),
                           labels = c("TMLE", "G-comp."),
                           guide = "legend")
  }
  
  if (!is.null(file)) {
    ggsave(here::here("plots", paste0("mse_", file, ".png")), 
           plot = out,
           dpi = 600, 
           width = 6, 
           height = 3)
    saveRDS(out, here::here("plots", paste0("mse_", file, ".rds")))
  }
  out
}

g_legend <- function(ggplot) {
  tmp <- ggplot_gtable(ggplot_build(ggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
