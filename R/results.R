respath <- here::here("data", "res")

read_finite <- function(file) {
  data <- readRDS(file)
  params <- names(data)
  tmle_vals <- unlist(data[params[grepl("^tmle", params)]])
  if (!is.null(tmle_vals)) {
    tmle_vals <- tmle_vals[tmle_vals != Inf & tmle_vals != -Inf]
    param_vals <- unlist(data[params[grepl("^param", params)]])
    data[["tmle"]] <- mean(tmle_vals)
    data[["tmle_mse"]] <- mse(tmle_vals, data$truth)
    data[["param"]] <- mean(param_vals)
    data[["param_mse"]] <- mse(param_vals, data$truth)
    data[params[grepl("*[[:digit:]]+", params)]] <- NULL
    return(data)
  }
}

mse <- function(x_i, x_bar) {
  mean((x_i - x_bar)^2)
}

read_results <- function(folder, files, finite = FALSE) {
  if (finite) {
    out <- map_dfr(file.path(respath, folder, files), ~ as.data.frame(read_finite(.x)))
  } else {
    out <- map_dfr(file.path(respath, folder, files), ~ as.data.frame(readRDS(.x)))
  }
  setDT(out)
  out[, `:=`(tmle_bias = tmle - truth, 
             param_bias = param - truth)][]
}

find_files <- function(files, regex) {
  files[grepl(regex, files)]
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
           width = 4, 
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
           width = 4, 
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
