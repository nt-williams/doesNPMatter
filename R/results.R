respath <- here::here("data", "res")

read_finite <- function(file) {
  data <- readRDS(file)
  params <- names(data)
  tmle_vals <- unlist(data[params[grepl("^tmle", params)]])
  if (!is.null(tmle_vals)) {
    tmle_vals <- tmle_vals[tmle_vals != Inf & tmle_vals != -Inf]
    data[["tmle"]] <- mean(tmle_vals)
    data[["param"]] <- mean(unlist(data[params[grepl("^param", params)]]))
    data[params[grepl("*[[:digit:]]+", params)]] <- NULL
    return(data)
  }
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

result_cdf <- function(data, limits = NULL) {
  ggplot(data) +
    geom_line(aes(x = abs(tmle_bias), y = 1 - ..y..), stat = 'ecdf', color = "blue") +
    geom_line(aes(x = abs(param_bias ), y = 1 - ..y..), stat = 'ecdf', color = "red") + 
    labs(x = expression(abs(Bias)), 
         y = expression(P(abs(Bias) > x))) + 
    scale_x_continuous(expand = c(0.01, 0), limits = limits) + 
    scale_y_continuous(expand = c(0.01, 0.01)) + 
    theme_classic()
}

find_files <- function(files, regex) {
  files[grepl(regex, files)]
}
