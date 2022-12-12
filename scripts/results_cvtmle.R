suppressPackageStartupMessages(library(tidyverse))
source("R/utils.R")

dgp <- "DGP_5_1_3_TRUE"

DGPs <- map_dfr(1:500, function(id) {
  meta <- try(read_dgp(dgp, id), silent = TRUE)
  if (inherits(meta, "try-error")) return(NULL)
  data.frame(
    id = id,
    truth = meta$truth, 
    bias = meta$bias,
    eta = meta$eta, 
    rho = meta$rho, 
    max_ipw = calc_max_ipw(meta),
    var_pt = pt_var(meta)
  )
})

res <- map_dfr(c('100' = 100, '500' = 500, '1000' = 1000), function(n) {
  read_zip(glue::glue("data/sims/sim_cvtmle_{dgp}_{n}.zip")) |> 
    bind_rows() |> 
    left_join(DGPs)
}, .id = "n") |> 
  mutate(n = as.numeric(n))

saveRDS(res, glue::glue("data/sims/res_cvtmle_{dgp}.rds"))

covered <- \(l, u, truth) between(truth, l, u)

coverage <- group_by(res, n, id) |> 
  summarise(cvtmle = mean(pmap_lgl(list(cvtmle.conf.low, cvtmle.conf.high, truth), covered))) |> 
  ungroup()

coverage <- select(res, id, n, eta, rho, max_ipw) |> 
  distinct() |> 
  left_join(coverage)

saveRDS(coverage, glue::glue("data/sims/coverage_cvtmle_{dgp}.rds"))
