suppressPackageStartupMessages(library(tidyverse))

source("R/utils.R")

dgp <- "DGP_5_1_3_TRUE"

DGPs <- map_dfr(1:500, function(id) {
  meta <- try(read_dgp(dgp, id))
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

res <-
  read_zip(glue::glue("data/sims/sim_tci_{dgp}.zip")) |> 
  bind_rows() |> 
  left_join(DGPs)

saveRDS(res, glue::glue("data/sims/res_tci_{dgp}.rds"))

covered <- \(l, u, truth) between(truth, l, u)

coverage <- group_by(res, id, n) |> 
  summarise(gcomp = mean(pmap_lgl(list(gcomp.conf.low, gcomp.conf.high, truth), covered)))

coverage <- select(res, id, eta, rho, max_ipw, var_pt) |> 
  distinct() |> 
  left_join(coverage)

saveRDS(coverage, glue::glue("data/sims/coverage_tci_{dgp}.rds"))
