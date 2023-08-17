suppressPackageStartupMessages({
  library(tidyverse)
  library(glue)
  library(kableExtra)
})

source("R/utils.R")

agg_covr_main <- function(dgp) {
  res1 <- readRDS(glue("data/sims/coverage_{dgp}.rds"))
  res2 <- readRDS(glue("data/sims/coverage_cvtmle_{dgp}.rds"))
  res3 <- readRDS(glue("data/sims/coverage_tci_{dgp}.rds"))
  
  left_join(res1, select(res2, id, n, cvtmle)) |> 
    left_join(select(res3, id, n, gcomp2 = gcomp)) |> 
    group_by(n) |> 
    summarise(across(cbps:gcomp2, list(median = ~ median(.x, na.rm = TRUE), 
                                       lower = ~ quantile(.x, 0.25, na.rm = TRUE), 
                                       upper = ~ quantile(.x, 0.75, na.rm = TRUE)))) |> 
    mutate(across(cbps_median:gcomp2_upper, sprintf, fmt = '%.2f')) |>
    mutate(cbps = paste0(cbps_median, " (", cbps_lower, ", ", cbps_upper, ")"),
           gcomp1 = paste0(gcomp_median, " (", gcomp_lower, ", ", gcomp_upper, ")"),
           gcomp2 = paste0(gcomp2_median, " (", gcomp2_lower, ", ", gcomp2_upper, ")"),
           bart = paste0(bart_median, " (", bart_lower, ", ", bart_upper, ")"),
           tmle = paste0(tmle_median, " (", tmle_lower, ", ", tmle_upper, ")"), 
           cvtmle = paste0(cvtmle_median, " (", cvtmle_lower, ", ", cvtmle_upper, ")")) |>
    select(n, cbps, gcomp1, gcomp2, bart, tmle, cvtmle)
}

dgps <- levels(interaction("DGP_5_1", c("1", "2", "3"), c(FALSE, TRUE), sep = "_"))
dgps <- dgps[order(dgps)]

coverage <- map_dfr(dgps, agg_covr_main)

coverage <- mutate(coverage, 
                   Class = rep(c("No int., no HTE", 
                                 "No int., HTE", 
                                 "2-way int., no HTE", 
                                 "2-way int., HTE", 
                                 "3-way int., no HTE", 
                                 "3-way int., HTE"), each = 3)) |> 
  select(Class, everything())

kbl(coverage, "latex", booktabs = TRUE, linesep = "", 
    align = c("lccccc"), 
    col.names = c("", "n", "IPW", "G comp.", "G comp w/TCI", "BART", "TMLE", "CV-TMLE")) |> 
  clipr::write_clip()

# Supplementary -----------------------------------------------------------

agg_covr_supp <- function(dgp) {
  res1 <- readRDS(glue("data/sims/coverage_{dgp}.rds"))
  
  group_by(res1, n) |> 
    summarise(across(cbps:tmle, list(median = median, 
                                     lower = ~ quantile(.x, 0.25), 
                                     upper = ~ quantile(.x, 0.75)))) |> 
    mutate(across(cbps_median:tmle_upper, sprintf, fmt = '%.2f')) |>
    mutate(cbps = paste0(cbps_median, " (", cbps_lower, ", ", cbps_upper, ")"),
           gcomp = paste0(gcomp_median, " (", gcomp_lower, ", ", gcomp_upper, ")"),
           bart = paste0(bart_median, " (", bart_lower, ", ", bart_upper, ")"),
           tmle = paste0(tmle_median, " (", tmle_lower, ", ", tmle_upper, ")")) |>
    select(n, cbps, gcomp, bart, tmle)
}

coverage <- map_dfr(dgps, agg_covr_supp)

coverage <- mutate(coverage, 
                   Class = rep(c("No int., no HTE", 
                             "No int., HTE", 
                             "2-way int., no HTE", 
                             "2-way int., HTE", 
                             "3-way int., no HTE", 
                             "3-way int., HTE"), each = 3)) |> 
  select(Class, everything())

kbl(coverage, "latex", booktabs = TRUE, linesep = "", 
    align = c("llcccc"), 
    col.names = c("", "N", "IPW", "G comp.", "BART", "TMLE")) |> 
  clipr::write_clip()
