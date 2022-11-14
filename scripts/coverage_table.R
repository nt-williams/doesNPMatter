suppressPackageStartupMessages({
  library(tidyverse)
  library(glue)
  library(kableExtra)
})

source("R/utils.R")

agg_covr <- function(dgp) {
  res1 <- readRDS(glue("data/sims/coverage_{dgp}.rds"))
  res2 <- readRDS(glue("data/sims/coverage_cvtmle_{dgp}.rds"))
  
  filter(res1, n == 1000) |>
    left_join(select(res2, id, cvtmle)) |> 
    summarise(across(cbps:cvtmle, median, na.rm = TRUE))
}


dgps <- levels(interaction("DGP_5_1", c("1", "2", "3"), c(FALSE, TRUE), sep = "_"))
dgps <- dgps[order(dgps)]

coverage <- map_dfr(dgps, agg_covr)

coverage <- mutate(coverage, 
                   Class = c("No interactions, no HTE", 
                             "No interactions, HTE", 
                             "Up to 2-way interactions, no HTE", 
                             "Up to 2-way interactions, HTE", 
                             "Up to 3-way interactions, no HTE", 
                             "Up to 3-way interactions, HTE")) |> 
  select(Class, everything())

kbl(coverage, "latex", booktabs = TRUE, linesep = "", 
    align = c("lccccc"), 
    col.names = c("", "IPW", "G comp.", "BART", "TMLE", "CV-TMLE")) |> 
  clipr::write_clip()
