setwd("doesNPMatter")

zip_dgps <- function(dir) {
  files <- list.files(file.path("data", "dgps", dir))
  zip(file.path("data", "dgps", paste0(dir, ".zip")), 
      file.path("data", "dgps", dir, files))
}

dirs <- list.dirs("data/dgps/", full.names = FALSE, recursive = FALSE)

purrr::walk(dirs, zip_dgps)
