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
