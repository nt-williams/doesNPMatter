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
