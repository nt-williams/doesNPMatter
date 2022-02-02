filesstrings::file.move(
  list.files("data/dgps", pattern = ".zip$", full.names = TRUE),
  file.path("data/dgps", gsub(pattern = "\\.zip$", "", list.files("data/dgps", pattern = ".zip$")))
)
