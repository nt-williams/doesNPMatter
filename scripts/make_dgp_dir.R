purrr::walk(c("sims", "dgps"), function(dir) {
  purrr::pmap(expand.grid(
    n_bin = c(3, 5),
    n_num = c(0, 1), 
    inter_order = 1:3, 
    hte = c(TRUE, FALSE)
  ), function(n_bin, n_num, inter_order, hte) {
    dir.create(glue::glue("data/{dir}/DGP_{n_bin}_{n_num}_{inter_order}_{hte}"))
  })
})
