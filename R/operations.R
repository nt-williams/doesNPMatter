alt_01 <- function(meta, x, cats = 2) {
  ((meta - 1) %/% x) %% cats
}

mod_op <- function(meta, x){
  (meta - 1) %% x + 1
}
