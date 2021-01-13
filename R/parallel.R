#' Run simulations in parallel on a computing cluster.
#'
#' @param task A list containing meta information for a simulation task.
#'
#' @return Saves results files.
#' 
#' @author Nicholas Williams and Iván Díaz
partition <- function(task) {
  index <- (1:nrow(task$tasks))[(0:(nrow(task$tasks) - 1)) %/% (nrow(task$tasks) / task$machines) + 1 == task$id]
  out <- list()
  globals <- append(ls(), c("index", "i"))
  
  for (i in 1:length(index)) {
    row <- index[i]
    cat("doing task ", row, " of ", nrow(task$tasks), "\n", file = task$progfile, append = TRUE)
    
    out[[i]] <- future({
      try(
        bias(task$context, task$tasks$seeds[row], task$n, 
             task$reps, task$size, task$tasks$rho[row], 
             task$tasks$binary_cnf[i], task$tasks$cont_cnf[row], 
             mu = task$mu, sigma = task$sigma, 
             randomized = task$tasks$randomized[row])
      )
    }, globals = globals, packages = task$pkgs, seed = TRUE)
    
    if (inherits(value(out[[i]]), "try-error")) {
      cat("error in task ", row, "\n", file = task$errfile, append = TRUE)
    }
    saveRDS(append(list(id = row), append(unlist(value(out[[i]])), task$tasks[row, ])), 
            file.path(task$respath, paste0(task$ident, row, "_", task$tasks$randomized[row], ".rds")))
  }
}
