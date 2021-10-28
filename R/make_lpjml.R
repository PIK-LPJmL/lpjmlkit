#' Make all (compile, link, etc.) LPJmL
#'
#' Compiles LPJmL and create executables by internally executing make all.
#'
#' @param model_path character string providing the path to LPJmL
#' (equal to LPJROOT)
#'
#' @param make_fast boolean - if TRUE `make -j8 all` is executed.
#' Defaults to TRUE
#'
#' @param pretty_print boolean - if TRUE print stdout/stderr else return.
#' Defaults to TRUE
#'
#' @return see `pretty_print`
#'
#' @export
make_lpjml <- function(model_path = ".", make_fast=TRUE, pretty_print = TRUE) {
  # take precompiled config.json as proxy if LPJmL was already configured
  if (file.exists(paste0(model_path, "bin/lpjml"))) {
    init <- processx::run(command = "make",
                          args = c(ifelse(make_fast, "-j8", ""),
                                   "all"),
                          cleanup_tree = TRUE,
                          wd = model_path)
  } else {
    init <- processx::run(command = "sh",
                          args = c("-c",
                                   paste0("./configure.sh;",
                                          "make ",
                                          ifelse(make_fast, "-j8", "")
                                          , " all;")),
                          cleanup_tree = TRUE,
                          wd = model_path)
  }
  if (pretty_print) {
    if (init$status == 0) {
      return(cat(init$stdout))
    } else {
      return(cat(init$stderr))
    }
  } else {
    return(init)
  }
}
