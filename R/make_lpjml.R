#' Make all (compile, link, etc.) LPJmL
#'
#' Compiles LPJmL and create executables by internally executing make all.
#'
#' @param model_path character string providing the path to LPJmL
#' (equal to LPJROOT)
#'
#' @param make_fast logical - if TRUE `make -j8 all` is executed.
#' Defaults to TRUE
#'
#' @param make_clean logical - if TRUE execute make clean first.
#' Defaults to FALSE
#'
#' @return a list with process status, see \link[processx]{run}
#'
#' @export
make_lpjml <- function(model_path = ".", make_fast = TRUE, make_clean = FALSE) {
  # take precompiled config.json as proxy if LPJmL was already configured
  if (file.exists(paste0(model_path, "/bin/lpjml"))) {
    init <- processx::run(command = "sh",
                          args = c("-c",
                                   paste0(ifelse(make_clean,
                                                 "make clean;",
                                                 ""),
                                          "make ",
                                          ifelse(make_fast,
                                                 "-j8",
                                                 ""),
                                          " all;")),
                          cleanup_tree = TRUE,
                          wd = model_path,
                          echo = TRUE)
  } else {
    init <- processx::run(command = "sh",
                          args = c("-c",
                                   paste0("./configure.sh;",
                                          "make ",
                                          ifelse(make_fast, "-j8", "")
                                          , " all;")),
                          cleanup_tree = TRUE,
                          wd = model_path,
                          echo = TRUE)
  }
  return(init)
}
