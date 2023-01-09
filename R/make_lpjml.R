#' Make all (compile, link, etc.) LPJmL
#'
#' Compiles LPJmL and create executables by internally executing make all.
#'
#' @param model_path character string providing the path to LPJmL
#' (equal to LPJROOT)
#'
#' @param make_fast logical - if TRUE `make -j16 all` is executed.
#' Defaults to TRUE
#'
#' @param make_clean logical - if TRUE execute make clean first.
#' Defaults to FALSE
#'
#' @param throw_error logical - if `FALSE` does not throw an error if sub
#' process has non-zero exit status, hence if compilation fails in first
#' attempt. Defaults to TRUE
#'
#'@param debug logical - if `TRUE`LPJmL is compiled with flag `-debug`
#'
#' @return a list with process status, see \link[processx]{run}
#'
#' @export
make_lpjml <- function(model_path = ".",
                      make_fast = TRUE,
                      make_clean = FALSE,
                      throw_error = TRUE,
                      debug = FALSE) {
  # take precompiled config.json as proxy if LPJmL was already configured
  if (file.exists(paste0(model_path, "/bin/lpjml"))) {
    init <- processx::run(command = "sh",
                          args = c("-c",
                                   paste0(ifelse(make_clean,
                                                 "make clean;",
                                                 ""),
                                          "make ",
                                          ifelse(make_fast,
                                                 "-j16",
                                                 ""),
                                          " all;")),
                          cleanup_tree = TRUE,
                          error_on_status = throw_error,
                          wd = model_path,
                          echo = TRUE)
  } else {
    init <- processx::run(command = "sh",
                          args = c("-c",
                                   paste0("./configure.sh;",
                                          ifelse(debug, "-debug", ""),
                                          "make ",
                                          ifelse(make_fast, "-j16", ""),
                                          " all;")),
                          cleanup_tree = TRUE,
                          error_on_status = throw_error,
                          wd = model_path,
                          echo = TRUE)
  }
  return(init)
}
