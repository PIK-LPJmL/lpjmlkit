#' Compile LPJmL model
#'
#' Compiles the LPJmL source code and creates an executable by internally
#' executing make all.
#'
#' @param model_path Character string providing the path to LPJmL
#' (equal to LPJROOT)
#'
#' @param make_fast Logical - if TRUE `make -j16 all` is executed which uses 16
#' processors for compilation in parallel mode. Defaults to TRUE
#'
#' @param make_clean Logical - if TRUE execute make clean first to remove
#' previous installation. Defaults to FALSE
#'
#' @param throw_error Logical - if `FALSE` does not throw an error if sub
#' process has non-zero exit status, hence if compilation fails in first
#' attempt. Defaults to TRUE
#'
#'@param debug Logical - if `TRUE`LPJmL is compiled with flag `-debug`
#'
#' @return A list with process status, see \link[processx]{run}
#'
#' @examples
#' \dontrun{
#' model_path <- "./LPJmL_internal"
#' make_lpjml(model_path = model_path)
#' }
#'
#' @export
make_lpjml <- function(model_path = ".",
                      make_fast = TRUE,
                      make_clean = FALSE,
                      throw_error = TRUE,
                      debug = FALSE) {

  # take precompiled config.json as proxy if LPJmL was already configured
  if (file.exists(paste0(model_path, "/bin/lpjml"))) {  # nolint:absolute_path_linter.
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
                                          ifelse(debug, "-debug", ""), # nolint:undesirable_function_linter.
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
