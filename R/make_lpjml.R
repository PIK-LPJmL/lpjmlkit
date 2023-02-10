#' Compile LPJmL model
#'
#' Compiles the LPJmL source code and creates an executable by executing
#' "make all" on the operating system shell.
#'
#' @param model_path Character string providing the path to LPJmL
#' (equal to `LPJROOT` environment variable)
#'
#' @param nproc numeric. Number of N central processing units / threads to be
#'   used for compilation.
#'
#' @param make_clean Logical. If set to `TRUE`, calls "make clean" first to
#'   remove previous installation. Defaults to `FALSE`.
#'
#' @param throw_error Logical. Whether to throw an error if sub-process has
#'   non-zero exit status, hence if compilation fails. Defaults to `TRUE`.
#'
#' @param debug NULL or Logical. Whether to compile LPJmL with "-debug" flag.
#'  Defaults to `NULL`. If set to `FALSE` or `TRUE`, `make_clean` is set
#'  automatically and compilation configuration is reset with/without "-debug".
#'  Also the "configure.sh" file is rewritten.
#'
#' @return A list with process status, see \link[processx]{run}.
#'
#' @examples
#' \dontrun{
#' model_path <- "./LPJmL_internal"
#' make_lpjml(model_path = model_path)
#' }
#'
#' @export
make_lpjml <- function(model_path = ".",
                       nproc = NULL,
                       make_clean = FALSE,
                       throw_error = TRUE,
                       debug = NULL) {

  if (!is.null(debug)) { # nolint:undesirable_function_linter.
    make_clean <- TRUE
  }
  # Take precompiled config.json as proxy if LPJmL was already configured
  if (file.exists(paste0(model_path, "/Makefile.inc"))) {  # nolint:absolute_path_linter.
    init <- processx::run(command = "sh",
                          args = c("-c",
                                   paste0(ifelse(make_clean,
                                                 "make clean;",
                                                 ""),
                                          ifelse(!is.null(debug), # nolint:undesirable_function_linter.
                                            ifelse(debug, "./configure.sh -debug;", "./configure.sh;"), # nolint
                                            ""
                                          ),
                                          "make ",
                                          ifelse(!is.null(nproc),
                                                 paste0("-j", nproc),
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
                                          ifelse(!is.null(debug) && debug, "-debug", ""), # nolint
                                          ifelse(!is.null(nproc),
                                                 paste0("-j", nproc),
                                                 ""),
                                          " all;")),
                          cleanup_tree = TRUE,
                          error_on_status = throw_error,
                          wd = model_path,
                          echo = TRUE)
  }

  init
}
