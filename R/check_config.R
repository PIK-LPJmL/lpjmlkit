#' Check the validity of LPJmL config JSON files
#'
#' Check if created LPJmL config JSON files ([`write_config()`]) are
#' valid and are ready to be used for simulations using lpjcheck for multiple
#' files.
#'
#' @param x `job_details` object returned by [`write_config()`] or
#'   character vector providing the config file names
#'   (hint: returns `x` as a list entry).
#'
#' @param model_path Character string providing the path to LPJmL
#'   (equal to `LPJROOT` environment variable). Defaults to ".".
#'
#' @param sim_path Character string defining path where all simulation data are
#'   written, including output, restart and configuration files. If `NULL`,
#'   `model_path` is used. See also [write_config]
#'
#' @param return_output Parameter affecting the output. If `FALSE` print
#'   stdout/stderr message. If `TRUE`, return the result of the check.
#'   Defaults to `FALSE`.
#'
#' @param raise_error Logical. Whether to raise an error if sub-process has
#'   non-zero exit status. Defaults to `FALSE`.
#'
#' @param output_path Argument is deprecated as of version 1.0; use sim_path
#'   instead.
#'
#' @return NULL.
#'
#' @examples
#' \dontrun{
#' library(tibble)
#' library(lpjmlkit)
#'
#' model_path <- "./LPJmL_internal"
#' sim_path <-"./my_runs"
#'
#'
#' # Basic usage
#' my_params <- tibble(
#'   sim_name = c("scen1", "scen2"),
#'   random_seed = c(12, 404),
#'   `pftpar[[1]]$name` = c("first_tree", NA),
#'   `param$k_temp` = c(NA, 0.03),
#'   gsi_phenology = c(TRUE, FALSE)
#' )
#'
#' config_details <- write_config(
#'   x = my_params,
#'   model_path = model_path,
#'   sim_path = sim_path
#' )
#'
#' check_config(x = config_details,
#'   model_path = model_path,
#'   sim_path = sim_path,
#'   return_output = FALSE
#' )
#' }
#' @export
check_config <- function(x,
                         model_path = ".",
                         sim_path = NULL,
                         return_output = FALSE,
                         raise_error = FALSE,
                         output_path = NULL) {

  warn_runner_os("check_config")

  sim_path <- deprecate_arg(new_arg = sim_path,
                            deprec_arg = output_path,
                            version = "1.0.0")

  if (is.null(sim_path)) sim_path <- model_path

  # Check if x is character (vector). If so convert to tibble for the following.
  if (methods::is(x, "character")) {
    x <- tibble::tibble(sim_name = sapply( # nolint:undesirable_function_linter.
      x,
      function(x) {
        strsplit(
          strsplit(rev(strsplit(x, "/")[[1]])[1], "config_")[[1]][2],
          ".json"
        )[[1]]
      }
    ))
  }

  config_files <- paste0("config_", x$sim_name, ".json")

  if (length(config_files) > 1) {
      files <- paste0(sim_path,
                      "/configurations/",
                      config_files,
                      collapse = " ")

      # For loop in bash -> background: process limit on the cluster
      inner_command <- paste0("files=( ",
                              paste("\"", files, "\"", collapse = " "),
                              " ) ;",
                              "for ff in ${files[@]};",
                              "do echo '\n'$ff: >&2;",
                              "echo '\n'$ff: ;",
                              model_path,
                              "/bin/lpjcheck", # nolint:absolute_path_linter.
                              " $ff; done;")

  } else {
    inner_command <- paste0(model_path,
                      "/bin/lpjcheck ", # nolint:absolute_path_linter.
                      sim_path,
                      "/configurations/",
                      config_files)
  }

  # Call sh command via processx to kill any subprocesses after
  #   background: process limit on the cluster
  check <- processx::run(command = "bash",
                         args = c("-c", inner_command),
                         error_on_status = raise_error,
                         cleanup_tree = TRUE,
                         wd = sim_path)

  if (!return_output) {
    return(
      cat(check$stdout,
          "\n",
          ifelse(check$status == 1,
                 "\n>>>> ERRORS FOUND - PLEASE ADJUST YOUR CONFIG <<<<\n",
                 "\n>>>> Please check for warnings <<<<\n"),
          "\n",
          check$stderr)
    )

  } else {
    return(check)
  }
}
