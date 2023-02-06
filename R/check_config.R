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
#'   (equal to `LPJROOT` environment variable).
#'
#' @param output_path Character string providing path where an output, a restart
#'   and a configuration folder are created. If ǸULL`, `model_path` is used.
#'
#' @param return_output Parameter affecting the output. If `FALSE` print
#'   stdout/stderr message. If `TRUE`, return the result of the check.
#'   Defaults to `FALSE`.
#'
#' @return NULL.
#'
#' @examples
#' \dontrun{
#' library(tibble)
#' library(lpjmlkit)
#'
#' model_path <- "./LPJmL_internal"
#' output_path <-"./my_runs"
#'
#'
#' # Basic usage
#' my_params <- tibble(
#'   sim_name = c("scen1", "scen2"),
#'   random_seed = c(12, 404),
#'   pftpar.1.name = c("first_tree", NA),
#'   param.k_temp = c(NA, 0.03),
#'   new_phenology = c(TRUE, FALSE)
#' )
#'
#' config_details <- write_config(
#'   params = my_params,
#'   model_path = model_path,
#'   output_path = output_path
#' )
#'
#' check_config(x = config_details,
#'   model_path = model_path,
#'   output_path = output_path,
#'   return_output = FALSE
#' )
#' }
#' @export
check_config <- function(x,
                         model_path,
                         output_path = NULL,
                         return_output = FALSE) {

  if (is.null(output_path)) output_path <- model_path

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
      files <- paste0(output_path,
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
                      output_path,
                      "/configurations/",
                      config_files)
  }

  # Call sh command via processx to kill any subprocesses after
  #   background: process limit on the cluster
  check <- processx::run(command = "sh",
                         args = c("-c", inner_command),
                         error_on_status = FALSE,
                         cleanup_tree = TRUE)

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
