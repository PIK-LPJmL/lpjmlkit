#' Check LPJmL config JSON files using lpjcheck
#'
#' Check if created LPJmL config JSON files (\link[lpjmlKit]{write_config}) are
#' valid.
#'
#' @param x job_details object returned by \link[lpjmlKit]{write_config} or
#' character vector providing the config file names
#' (hint: returns x as a list entry)
#'
#' @param model_path character string providing the path to LPJmL
#' (equal to LPJROOT)
#'
#' @param output_path character string - if output_path differs from
#' model_path - path where an output, a restart and a configuration
#' folder are created
#'
#' @param return_output boolean - if FALSE print stdout/stderr else return.
#' Defaults to FALSE
#'
#' @return see `pretty_print`
#'
#' @export
check_config <- function(x,
                         model_path,
                         output_path = NULL,
                         return_output = FALSE) {

  if (is.null(output_path)) output_path <- model_path
  config_files <- paste0("config_", x$sim_name, ".json")
  if (length(config_files) > 1) {
      files <- paste0(output_path,
                      "/configurations/",
                      config_files,
                      collapse = " ")
      # for loop in bash -> background: process limit on the cluster
      inner_command <- paste0("files=( ",
                              paste("\"", files, "\"", collapse = " "),
                              " ) ;",
                              "for ff in ${files[@]};",
                              "do echo '\n'$ff: >&2;",
                              "echo '\n'$ff: ;",
                              model_path,
                              "/bin/lpjcheck",
                              " $ff; done;")

  } else {
    inner_command <- paste0(model_path,
                      "/bin/lpjcheck ",
                      output_path,
                      "/configurations/",
                      config_files)
  }
  # call sh command via processx to kill any subprocesses after
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