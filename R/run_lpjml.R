#' Run LPJmL
#'
#' LPJmL is run using a config_*.json written by \link[lpjmlKit]{write_config}.
#' `runLPJmL` can only perform single or subsequent model runs. For multiple
#' runs (via slurm) you may consider to use \link[lpjmlKit]{submit_lpjml} or
#' loop over an R-script containing `run_lpjml`.
#'
#' @param sim_name_s character vector providing sim_name or sim_names if the
#' runs are dependent from each other (e.g. spin-up and transient run)
#'
#' @param model_path character string providing the path to LPJmL
#' (equal to LPJROOT)
#'
#' @param output_path character string - if output_path differs from
#' model_path - path where an output, a restart and a configuration
#' folder are created
#'
#' @return "Done."
#'
#' @export
run_lpjml <- function(sim_name_s,
                      model_path,
                      output_path = NULL) {

  if (is.null(output_path)) output_path <- model_path

  runner <- function(sim_name,
                     model_path,
                     output_path) {
    config_file <- paste0("config_",
                          sim_name,
                          ".json")
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
    inner_command <-  paste0("srun --propagate ",
                             model_path,
                             "/bin/lpjml ",
                             output_path,
                             "/configurations/",
                             config_file)
    cat("\nRunning LPJmL for config: ", config_file, "...\n")
    processx::run(command = "sh",
                  args = c("-c", inner_command),
                  stdout = paste0(output_path,
                                  "/output/",
                                  sim_name,
                                  "/",
                                  "outfile_",
                                  timestamp,
                                  ".out"),
                  stderr = paste0(output_path,
                                  "/output/",
                                  sim_name,
                                  "/",
                                  "errfile_",
                                  timestamp,
                                  ".err"),
                  cleanup_tree = TRUE)
  }
  for (sim_name in sim_name_s) {
    runner(sim_name, model_path, output_path)
  }
  cat("\nDone.")
}
