#' Submit LPJmL
#'
#' LPJmL runs are submitted to Slurm using config*.json files written by
#' \link[lpjmlKit]{write_config}. `write_config` returns a tibble that can be
#' used as an input (see `run_details`). It serves the details to submit sinlge
#' or multiple (dependent/subsequent) model runs.
#'
#' @param run_details tibble with at least "sim_name" and "config_file"
#' defined as columns. Runs as rows. Optional pseudo parameters "order" and
#' "dependency" used for subsequent runs (see details). Hint:
#' \link[lpjmlKit]{write_config} returns a tibble in the required format
#'
#' @param model_path character string providing the path to LPJmL
#' (equal to LPJROOT)
#'
#' @param output_path character string - if output_path differs from
#' model_path - path where an output, a restart and a configuration
#' folder are created
#'
#' @param group character string, provide a group for which the job is
#' submitted. Default is "lpjml"
#'
#' @param sclass character string, define the job classification, for more
#' information have a look [here](https://www.pik-potsdam.de/en/institute/about/it-services/hpc/user-guides/slurm#section-5).
#' Defaults to "short".
#'
#' @param ntasks integer, define the number of tasks/threads, for more
#' information have a look [here](https://www.pik-potsdam.de/en/institute/about/it-services/hpc/user-guides/slurm#section-18).
#'
#' @param wtime character string, defining the time limit which can be an
#' advantage to get faster to the top of the (s)queue. For more information
#' have a look [here](https://www.pik-potsdam.de/en/institute/about/it-services/hpc/user-guides/slurm#section-18).
#'
#' @param blocking integer, cores to be blocked. For more information
#' have a look [here](https://www.pik-potsdam.de/en/institute/about/it-services/hpc/user-guides/slurm#section-18).
#'
#' @return see `run_details`, extended with columns "job_id" and "status".
#'
#' @details
#'
#' Supply a \link[tibble]{tibble} (or data.frame) for `run_details` in the form
#' of (random example):
#'
#' | **sim_name**    | **config_file**   |
#' |:--------------- |:----------------- |
#' | scen1_spinup    | config_scen1.json |
#' | scen2_transient | config_scen2.json |
#'
#' To perform subsequent runs provide pseudo parameters order and dependency
#' as in the following example:
#'
#' | **sim_name**    | **config_file**   | **order** | **dependency** |
#' |:--------------- |:----------------- | ---------:|:-------------- |
#' | scen1_spinup    | config_scen1.json | 1         | NA             |
#' | scen2_transient | config_scen2.json | 2         | scen1 _spinup  |
#'
#'
#' @export
submit_lpjml <- function(run_details,
                         model_path,
                         output_path = NULL,
                         group = "lpjml",
                         sclass = "short",
                         ntasks = 256,
                         wtime = "",
                         blocking = "") {
  if (is.null(output_path)) output_path <- model_path

  submitter <- function(sim_name,
                        model_path,
                        output_path,
                        group,
                        sclass,
                        ntasks,
                        wtime,
                        blocking,
                        dependency) {
    config_file <- paste0("config_",
                          sim_name,
                          ".json")

    timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
    stdout <- paste0(output_path,
                    "/output/",
                    sim_name,
                    "/",
                    "outfile_",
                    timestamp,
                    ".out")
    stderr <- paste0(output_path,
                    "/output/",
                    sim_name,
                    "/",
                    "errfile_",
                    timestamp,
                    ".err")
    inner_command <-  paste0(model_path, "/bin/lpjsubmit",
                             " -nocheck",
                             " -class ", sclass,
                             " -group ", group,
                             ifelse(wtime != "",
                                    paste0(" -wtime ", wtime),
                                    ""),
                             ifelse(blocking != "",
                                    paste0(" -blocking ", blocking),
                                    ""),
                             ifelse(!is.na(dependency),
                                    paste0(" -dependency ", dependency),
                                    ""),
                             " -o ", stdout,
                             " -e ", stderr,
                             " ",
                             ntasks,
                             " ",
                             output_path,
                             "/configurations/",
                             config_file)
    submit_status <- processx::run(command = "sh",
                                   args = c("-c", inner_command),
                                   cleanup_tree = TRUE)
    return(submit_status)
  }

  run_details$job_id <- rep(NA, nrow(run_details))
  run_details$status <- rep("submitted", nrow(run_details))

  if ("order" %in% colnames(run_details)) {
    for (order in unique(sort(run_details$order))) {
      sim_names <- run_details$sim_name[
        which(run_details$order == order)
      ]
      for (sim_name in sim_names) {
        dep_sim_name <- run_details$dependency[
          which(run_details$sim_name == sim_name)
        ]
        if (!is.na(dep_sim_name)) {
          dependency <- run_details$job_id[
            which(run_details$sim_name == dep_sim_name)
          ]
        } else {
          dependency <- NA
        }
        job <- submitter(sim_name,
                         model_path,
                         output_path,
                         group,
                         sclass,
                         ntasks,
                         wtime,
                         blocking,
                         dependency)
        if (job$status == 0) {
          run_details$job_id[
            which(run_details$sim_name == sim_name)
          ] <- strsplit(
            strsplit(job$stdout, "Submitted batch job ")[[1]][2], "\n"
          )[[1]][1]
        } else {
          run_details$status <- "failed"
        }
      }
    }
  } else {
    for (sim_name in run_details$sim_name) {
      job <- submitter(sim_name,
                       model_path,
                       output_path,
                       group,
                       sclass,
                       ntasks,
                       wtime,
                       blocking,
                       dependency = NA)
      if (job$status == 0) {
        run_details$job_id[
          which(run_details$sim_name == sim_name)
        ] <- strsplit(
          strsplit(job$stdout, "Submitted batch job ")[[1]][2], "\n"
        )[[1]][1]
      } else {
        run_details$status <- "failed"
      }
    }
  }
  cat("\nDone.")
  return(run_details)
}