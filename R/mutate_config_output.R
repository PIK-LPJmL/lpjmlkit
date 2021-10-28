# Function to rewrite parts in the output and restart section of precompiled
#   and read (as list) lpjml.js > config.json
#   output format (raw, clm cdf), output selection, output path, restart_path
mutate_config_output <- function(x,
                                 params,
                                 output_path,
                                 output_format,
                                 output_list,
                                 dir_create = FALSE) {
  # create vector to fill with outputs to delete (non matches with output_list)
  to_remove <- c()

  for (x_id in seq_len(length(x[["output"]]))) {

    # if output_list is defined append output index to to_remove vector
    if (!is.null(output_list)) {
      if (!(x[["output"]][[x_id]]$id %in% output_list)) {
        to_remove <- append(to_remove, x_id)
        next
      }
    }

    # replace output format in x if defined (e.g. raw, clm, cdf)
    if (x[["output"]][[x_id]]$file$fmt != "txt" && output_format != "raw") {
      x[["output"]][[x_id]]$file$fmt <- output_format
    }

    # concatenate output path and folder and replace in x
    opath <- paste(output_path, "output", params[["sim_name"]], "", sep = "/")
    x[["output"]][[x_id]]$file$name <- gsub("output/",
                                            opath,
                                            x[["output"]][[x_id]]$file$name)
    if (dir_create) dir.create(opath, recursive = TRUE, showWarnings = FALSE)
  }

  # remove non matching outputs
  if (length(to_remove) > 0) {
    x[["output"]][to_remove] <- NULL
  }

  # replace restart paths if write restart is set
  rpath <- paste(output_path, "restart", params[["sim_name"]], "", sep = "/")
  if (dir_create) dir.create(rpath, recursive = TRUE, showWarnings = FALSE)
  if (!is.null(x[["checkpoint_filename"]])) {
    x[["checkpoint_filename"]] <- gsub("restart/",
                                       rpath,
                                       x[["checkpoint_filename"]])
  }
  if (!is.null(x[["restart_filename"]]) && !is.null(params[["dependency"]])) {
    # if dependency is defined start from restart file of dependency sim_name
    x[["restart_filename"]] <- gsub("restart/",
                                          ifelse(is.na(params[["dependency"]]),
                                            rpath,
                                            paste(output_path,
                                                  "restart",
                                                  params[["dependency"]],
                                                  "",
                                                  sep = "/")
                                          ),
                                          x[["restart_filename"]])
  }
  x[["write_restart_filename"]] <- gsub("restart/",
                                        rpath,
                                        x[["write_restart_filename"]])

  return(x)
}
