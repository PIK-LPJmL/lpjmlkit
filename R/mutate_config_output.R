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
  match_outputvars <- c()
  # concatenate output path and create folder if set
  opath <- paste(output_path, "output", params[["sim_name"]], "", sep = "/")
  if (dir_create) dir.create(opath, recursive = TRUE, showWarnings = FALSE)

  for (x_id in seq_len(length(x[["output"]]))) {

    # if output_list is defined append output index to to_remove vector
    if (!is.null(output_list)) {
      tomatch <- match(x[["output"]][[x_id]]$id, output_list)
      if (is.na(tomatch)) {
        to_remove <- append(to_remove, x_id)
        next
      } else {
        match_outputvars <- append(match_outputvars, tomatch)
      }
    }

    # replace output format in x if defined (e.g. raw, clm, cdf)
    if (x[["output"]][[x_id]]$file$fmt != "txt" && output_format != "raw") {
      x[["output"]][[x_id]]$file$fmt <- output_format
    }

    # replace output path replace in x
    x[["output"]][[x_id]]$file$name <- gsub("output/",
                                            opath,
                                            x[["output"]][[x_id]]$file$name)
  }
  # check for elements in output list that are not defined in x[["output"]]
  if (!is.null(output_list) & length(match_outputvars) > 0) {
    # get list of outputvar names
    outputvar_names <- unlist(lapply(x[["outputvar"]], function(x)x$name))
    for (id_ov in seq_len(length(output_list))[-match_outputvars]) {
      # get elements in output list that are not defined in x[["output"]]
      if (output_list[id_ov] %in% outputvar_names) {
        # NOT SO GOOD approach to filter spinup configs as well as empty outputs
        #  !find a better way to replace this implicit approach!
        if (length(x[["output"]]) > 0) {
          if (x[["output"]][[1]]$file$fmt == "txt") {
            break
          }
        } else {
          break
        }
        # use first entry as template (therefor beforehand filtering)
        id_op <- length(x[["output"]]) + 1
        x[["output"]][[id_op]] <- x[["output"]][[1]]
        # get index

        # replace ID name
        x[["output"]][[id_op]]$id <- output_list[id_ov]
        # mutate file name
        x[["output"]][[id_op]]$file$name <- sub(
          # use filename as filtering pattern
          rev(strsplit(x[["output"]][[1]]$file$name, "/")[[1]])[1],
          # for replacement check timestep, if not annual take first letter
          #  e.g. "m" in "monthly", "d" in "daily"
          # some "annual" outputs start with "a", some not -> no rule!
          paste0(ifelse(x[["outputvar"]][[
                          which(outputvar_names == output_list[id_ov])
                        ]]$timestep != "annual",
                        substring(x[["outputvar"]][[
                          which(outputvar_names == output_list[id_ov])
                        ]]$timestep, 1, 1),
                        ""),
                 output_list[id_ov],
                 ".",
                 # reuse already replaced output file format from template
                 rev(strsplit(x[["output"]][[1]]$file$name, "\\.")[[1]])[1]),
          # reuse already replaced output path from template
          x[["output"]][[id_op]]$file$name
        )
      } else {
        # if ID not available print warning
        warning(paste0("Output with ID ",
                       output_list[id_ov],
                       " is not available in current model version",
                       " (not defined in outputvars.js)."))
      }
    }
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
