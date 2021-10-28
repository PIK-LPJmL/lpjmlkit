# Function to write a single config_<sim_name>.json file using the default
#   config.json file
#   mutate* functions are applied to change params/keys after provided
#   data frame/tibble
write_single_config <- function(params,
                                model_path,
                                output_path,
                                output_list,
                                output_format,
                                js_filename,
                                config_tmp,
                                test_it = FALSE) {
  # read json file without simplification (to vector) to avoid destroying the
  #   original json structure (important to be readable for LPJmL)
  #   save it as config.json (as a convention)
  if (is.null(params[["sim_name"]])) {
    stop(cat("In params a sim_name is missing!"))
  }
  config_tmp$sim_name <- params[["sim_name"]]
  config_tmp$config_file <- paste0("config_",
                                       params[["sim_name"]],
                                       ".json")
  # check if order and dependency is defined to set sequence of dependent runs
  #   include error catching for missing order or dependency if other is def
  if (!is.null(params[["order"]])) {
    if (params[["order"]] == 1) {
      from_restart <- FALSE
      config_tmp$order <- params[["order"]]
    } else if (params[["order"]] > 1) {
      from_restart <- TRUE
      config_tmp$order <- params[["order"]]
      if (is.null(params[["dependency"]])) {
        stop(cat(paste0(params[["sim_name"]],
                        "'s field dependency is missing")))
      } else {
        config_tmp$dependency <- params[["dependency"]]
      }
    } else {
      stop(cat(paste0(params[["sim_name"]], "'s field order not legit")))
    }
  } else {
    from_restart <- FALSE
  }
  # check if macros define use pseudo option -D for filtering
  #   if macros are set false then ignore but use names to sort from parameters
  if (any(grepl("^-D*", colnames(params)))) {
    macro <- unlist(params[grepl("^-D*", colnames(params))])
    macro_name <- names(macro)
    if (length(which(macro)) == 0) {
      macro <- ""
    } else {
      macro <- names(which(macro))
    }
  } else {
    macro <- ""
    macro_name <- ""
  }
  # parse config using the cpp precompiler and thereby evaluate macros
  tmp_json <- parse_config(path = model_path,
                           from_restart = from_restart,
                           js_filename = js_filename,
                           macro = macro,
                           test_file = test_it) %>%
    # replace output and restart params (paths, output format & which outputs)
    mutate_config_output(params = params,
                         output_path = output_path,
                         output_format = output_format,
                         output_list = output_list,
                         dir_create = !test_it) %>%
    # params/keys insert from params data frame
    #   columns as keys and rows as values (values, vectors possible)
    mutate_config_param(params = params,
                        exclude_macros = macro_name)

  if (!test_it) {
    # write config json file, use sim_name for naming
    #   additional jsonlite::write_json arguments are very important to be
    #   readable in LPJmL (type conservation/hinting)
    jsonlite::write_json(
      path = paste0(output_path,
                    "/configurations/",
                    "config_",
                    params[["sim_name"]],
                    ".json"),
      x = tmp_json,
      auto_unbox = TRUE,
      pretty = TRUE,
      null = "null",
      digits = 10,
      always_decimal = TRUE)

    return(config_tmp)
  } else {
    return(list(tmp_json, config_tmp))
  }
}
