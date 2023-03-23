#' Write LPJmL config files (JSON)
#'
#' Requires a \link[tibble]{tibble} (modern \link[base]{data.frame} class) in a
#' specific format (see details & examples) to write the model configuration
#' file `"config_*.json"`. Each row in the tibble corresponds to a model run.
#' The generated `"config_*.json"` is based on a js file (e.g. `"lpjml_*.js"`).
#'
#' @param x A tibble in a defined format (see details).
#'
#' @param model_path Character string providing the path to LPJmL
#' (equal to `LPJROOT` environment variable).
#'
#' @param sim_path Character string defining path where all simulation data
#'   are written. Also an output, a restart and a configuration folder are
#'   created in `sim_path` to store respective data. If `NULL`, `model_path` is
#'   used.
#'
#' @param output_list Character vector containing the `"id"` of outputvars.
#'   If defined, only these defined outputs will be written. Otherwise, all
#'   outputs set in `js_filename` will be written. Defaults to `NULL`.
#'
#' @param output_list_timestep Single character string or character vector
#'   defining what temporal resolution the defined outputs from `output_list`
#'   should have. Either provide a single character string for all outputs or
#'   a vector with the length of `output_list` defining each timestep
#'   individually. Choose between `"annual"`, `"monthly"` or `"daily"`.
#'
#' @param output_format Character string defining the format of the output.
#'   Defaults to `"raw"`. Further options: `"cdf"` (NetCDF) or `"clm"`
#'   (file with header).
#'
#' @param js_filename Character string providing the name of the main js file to
#'   be parsed. Defaults to `"lpjml.js"`.
#'
#' @param parallel_cores Integer defining the number of available CPU cores for
#'   parallelization. Defaults to `4`.
#'
#' @param debug logical If `TRUE`, the inner parallelization is switched off
#'   to enable tracebacks and all types of error messages. Defaults to `FALSE`.
#'
#' @param params Argument is deprecated as of version 1.0; use x
#'   instead.
#'
#' @param output_path Argument is deprecated as of version 1.0; use sim_path
#'   instead.
#'
#' @return \link[tibble]{tibble} with at least one column named `"sim_name"`.
#'   Run parameters `"order"` and `"dependency"` are included if defined in
#'   `x`. \link[tibble]{tibble} in this format is required for
#'   [`submit_lpjml()`].
#'
#' @details
#'
#' Supply a \link[tibble]{tibble} for `x`, in which each row represents
#' a configuration (config) for an LPJmL simulation. \cr
#' Here a config is referred to as the precompiled `"lpjml.js"` file (or file
#' name provided as `js_filename` argument), which links to all other
#' mandatory ".js" files. The precompilation is done internally by
#' [`write_config()`].\cr
#' `write_config()` uses the column names of `param` as keys for the config
#' json using the same syntax as lists, e.g. `"k_temp"` from `"param.js"`
#' can be accessed with `"param$k_temp"` or `"param[["k_temp"]]"` as the column
#' name. (The former point-style syntax - `"param.k_temp"` - is still valid but
#' deprecated) \cr
#' For each run and thus each row, this value has to be specified in the
#' \link[tibble]{tibble}. If the original value should instead be used, insert
#' `NA`.\cr
#' Each run can be identified via the `"sim_name"`, which is mandatory to
#' specify.
#'
#' ```R
#' my_params1 <- tibble(
#'   sim_name = c("scenario1", "scenario2"),
#'   random_seed = c(42, 404),
#'   `pftpar[[1]]$name` = c("first_tree", NA),
#'   `param$k_temp` = c(NA, 0.03),
#'   new_phenology = c(TRUE, FALSE)
#' )
#'
#' my_params1
#' # A tibble: 2 x 5
#' #   sim_name random_seed `pftpar[[1]]$name` `param$k_temp` new_phenology
#' #   <chr>          <dbl> <chr>                <dbl> <lgl>
#' # 1 scenario1         42 first_tree           NA    TRUE
#' # 2 scenario2        404 NA                    0.03 FALSE
#' ```
#'
#'
#' ### Simulation sequences
#' To set up spin-up and transient runs, where transient runs are dependent on
#' the spin-up(s), a parameter `"dependency"`  has to be defined as a column in
#' the \link[tibble]{tibble} that links simulations with each other using the
#' `"sim_name"`. \cr
#' Do not manually set "-DFROM_RESTART" when using `"dependency"`. The same
#' applies for LPJmL config settings "restart", "write_restart",
#' "write_restart_filename", "restart_filename", which are set automatically
#' by this function.
#' This way multiple runs can be performed in succession and build a
#' conceivably endless chain or tree.
#'
#' ```R
#' # With dependent runs.
#' my_params3 <- tibble(
#'  sim_name = c("scen1_spinup", "scen1_transient"),
#'  random_seed = c(42, 404),
#'  dependency = c(NA, "scen1_spinup")
#' )
#' my_params3
#' # A tibble: 2 x 4
#' #   sim_name        random_seed order dependency
#' #   <chr>                 <int> <lgl> <chr>
#' # 1 scen1_spinup             42 FALSE NA
#' # 2 scen1_transient         404 TRUE  scen1_spinup
#' ```
#'
#'
#' ### SLURM options
#' Another feature is to define SLURM options for each simulation (row)
#' separately. For example, users may want to set a lower wall clock limit
#' (`wtime`) for the transient run than the spin-up run to get a higher priority
#' in the SLURM queue. This can be achieved by supplying this option as a
#' parameter to `param`. \cr
#' 4 options are available, namely `sclass`, `ntask`, `wtime`, `blocking`. \cr
#' If specified in `param`, they overwrite the corresponding function arguments
#' in [`submit_lpjml()`].
#'
#' ```R
#' my_params4 <- tibble(
#'  sim_name = c("scen1_spinup", "scen1_transient"),
#'  random_seed = c(42, 404),
#'  dependency = c(NA, "scen1_spinup"),
#'  wtime = c("8:00:00", "2:00:00")
#' )
#'
#' my_params4
#' # A tibble: 2 x 5
#' #   sim_name        random_seed order dependency   wtime
#' #   <chr>                 <int> <lgl> <chr>        <chr>
#' # 1 scen1_spinup             42 FALSE NA           8:00:00
#' # 2 scen1_transient         404 TRUE  scen1_spinup 2:00:00
#' ```
#'
#'
#' ### Use of macros
#' To set a macro (e.g. "MY_MACRO" or "CHECKPOINT") provide it as a column of
#' the \link[tibble]{tibble} as you would do with a flag in the shell:
#' `"-DMY_MACRO"` `"-DCHECKPOINT"`. \cr
#' Wrap macros in backticks or \link[tibble]{tibble} will raise an error, as
#' starting an object definition with `"-"` is not allowed in *R*.
#'
#' ```R
#' my_params2 <- tibble(
#'   sim_name = c("scen1_spinup", "scen1_transient"),
#'   random_seed = c(42, 404),
#'   `-DMY_MACRO` = c(TRUE, FALSE),
#' )
#'
#' my_params2
#' # A tibble: 2 x 3
#' #   sim_name        random_seed `-DMY_MACRO`
#' #   <chr>                 <int> <lgl>
#' # 1 scen1_spinup             42 TRUE
#' # 2 scen1_transient         404 FALSE
#' ```
#'
#' ### In short
#' * `write_config()` creates subdirectories within the `sim_path` directory
#'    * `"./configurations"` to store the config files.
#'    * `"./output"` to store the output within subdirectories for each
#'      `sim_name`.
#'    * `"./restart"` to store the restart files within subdirectories for each
#'      `sim_name`.
#' * The "." syntax (e.g. `"pftpar.1.name"`) allows to create column names and
#'   thus keys for accessing values in the config json.
#' * The column `"sim_name"` is mandatory (used as an identifier).
#' * The run parameter `"dependency"` is optional but enables interdependent
#'   consecutive runs using [`submit_lpjml()`].
#' * SLURM options in `param` allow to use different values per run.
#' * If `NA` is specified as cell value the original value is used.
#' * *R* booleans/logical constants `TRUE` and `FALSE` are to be used for
#'   boolean parameters in the config json.
#' * Value types need to be set correctly, e.g. no strings where numeric values
#'   are expected.
#'
#' @examples
#' \dontrun{
#' library(tibble)
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
#'   new_phenology = c(TRUE, FALSE)
#' )
#'
#' config_details <- write_config(
#'   x = my_params,
#'   model_path = model_path,
#'   sim_path = sim_path
#' )
#'
#' config_details
#' # A tibble: 2 x 1
#' #   sim_name
#' #   <chr>
#' # 1 scen1
#' # 2 scen2
#'
#' # Usage with dependency
#' my_params <- tibble(
#'  sim_name = c("scen1_spinup", "scen1_transient"),
#'  random_seed = c(42, 404),
#'  dependency = c(NA, "scen1_spinup")
#' )
#'
#' config_details <- write_config(
#'   x = my_params,
#'   model_path = model_path,
#'   sim_path = sim_path
#' )
#'
#' config_details
#' # A tibble: 2 x 3
#' #   sim_name        order dependency
#' #   <chr>           <dbl> <chr>
#' # 1 scen1_spinup        1 NA
#' # 2 scen1_transient     2 scen1_spinup
#'
#'
# Usage with SLURM option
#' my_params <- tibble(
#'  sim_name = c("scen1_spinup", "scen1_transient"),
#'  random_seed = c(42, 404),
#'  dependency = c(NA, "scen1_spinup"),
#'  wtime = c("8:00:00", "2:00:00")
#' )
#'
#' config_details <- write_config(
#'   x = my_params,
#'   model_path = model_path,
#'   sim_path = sim_path
#' )
#'
#' config_details
#' # A tibble: 2 x 4
#' #   sim_name        order dependency   wtime
#' #   <chr>           <dbl> <chr>        <chr>
#' # 1 scen1_spinup        1 NA           8:00:00
#' # 2 scen1_transient     2 scen1_spinup 2:00:00
#'
#' }
#' @md
#' @importFrom foreach "%dopar%"
#' @importFrom magrittr %>%
#' @export
write_config <- function(x,
                         model_path,
                         sim_path = NULL,
                         output_list = c(),
                         output_list_timestep = "annual",
                         output_format = "raw",
                         js_filename = "lpjml.js",
                         parallel_cores = 4,
                         debug = FALSE,
                         params = NULL,
                         output_path = NULL) {


  # Deprecate argument params
  if (missing("x")) x <- NULL
  x <- deprecate_arg(new_arg = x,
                     deprec_arg = params,
                     version = "1.0.0")

  # Check if model_path is valid
  if (!dir.exists(model_path)) {
    stop("Folder of model_path \"", model_path, "\" does not exist!")
  }

  # Deprecate argument output_path
  sim_path <- deprecate_arg(new_arg = sim_path,
                            deprec_arg = output_path,
                            version = "1.0.0")

  if (is.null(sim_path)) sim_path <- model_path

  # Create configurations directory to store config_*.json files
  dir.create(
    paste(ifelse(is.null(sim_path), model_path, sim_path),
          "configurations",
          sep = "/"),
    recursive = TRUE,
    showWarnings = FALSE
  )

  # Check if dependency exists but not order. Calculate order automatically.
  if ("dependency" %in% colnames(x) && !"order" %in% colnames(x)) {
    x <- get_order(x)
  }

  commit_hash <- get_git_urlhash(path = model_path, raise_error = FALSE)

  # Call function row-wise on dataframe/tibble.
  #   Initiate run/SLURM parameters. If not defined by x tibble, NA columns
  #   are removed at the end of this function.
  config_tmp <- tibble::tibble(sim_name = NA,
                               order = NA,
                               dependency = NA)

  slurm_args <- c("sclass", "ntask", "wtime", "blocking")

  config_tmp[slurm_args] <- NA

  # Debug option to traceback other function errors because errors are not
  #   written correctly to outfile = error_file by parallel::makeCluster().
  #   TODO: Find a better solution (maybe via doSNOW package). # nolint
  if (!debug) { # nolint:undesirable_function_linter.

    # Create temporary file to store stdout and stderr within parallel mode
    error_file <- tempfile(fileext = ".txt")

    # Parallelize write_single_config() because parsing and replacing json takes
    # time. Create and register cluster based on available CPU cores/nodes.
    cl <- parallel::makeCluster(parallel_cores, outfile = error_file)
    doParallel::registerDoParallel(cl)

    row_id <- NULL
    # Parallel foreach param row with row binding.
    job_details <- foreach::foreach(row_id = seq_len(nrow(x)),
                                    .combine = "rbind",
                                    .packages = "tibble",
                                    .errorhandling = "stop"
    ) %dopar% {

      # Write a single configuration
      tryCatch({
        write_single_config(x = x[row_id, ],
                            model_path = model_path,
                            sim_path = sim_path,
                            output_format = output_format,
                            output_list = output_list,
                            output_list_timestep = output_list_timestep,
                            js_filename = js_filename,
                            config_tmp = config_tmp,
                            slurm_args = slurm_args,
                            commit_hash = commit_hash)

      # Stop if an error occurs
      }, error = function(e) {

        # Check if error is returned
        if (e != "") {

          # Error with hint to use the debug argument
          stop(
            e,
            "Please use argument debug = TRUE for traceback ",
            " functionality",
            call. = FALSE
          )
        } else {

          # Hint to use the debug argument
          stop("This is not a common error, please use argument debug = TRUE")
        }
      })
    }

    # Close cluster
    parallel::stopCluster(cl)

    # Check for and display any warnings written to error_file by parallel
    # processes
    warns <- readLines(error_file, warn = FALSE)
    warn_msg <- which(grepl("Warning message", warns)) + 2

    if (length(warn_msg) > 0) {
      invisible(sapply(unique(warns[warn_msg]), warning, call. = FALSE)) # nolint:undesirable_function_linter.
    }

  } else {
    # Run without parallelization to allow debugging of write_single_config()
    job_details <- config_tmp

    for (row_id in seq_len(dim(x)[1])) {
      job_details[row_id, ] <- write_single_config(x[row_id, ],
                                                   model_path = model_path,
                                                   sim_path = sim_path,
                                                   output_format = (
                                                     output_format),
                                                   output_list = output_list,
                                                   output_list_timestep = (
                                                     output_list_timestep),
                                                   js_filename = js_filename,
                                                   config_tmp = config_tmp,
                                                   slurm_args = slurm_args,
                                                   commit_hash = commit_hash)
    }
  }


  # Return job_details with sim_names as well as config_names.
  #   "order" and "dependency" are only returned if defined in x.
  if (any(is.na(job_details$order)) ||
      all(is.na(job_details$dependency))) {
    job_details$order <- NULL
    job_details$dependency <- NULL
  }

  na_columns <- apply(is.na(job_details[slurm_args]), 2, all)

  if (any(na_columns)) {
    job_details[names(which(na_columns))] <- NULL
  }

  attr(job_details, "stages") <- c("config")

  job_details
}


# Function to write a single config_<sim_name>.json file using the default
#   config.json file.
#   mutate* functions are applied to change params/keys after provided
#   data frame/tibble.
write_single_config <- function(x,
                                model_path,
                                sim_path,
                                output_list,
                                output_list_timestep,
                                output_format,
                                js_filename,
                                config_tmp,
                                slurm_args,
                                commit_hash = "") {

  # Read json file without simplification (to vector) to avoid destroying the
  #   original json structure (important to be readable for LPJmL).
  #   Save it as config.json (as a convention).
  if (is.null(x[["sim_name"]])) {
    stop("A sim_name is missing in `x`.")
  }

  config_tmp$sim_name <- x[["sim_name"]]

  # Check if order and dependency is defined to set sequence of dependent runs.
  #   Include error catching for missing order or dependency if other is defined.
  if (!is.null(x[["order"]])) {

    if (x[["order"]] == 1) {
      from_restart <- FALSE
      config_tmp$order <- x[["order"]]

    } else if (x[["order"]] > 1) {
      from_restart <- TRUE
      config_tmp$order <- x[["order"]]

      if (is.null(x[["dependency"]])) {
        stop(x[["sim_name"]], "'s field dependency is missing!")
      } else {
        config_tmp$dependency <- x[["dependency"]]
      }

    } else {
      stop(x[["sim_name"]], "'s order is not valid!")
    }

  } else {
    from_restart <- FALSE
  }

  if (any(slurm_args %in% colnames(x))) {
    config_tmp[slurm_args[slurm_args %in% colnames(x)]] <- (
      x[slurm_args[slurm_args %in% colnames(x)]]
    )
  }

  # Check if macros defined use option -D for filtering.
  #   If macros are set false then ignore but use names to sort from parameters.
  if (any(grepl("^-D", colnames(x)))) {
    macro <- unlist(x[grepl("^-D", colnames(x))])
    macro_name <- names(macro)

    if (length(which(macro)) == 0) {
      macro <- ""
    } else {
      macro <- names(which(macro))
    }

  } else {
    macro <- ""
    macro_name <- NULL
  }

  # Parse config and evaluate macros using the cpp precompiler
  tmp_json <- parse_config(path = model_path,
                           from_restart = from_restart,
                           js_filename = js_filename,
                           macro = macro) %>%

    # Replace output and restart parameters (paths, output format & which
    # outputs)
    mutate_config_output(params = x,
                         sim_path = sim_path,
                         output_format = output_format,
                         output_list = output_list,
                         output_timestep = output_list_timestep,
                         dir_create = !testthat::is_testing()) %>%

    # Insert parameters/keys from x.
    #   Columns as keys and rows as values (values, vectors possible).
    mutate_config_param(params = x,
                        exclude_macros = macro_name,
                        commit_hash = commit_hash,
                        slurm_args = slurm_args)

  if (!testthat::is_testing()) {

    # Write config json file, use sim_name for naming.
    #   Additional jsonlite::write_json arguments are very important to be
    #   readable in LPJmL (type conservation/hinting).
    jsonlite::write_json(
      path = paste0(sim_path,
                    "/configurations/",
                    "config_",
                    x[["sim_name"]],
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


# Function to run cpp precompiler on lpjml.js to parse config.json.
#  Define from_restart and any other macros set by user.
parse_config <- function(path,
                         from_restart = FALSE,
                         js_filename = "lpjml.js",
                         macro = "") {

   # processx::run kills any occuring subprocesses to avoid fork bombs.
   tmp_json <- processx::run(command = "bash", # nolint:object_usage_linter.
                             args = c(
                               "-c",
                               paste0("cpp -P ./",
                                      js_filename,
                                      ifelse(from_restart,
                                             " -DFROM_RESTART ",
                                             " "),
                                      paste(macro, collapse = " "))
                             ),
                             wd = path,
                             cleanup_tree = TRUE)$stdout %>%
    jsonlite::parse_json(simplify = FALSE)

  tmp_json
}


# Function to rewrite parts in the output and restart section of precompiled
#   and read (as list) lpjml.js > config.json.
#   Output format (raw, clm cdf), output selection, output path, restart_path.
mutate_config_output <- function(x, # nolint:cyclocomp_linter.
                                 params,
                                 sim_path,
                                 output_format,
                                 output_list,
                                 output_timestep,
                                 dir_create = FALSE) {

  # Concatenate output path and create folder if set
  opath <- paste(sim_path, "output", params[["sim_name"]], "", sep = "/")
  if (dir_create) dir.create(opath, recursive = TRUE, showWarnings = FALSE)

  if (is.null(output_list) || x[["nspinup"]] > 500) {
    for (x_id in seq_len(length(x[["output"]]))) {

      # Replace output format in x if defined (e.g. raw, clm, cdf)
      if (x[["output"]][[x_id]]$file$fmt != "txt") {
        x[["output"]][[x_id]]$file$fmt <- output_format
      }

      # Replace output path in x
      x[["output"]][[x_id]]$file$name <- gsub("output/",
                                              opath,
                                              x[["output"]][[x_id]]$file$name)
    }

  } else {

    # Get list of outputvar names
    outputvar_names <- unlist(lapply(x[["outputvar"]], function(x)x$name)) # nolint:paren_body_linter.
    outputvar_units <- unlist(lapply(x[["outputvar"]], function(x)x$unit)) # nolint:paren_body_linter.

    # Empty output and include grid if not done
    x["output"] <- list(c())

    if (!("grid" %in% output_list) && !("cdf" %in% output_format)) {
      output_list <- append(output_list, "grid", after = 0)
      output_timestep <- append(output_timestep, NA, after = 0)

      length_output_timestep <- ifelse(length(output_timestep) == 2,
                                      1,
                                      length(output_timestep))

    } else {
      length_output_timestep <- length(output_timestep)
    }

    # Iterate over all defined outputs.
    for (id_ov in seq_len(length(output_list))) {

      # Get elements in output list that are not defined in x[["outputvar"]]
      if (output_list[id_ov] %in% outputvar_names) {

        # Create empty (new) output list to be appended at the end
        new_output <- list()
        new_output[["id"]] <- output_list[id_ov]
        new_output[["file"]] <- list()

        # Output format three possibilities: netcdf: cdf, raw: bin and clm
        new_output[["file"]][["fmt"]] <- ifelse(
          length(output_format) == 1 && is.character(output_format),
          ifelse(output_list[id_ov] == "globalflux", "txt", output_format),
          stop(
            "No valid output_format. Please choose in from \"raw\"",
            " \"clm\" or \"cdf\" in form of a single character ",
            "string."
          )
        )

        # Output_timestep could be supplied as a single character string
        #   prescribing a timestep for all outputs or as a character vector
        #   with the length of output_list to assign an individual timestep for
        #   each output.
        if (length_output_timestep == 1 &&
            !(output_list[id_ov] %in% c("grid", "globalflux"))) {

          new_output[["file"]][["timestep"]] <- ifelse(
            stats::na.omit(output_timestep)[1] %in% c("daily",
                                                      "monthly",
                                                      "annual"),
            stats::na.omit(output_timestep)[1],
            stop(
              "No valid output_timestep. Please choose from ",
              "\"daily\", \"monthly\" or \"annual\" in form of",
              " a single character string."
            )
          )

        } else if (length_output_timestep == length(output_list) &&
            !(output_list[id_ov] %in% c("grid", "globalflux"))) {

          new_output[["file"]][["timestep"]] <- ifelse(
            output_timestep[id_ov] %in% c("daily", "monthly", "annual"),
            output_timestep[id_ov],
            stop(
              "No valid output_timestep. Please choose of \"daily\"",
              " \"monthly\" or \"annual\" in form of a single ",
              "character string."
            )
          )

        } else if (
          !(length_output_timestep %in% c(1, length(output_list)))) {
          stop(
            "output_timestep does not have a valid length. Please ",
            "supply either a single character string or a vector ",
            "matching the length of output_list."
          )
        }

        # Adjust correct units to avoid correction factors in LPJmL
        unit_replace <- outputvar_units[
          which(output_list[id_ov] == outputvar_names)
        ]

        new_output[["file"]][["unit"]] <- gsub(
          "/yr$|/month$|/day$",
          switch(
            ifelse(length(output_timestep) > 1,
                   output_timestep[id_ov],
                   output_timestep) %>%
              ifelse(is.na(.), "annual", .),
            annual = "/yr",
            monthly = "/month",
            daily = "/day"
          ),
          unit_replace
        )

        # Create file name with correct path, corresponding outputvar name and
        #   file extension based on the output_format
        new_output[["file"]][["name"]] <- paste0(
          opath,
          output_list[id_ov], ".",
          ifelse(output_list[id_ov] == "globalflux",
            "txt",
            switch(output_format,
                   raw = "bin",
                   clm = "clm",
                   cdf = "nc4")
          )
        )

        # Append new output to output in config
        x[["output"]] <- append(x[["output"]], list(new_output))

      } else {

        # If ID not available print warning
        warning(
          "Output with ID ",
          output_list[id_ov],
          " is not available in current model version",
          " (not defined in outputvars.js)."
        )
      }
    }
  }

  # Replace restart paths if write restart is set
  rpath <- paste(sim_path, "restart", params[["sim_name"]], "", sep = "/")

  if (dir_create) dir.create(rpath, recursive = TRUE, showWarnings = FALSE)

  if (!is.null(x[["checkpoint_filename"]])) {
    x[["checkpoint_filename"]] <- gsub("restart/",
                                       rpath,
                                       x[["checkpoint_filename"]])
  }

  if (!is.null(x[["restart_filename"]]) && !is.null(params[["dependency"]])) {

    # If dependency is defined start from restart file of dependency sim_name
    x[["restart_filename"]] <- paste0(ifelse(is.na(params[["dependency"]]),
                                        rpath,
                                        paste(sim_path,
                                              "restart",
                                              params[["dependency"]],
                                              "",
                                              sep = "/")
                                      ),
                                      "restart.lpj")

  } else if (!is.null(x[["restart_filename"]]) &&
             is.null(params[["dependency"]]) &&
             (is.na(params[["restart_filename"]]) ||
             is.null(params[["restart_filename"]]))) {

    warning(
      "With `-DFROM_RESTART` being set to TRUE",
      " please make sure to explicitly set restart_filename in",
      " params. Otherwise, the original entry is used."
    )
  }

  x[["write_restart_filename"]] <- paste0(rpath, "restart.lpj")

  return(x)
}


# Function to rewrite params in terms of JSON keys of precompiled
#   and read (as list) lpjml.js > config.json.
#   Nested keys can be reached via "key.subkey.subsubkey" -> "input.soil.name".
#   Indices can be used to access elements in lists. Lists occur only on second
#   level, e.g. "key.1.subkey" -> "soilpar.1.name".
mutate_config_param <- function(x,
                                params,
                                exclude_macros,
                                commit_hash,
                                slurm_args) {

  # Every column represents a key in config.json
  params[c("order", "dependency", slurm_args, exclude_macros)] <- NULL

  x[["sim_githash"]] <- commit_hash

  # Get all keys of config that are possible to use
  all_keys <- names_recursively(x)

  for (colname in colnames(params)) {

    # Use default value if NA is supplied
    param_value <- unlist(params[[colname]])

    # If NA use the default value (no replacement)
    if (any(is.na(param_value))) next

    # Check if common list syntax is used
    if (grepl("\\[\\[|\\]\\]|\\$", colname)) {
      x <- call_by_listsyntax(x, colname, param_value, all_keys)

    # Else it is assumed point syntax is used (previous standard)
    } else {
      x <- call_by_points(x, colname, param_value, all_keys)
    }
  }

  x
}

# Function to replace config/param (colname) of nested list x with param_value
# by common list syntax
call_by_listsyntax <- function(x, colname, param_value, all_keys) {
  # Split each keys by "[[", "[" or "$"
  keys <- strsplit(colname, "\\[\\[|\\]\\]|\\$|\\\"|\\'")[[1]]
  # Delete resulting non existing empty keys which are not allowed in the
  # following
  keys <- keys[!nchar(keys) == 0]

  # Keys must be either existing in the original config or an index
  # this is also a check to not allow any bad code to be evaluated
  if (!all(keys %in% all_keys | grepl("^[0-9]*$", keys))) {
    stop(
      paste(
        col_var(colname),
        " consists of keys that do not exist."
      )
    )
  }

  # Check if config/param does exists via checking if its NULL
  # non standard evaluation here to support using indices in combination with
  # keys in selection via "[[" and "[""
  tryCatch({
    eval(rlang::parse_expr(paste0("x$", colname)))
    # Stop when error occures
    }, error = function(e) {
      stop(
        paste(
          col_var(colname),
          "include a combination of keys or indices that do not exist!"
        )
      )
    }
  )

  # Again non standard evaluation with replacement of check function of
  # original type (R lacks distinction of float/double and integer values)
  eval(
    rlang::parse_expr(
      paste0(
        "x$",
        colname,
        " <- convert_integer(param_value, x$",
        colname,
        ")"
      )
    )
  )
  x
}


# Function to replace config/param (colname) of nested list x with param_value
# by "." syntax -> names(unlist(x)) with indices for unnamed list items
call_by_points <- function(x, colname, param_value, all_keys) {

  # Split each keys by "."
  keys <- strsplit(colname, "[.]")[[1]] %>%

    # Keys must be either existing in the original config or an index
    # this is also a check to not allow any bad code to be evaluated
    sapply(function(x) { # nolint:undesirable_function_linter.

      # Character strings must be in quotes
      if (!grepl("^[0-9]*$", x) && x %in% all_keys) {
        x <- dQuote(x, q = FALSE)
      } else if (!grepl("^[0-9]*$", x)) {
        stop(
          col_var(colname),
          " consists of key(s) (",
          x,
          ") that do not exist."
        )
      }
      return(x)
    })

  # Predefine evaluation of x and its keys
  eval_x <- paste0(
    "x",
    paste0("[[", keys, "]]", collapse = "")
  )

  # Check if config/param does exists via checking if its NULL
  # non standard evaluation here to support using indices in combination with
  # keys in selection via "[[" and "[""
  tryCatch({
    eval(rlang::parse_expr(eval_x))
    # Stop when error occures
    }, error = function(e) {
      stop(
        col_var(colname),
        " include a combination of keys or indices that do not exist!"
      )
    }
  )

  # Again non standard evaluation with replacement of check function of
  # original type (R lacks distinction of float/double and integer values)
  eval(
    rlang::parse_expr(
      paste0(
        eval_x,
        " <- convert_integer(param_value, ",
        eval_x,
        ")"
      )
    )
  )
  x
}


# Function to convert numerics to integers since R is missing explicit
#   non-/decimals. Both x <- 1 as well as x <- 1.0 assigns a numeric value.
convert_integer <- function(x, check_value) {

  # Check if value is a list to replace
  if (!is.list(check_value)) {

    # Convert if target value is an integer
    if (is.integer(check_value) ||
       (is.character(check_value)) && is.numeric(x)) {
      return(as.integer(x))

    } else {
      return(x)
    }

  } else {

    # For list replacements, convert values if list elements are integer
    if (all(sapply(check_value, is.integer))) {# nolint:undesirable_function_linter.
      return(lapply(x, as.integer))

    } else {
      return(x)
    }
  }
}


# Function to convert numerics to integers since R is missing explicit
#   non-/decimals. Both x <- 1 as well as x <- 1.0 assigns a numeric value.
convert_integer <- function(x, check_value) {

  # Check if value is a list to replace
  if (!is.list(check_value)) {

    # Convert if target value is an integer
    if (is.integer(check_value) ||
       (is.character(check_value)) && is.numeric(x)) {
      return(as.integer(x))

    } else {
      return(x)
    }

  } else {

    # For list replacements, convert values if list elements are integer
    if (all(sapply(check_value, is.integer))) {# nolint:undesirable_function_linter.
      return(lapply(x, as.integer))

    } else {
      return(x)
    }
  }
}


# Function to get order if not specified
get_order <- function(x) {
  .data <- NULL

  get_order_each <- function(x, sim, depend) {
    .data <- NULL
    order <- 1

    if (!is.na(depend)) {
      depend_x <- dplyr::filter(x, .data$sim_name == depend) # nolint
      order <- order + get_order_each(x, depend_x$sim_name, depend_x$dependency)
    }

    return(order)
  }

  dplyr::rowwise(x) %>%
    dplyr::mutate(order = get_order_each(., .data$sim_name, .data$dependency)) %>% # nolint
    return()
}
