#' Write a LPJmL config JSON file
#'
#' Requires a \link[tibble]{tibble} (modern \link[base]{data.frame} class) in a
#' specific format (see details & examples) to write model configuration files
#' `"config_*.json"` based on the parameters set in each row (corresponds to
#' each model run) to override them from the base file `"lpjml.js"`.
#'
#' @param params a tibble in a defined format (see details)
#'
#' @param model_path character string providing the path to LPJmL
#' (equal to LPJROOT)
#'
#' @param output_path character string - if `output_path` should differ from
#' `model_path` - provide a path where an output, a restart and a configuration
#' folder that will be containing each files are created
#'
#' @param output_list character vector containing the `"id"` of outputvars.
#' If defined only these defined outputs will be written. Defaults to NULL
#'
#' @param output_list_timestep single character string or character vector
#' defining what temporal resolution the defined output from `output_list`
#' should have. Either provide a single character string for all outputs or
#' a vector with the length of `output_list` defining each individually. Choose
#' between `"annual"`, `"monthly"` or `"daily"`.
#'
#' @param output_format character string defining the format of the output.
#' Defaults to `"raw"`, further options would be `"cdf"` (write netcdf) or
#' `"clm"` (use a header)
#'
#' @param js_filename character string providing name of the main js file to be
#' parsed. Default is `"lpjml.js"`
#'
#' @param parallel_cores integer defining the number of available CPU cores for
#' parallelization. Defaults to `4`
#'
#' @param debug logical If `TRUE` inner parallelization is switched off
#' to enable tracebacks and all types of error messages. Defaults to `FALSE`
#'
#' @return \link[tibble]{tibble} with at least columns `"sime_name"` defined.
#' If defined in params run parameters `"order"` and `"dependency"` are
#' included. \link[tibble]{tibble} in this format is required for
#' \link[lpjmlKit]{submit_lpjml}.
#'
#' @details
#'
#' Supply a \link[tibble]{tibble} for `params`, in which each row represents
#' a configuration (config) for a LPJmL run. \cr
#' Here a config is referred to as the precompiled `"lpjml.js"` file (or if you
#' name it differently, use the `js_filename` argument) which links to all other
#' mandatory `"js"` files. The precompilation is done internally by
#' `write_config`.\cr
#' `write_config` uses the column names of `param` as keys for the config
#' json using a object-oriented like syntax, e.g. `"k_temp"` from `"param.js"`
#' can be accessed with `"param.k_temp"` as the column name. \cr
#' For each run and thus each row, this value has to be specified in the
#' following. If the original value should instead be used, insert `NA`.\cr
#' Each run can be identified via the `"sim_name"`, which has to be provided in
#' the first column. \cr
#'
#' ```R
#' my_params1 <- tibble(
#'   sim_name = c("scenario1", "scenario2"),
#'   random_seed = c(42, 404),
#'   pftpar.1.name = c("first_tree", NA),
#'   param.k_temp = c(NA, 0.03),
#'   new_phenology = c(TRUE, FALSE)
#' )
#'
#' my_params1
#' # A tibble: 2 x 5
#' #   sim_name random_seed pftpar.1.name param.k_temp new_phenology
#' #   <chr>          <dbl> <chr>                <dbl> <lgl>
#' # 1 scenario1         42 first_tree           NA    TRUE
#' # 2 scenario2        404 NA                    0.03 FALSE
#' ```
#'
#'
#' To set a macro (e.g. "FROM_RESTART" or "CHECKPOINT") provide it as you would
#' do it beeing a flag in shell: `"-DFROM_RESTART"` `"-DCHECKPOINT"`. \cr
#' Also do not forget to wrap it in backticks, else \link[tibble]{tibble} will
#' raise an error, as starting an object definition with `"-"` is not allowed in
#' *R*.
#'
#' ```R
#' my_params2 <- tibble(
#'   sim_name = c("scen1_spinup", "scen1_transient"),
#'   random_seed = c(42, 404),
#'   `-DFROM_RESTART` = c(TRUE, FALSE),
#' )
#'
#' my_params2
#' # A tibble: 2 x 3
#' #   sim_name        random_seed `-DFROM_RESTART`
#' #   <chr>                 <int> <lgl>
#' # 1 scen1_spinup             42 TRUE
#' # 2 scen1_transient         404 FALSE
#' ```
#'
#'
#' A better approach, at least for setting spin-up and transient runs, would be
#' to set two available run parameters (`"order"`, `"dependency"`) that link
#' runs with each other. \cr
#' The macro "-DFROM_RESTART" is not (!) required here and is
#' instead automatically set internally for precompiling. In addition the
#' restart paths for each config are set accordingly. \cr
#' `"order"` is used to set the order for the execution or a type of run level,
#' spin-up runs are always `order=1`, i.e. historic runs would be `order=2` and a
#' future run would be `order=3`. Multiple runs can be performed at each
#' order/level, e.g. spin-up or transient runs for different scenarios. \cr
#' `"dependency"` determines which run to be used to restart from and also if
#' the run is submitted to slurm, for which run to wait until it can be started.
#' In the example above `"scen1_spinup"` would be a dependency of run
#' `"scen1_transient"`. \cr
#' This way, all conceivable scenario ensembles can be simulated in a quasi
#' arbitrarily nested and complicated manner.
#'
#' ```R
#' # with dependent runs
#' my_params3 <- tibble(
#'  sim_name = c("scen1_spinup", "scen1_transient"),
#'  random_seed = c(42, 404),
#'  order = c(FALSE, TRUE),
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
#' Another feature is to define slurm options for each run (row) separately.
#' E.g. you may want to allocate more time for the spin-up run but less for the
#' transient to get a better position in the slurm queue. This can be achieved
#' by supplying this option as parameter to `param`. \cr
#' 4 options are availble, namely `sclass` `ntask`, `wtime`, `blocking`. \cr
#' If specified in `param` they overwrite the corresponding function arguments
#' in \link[lpjmlKit]{submit_lpjml}.
#'
#' ```R
#' my_params4 <- tibble(
#'  sim_name = c("scen1_spinup", "scen1_transient"),
#'  random_seed = c(42, 404),
#'  order = c(FALSE, TRUE),
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
#' ### in short
#' * `write_config` creates subdirectories within the `output_path` directory
#'    * `"./configurations"` to store the config files
#'    * `"./output"` to store the output within subdirectories for each
#'      `"sim_name"`
#'    * `"./restart"` to store the restart files within subdirectories for each
#'      `sim_name`
#' * use the "." syntax (e.g. `"pftpar.1.name"`) to create column names and thus
#'   keys for accessing the config json values
#' * the column `"sim_name"` is mandatory (used as identifier)
#' * run parameters (`"order"`, `"dependency"`) are
#'   optional but lay the basis for subsequent runs using
#'   \link[lpjmlKit]{submit_lpjml}
#' * specify slurm options in `param` if you want to differentiate between the
#'   runs
#' * if `NA` is specified as cell value the original value is used
#' * use *R* booleans/logical constants, namely `TRUE` and `FALSE`
#' * make sure to set value types correctly
#'
#' @examples
#' \dontrun{
#' library(tibble)
#' library(lpjmlKit)
#'
#' model_path <- "./LPJmL_internal"
#' output_path <-"./my_runs"
#'
#'
#' # basic usage
#' my_params1 <- tibble(
#'   sim_name = c("scen1", "scen2"),
#'   random_seed = c(12, 404),
#'   pftpar.1.name = c("first_tree", NA),
#'   param.k_temp = c(NA, 0.03),
#'   new_phenology = c(TRUE, FALSE)
#' )
#'
#' config_details1 <- write_config(
#'   params = my_params1,
#'   model_path = model_path,
#'   output_path = output_path
#' )
#'
#' config_details1
#' # A tibble: 2 x 1
#' #   sim_name
#' #   <chr>
#' # 1 scen1
#' # 2 scen2
#'
#'
#' # usage with macro
#' my_params2 <- tibble(
#'  sim_name = c("scen1_spinup", "scen1_transient"),
#'  random_seed = c(42, 404),
#'  `-DFROM_RESTART` = c(FALSE, TRUE)
#' )
#'
#' config_details2 <- write_config(
#'   params = my_params2,
#'   model_path = model_path,
#'   output_path = output_path
#' )
#'
#' config_details2
#' # A tibble: 2 x 1
#' #   sim_name
#' #   <chr>
#' # 1 scen1_spinup
#' # 2 scen1_transient
#'
#'
#' # usage with dependency
#' my_params3 <- tibble(
#'  sim_name = c("scen1_spinup", "scen1_transient"),
#'  random_seed = c(42, 404),
#'  order = c(1, 2),
#'  dependency = c(NA, "scen1_spinup")
#' )
#'
#' config_details3 <- write_config(
#'   params = my_params3,
#'   model_path = model_path,
#'   output_path = output_path
#' )
#'
#' config_details3
#' # A tibble: 2 x 3
#' #   sim_name        order dependency
#' #   <chr>           <dbl> <chr>
#' # 1 scen1_spinup        1 NA
#' # 2 scen1_transient     2 scen1_spinup
#'
#'
# usage with slurm option
#' my_params4 <- tibble(
#'  sim_name = c("scen1_spinup", "scen1_transient"),
#'  random_seed = c(42, 404),
#'  order = c(1, 2),
#'  dependency = c(NA, "scen1_spinup"),
#'  wtime = c("8:00:00", "2:00:00")
#' )
#'
#' config_details4 <- write_config(
#'   params = my_params4,
#'   model_path = model_path,
#'   output_path = output_path
#' )
#'
#' config_details4
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
write_config <- function(params,
                         model_path,
                         output_path = NULL,
                         output_list = c(),
                         output_list_timestep = "annual",
                         output_format = "raw",
                         js_filename = "lpjml.js",
                         parallel_cores = 4,
                         debug = FALSE) {

  # check if model_path valid
  if (!dir.exists(model_path)) {
    stop(
      paste0("Folder of model_path \"", model_path, "\" does not exist!")
    )
  }
  # if output_path is not supplied use model_path as output_path
  if (is.null(output_path)) {
    output_path <- model_path
  }
  # create configurations directory to store config_*.json files
  dir.create(
    paste(ifelse(is.null(output_path), model_path, output_path),
          "configurations",
          sep = "/"),
    recursive = TRUE,
    showWarnings = FALSE
  )

  commit_hash <- get_git_urlhash(path = model_path, raise_error = FALSE)

  # call function rowwise on dataframe/tibble
  #   initiate run/slurm parameters, if not defined by tibble NA columns are
  #   removed at the end of this functions
  config_tmp <- tibble::tibble(sim_name = NA,
                               order = NA,
                               dependency = NA)
  slurm_args <- c("sclass", "ntask", "wtime", "blocking")
  config_tmp[slurm_args] <- NA

  # debug option to traceback other function errors which strangely are not
  #   written to stdout and thus outfile=error_file by parallel::makeCluster
  #   requires a better solution soon (maybe via doSNOW package)
  if (!debug) {
    # create temporary file to store stdout and stderr within parallel mode
    error_file <- tempfile(fileext = ".txt")
    # parallelize write_single_config, parsing and replacing json takes time
    # create and register cluster based on available CPU cores/nodes
    cl <- parallel::makeCluster(parallel_cores, outfile = error_file)
    doParallel::registerDoParallel(cl)
    row_id <- NULL
    # parallel foreach param row with row binding
    job_details <- foreach::foreach(row_id = seq_len(nrow(params)),
                                    .combine = "rbind",
                                    .packages = "tibble",
                                    .errorhandling = "stop"
    ) %dopar% {
      # write single call
      tryCatch({
        write_single_config(params = params[row_id, ],
                            model_path = model_path,
                            output_path = output_path,
                            output_format = output_format,
                            output_list = output_list,
                            output_list_timestep = output_list_timestep,
                            js_filename = js_filename,
                            config_tmp = config_tmp,
                            slurm_args = slurm_args,
                            commit_hash = commit_hash)
      # stop when error occures
      }, error = function(e) {
        # check if error is returned
        if (e != "") {
          # error with hint to use the debug argument
          stop(paste0(e,
                      " - Please use argument debug=TRUE for traceback ",
                      "functionality"),
               call. = FALSE)
        } else {
          # hint to use the debug argument
          stop("This is not a common error, please use argument debug=TRUE")
        }
      })
    }
    # close cluster
    parallel::stopCluster(cl)

    # check for warnings, if there are any return warning with original warning
    #   message
    warns <- readLines(error_file, warn = FALSE)
    warn_msg <- which(grepl("Warning message", warns)) + 2
    if (length(warn_msg) > 0) {
      invisible(sapply(unique(warns[warn_msg]), warning, call. = FALSE))
    }

  } else {
    job_details <- config_tmp
    for (row_id in seq_len(dim(params)[1])) {
      job_details[row_id, ] <- write_single_config(params[row_id, ],
                                                   model_path = model_path,
                                                   output_path = output_path,
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


  # return job_details with sim_names as well as config_names
  #   order and dependency are only returned if defined in the params
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
  return(job_details)
}


# Function to write a single config_<sim_name>.json file using the default
#   config.json file
#   mutate* functions are applied to change params/keys after provided
#   data frame/tibble
write_single_config <- function(params,
                                model_path,
                                output_path,
                                output_list,
                                output_list_timestep,
                                output_format,
                                js_filename,
                                config_tmp,
                                slurm_args,
                                test_it = FALSE,
                                commit_hash = "") {
  # read json file without simplification (to vector) to avoid destroying the
  #   original json structure (important to be readable for LPJmL)
  #   save it as config.json (as a convention)
  if (is.null(params[["sim_name"]])) {
    stop(cat("In params a sim_name is missing!\n"))
  }
  config_tmp$sim_name <- params[["sim_name"]]

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
                        "'s field dependency is missing!\n")))
      } else {
        config_tmp$dependency <- params[["dependency"]]
      }
    } else {
      stop(cat(paste0(params[["sim_name"]], "'s field order not legit!\n")))
    }
  } else {
    from_restart <- FALSE
  }
  if (any(slurm_args %in% colnames(params))) {
    config_tmp[slurm_args[slurm_args %in% colnames(params)]] <- (
      params[slurm_args[slurm_args %in% colnames(params)]]
    )
  }

  # check if macros defined use option -D for filtering
  #   if macros are set false then ignore but use names to sort from parameters
  if (any(grepl("^-D", colnames(params)))) {
    macro <- unlist(params[grepl("^-D", colnames(params))])
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
                         output_timestep = output_list_timestep,
                         dir_create = !test_it) %>%
    # params/keys insert from params data frame
    #   columns as keys and rows as values (values, vectors possible)
    mutate_config_param(params = params,
                        exclude_macros = macro_name,
                        commit_hash = commit_hash,
                        slurm_args = slurm_args)

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


# Function to run cpp precompiler on lpjml.js to parse config.json
#  define from_restart and if further macros required a macro
parse_config <- function(path,
                         from_restart = FALSE,
                         js_filename="lpjml.js",
                         macro = "",
                         test_file = FALSE) {
  if (!test_file) {
       # processx::run kills any occuring subprocesses to avoid fork bombs
       tmp_json <- processx::run(command = "sh",
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
  } else {
    test_json <- read_config("../testdata/test_config.json")
    return(test_json)
  }
}


# Function to rewrite parts in the output and restart section of precompiled
#   and read (as list) lpjml.js > config.json
#   output format (raw, clm cdf), output selection, output path, restart_path
mutate_config_output <- function(x,
                                 params,
                                 output_path,
                                 output_format,
                                 output_list,
                                 output_timestep,
                                 dir_create = FALSE) {

  # concatenate output path and create folder if set
  opath <- paste(output_path, "output", params[["sim_name"]], "", sep = "/")
  if (dir_create) dir.create(opath, recursive = TRUE, showWarnings = FALSE)

  if (is.null(output_list) || x[["nspinup"]] > 500) {
    for (x_id in seq_len(length(x[["output"]]))) {

      # replace output format in x if defined (e.g. raw, clm, cdf)
      if (x[["output"]][[x_id]]$file$fmt != "txt") {
        x[["output"]][[x_id]]$file$fmt <- output_format
      }

      # replace output path replace in x
      x[["output"]][[x_id]]$file$name <- gsub("output/",
                                              opath,
                                              x[["output"]][[x_id]]$file$name)
    }
  } else {
    # get list of outputvar names
    outputvar_names <- unlist(lapply(x[["outputvar"]], function(x)x$name))
    outputvar_units <- unlist(lapply(x[["outputvar"]], function(x)x$unit))

    # empty output and include grid if not done
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
    # iterate over all defined outputs
    for (id_ov in seq_len(length(output_list))) {
      # get elements in output list that are not defined in x[["outputvar"]]
      if (output_list[id_ov] %in% outputvar_names) {
        # create empty (new) output list to be appended at the end
        new_output <- list()
        new_output[["id"]] <- output_list[id_ov]
        new_output[["file"]] <- list()
        # output format three possibilities: netcdf: cdf, raw: bin and clm
        new_output[["file"]][["fmt"]] <- ifelse(
          length(output_format) == 1 && is.character(output_format),
          ifelse(output_list[id_ov] == "globalflux", "txt", output_format),
          stop(paste0("No valid output_format. Please choose in from \"raw\"",
                      " \"clm\" or \"cdf\" in form of a single character ",
                      "string."))
        )

        # output_timestep could be supplied as single character string
        #   prescribing a timestep for all outputs and a character vector with
        #   the length of output_list to assign an individual timestep for each
        if (length_output_timestep == 1 &&
            !(output_list[id_ov] %in% c("grid", "globalflux"))) {
          new_output[["file"]][["timestep"]] <- ifelse(
            stats::na.omit(output_timestep)[1] %in% c("daily",
                                                      "monthly",
                                                      "annual"),
            stats::na.omit(output_timestep)[1],
            stop(paste0("No valid output_timestep. Please choose from ",
                        "\"daily\", \"monthly\" or \"annual\" in form of",
                        " a single character string."))
          )
        } else if (length_output_timestep == length(output_list) &&
            !(output_list[id_ov] %in% c("grid", "globalflux"))) {
          new_output[["file"]][["timestep"]] <- ifelse(
            output_timestep[id_ov] %in% c("daily", "monthly", "annual"),
            output_timestep[id_ov],
            stop(paste0("No valid output_timestep. Please choose of \"daily\"",
                        " \"monthly\" or \"annual\" in form of a single ",
                        "character string."))
          )
        } else if (
          !(length_output_timestep %in% c(1, length(output_list)))) {
          stop(paste0("output_timestep does not have a valid length. Please ",
                      "supply either a single character string or a vector ",
                      "matching the length of output_list."))
        }
        # adjust correct units to avoid correction factors in LPJmL
        new_output[["file"]][["unit"]] <- gsub(
          "/yr$|/month$|/day$",
          outputvar_units[which(output_list[id_ov] %in% outputvar_names)],
          switch(
            output_format,
            annual = "yr",
            monthly = "month",
            daily = "day"
          )
        )
        # create file name with correct path, corresponding outputvar name and
        #   file ending based on the output_format
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
        # append new output to output in config
        x[["output"]] <- append(x[["output"]], list(new_output))
      } else {
        # if ID not available print warning
        warning(paste0("Output with ID ",
                       output_list[id_ov],
                       " is not available in current model version",
                       " (not defined in outputvars.js)."))
      }
    }
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
  } else if (!is.null(x[["restart_filename"]]) &&
             is.null(params[["dependency"]]) &&
             is.na(params[["restart_filename"]])) {
    warning(paste0("With `-DFROM_RESTART` being set to TRUE",
                   " please make sure to explicitly set restart_filename in",
                   " params. Else the original entry is used!"))
  }
  x[["write_restart_filename"]] <- gsub("restart/",
                                        rpath,
                                        x[["write_restart_filename"]])

  return(x)
}


# Function to rewrite params in terms of JSON keys of precompiled
#   and read (as list) lpjml.js > config.json
#   Nested keys can be reached via "key.subkey,subsubkey" -> "input.soil.name".
#   For the second level (only) indices can be used, lists occur quite on that
#   level, e.g. "key.1.subkey" -> "soilpar.1.name"
mutate_config_param <- function(x,
                                params,
                                exclude_macros,
                                commit_hash,
                                slurm_args) {
  # every column represents a key in config.json
  params[c("order", "dependency", slurm_args, exclude_macros)] <- NULL
  x[["sim_githash"]] <- commit_hash

  for (colname in colnames(params)) {
    # test if NA is supplied, then default value is used
    param_value <- unlist(params[[colname]])
    if (any(is.na(param_value))) next

    # split keys for each level
    keys <- strsplit(colname, "[.]")[[1]]

    # test for length and digits (indices) -> handle each case
    if (length(keys) > 1) {
      if (grepl("^[[:digit:]]+$", keys)[2]) {
        if (length(keys) > 2) {
          if (is.null(x[[keys[1]]][[
            as.integer(keys[2])
          ]][[keys[3:length(keys)]]])) {
            stop(paste(colname, "is not legit!"))
          } else {
            x[[keys[1]]][[as.integer(keys[2])]][[
              keys[3:length(keys)]
            ]] <- convert_integer(param_value, x[[keys[1]]][[
              as.integer(keys[2])
            ]][[keys[3:length(keys)]]])
          }
        } else {
          if (is.null(x[[keys[1]]][[as.integer(keys[2])]])) {
            stop(paste(colname, "is not legit!"))
          } else {
            x[[keys[1]]][[as.integer(keys[2])]] <- convert_integer(
              param_value, x[[keys[1]]][[as.integer(keys[2])]]
            )
          }
        }
      } else {
        if (is.null(x[[keys]])) {
          stop(paste(colname, "is not legit!"))
        } else {
          x[[keys]] <- convert_integer(param_value, x[[keys]])
        }
      }
    } else {
      if (is.null(x[[keys]])) {
        stop(paste(colname, "is not legit!"))
      } else {
        x[[keys]] <- convert_integer(param_value, x[[keys]])
      }
    }
  }
  return(x)
}


# Function to convert numerics to integers since R is missing explicit
#   non-/decimals, both x <- 1 as well as x <- 1.0 assigns a numeric value
convert_integer <- function(x, check_value) {
  # check if value is a list to replace
  if (!is.list(check_value)) {
    # check if target value is an integer -> convert
    if (is.integer(check_value) ||
       (is.character(check_value)) && is.numeric(x)) {
      return(as.integer(x))
    } else {
      return(x)
    }
  } else {
    # for list replacements, check if list elements are integer if so -> convert
    if (all(sapply(check_value, is.integer))) {
      return(lapply(x, as.integer))
    } else {
      return(x)
    }
  }
}