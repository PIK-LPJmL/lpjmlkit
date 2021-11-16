#' Write a LPJmL config JSON file
#'
#' Requires a tibble/data frame in a defined format (see details) and writes
#' model configuration files (config_*.json) based on the parameters set
#' in each row (corresponds to each model run).
#'
#' @param params a tibble/data frame in a defined format (see details)
#'
#' @param model_path character string providing the path to LPJmL
#' (equal to LPJROOT)
#'
#' @param output_path character string - if output_path should differ from
#' model_path - provide a path where an output, a restart and a configuration
#' folder that will be containing each files are created
#'
#' @param output_list character vector containing the **id** of outputvars.
#' If defined only these defined outputs will be written. Defaults to NULL
#'
#' @param output_format character string defining the format of the output.
#' Defaults to `"raw"`, further options would be `"cdf"` (write netcdf) or
#' `"clm"` (use a header)
#'
#' @param js_filename character string providing name of the main js file to be
#' parsed. Default is "lpjml.js"
#'
#' @param parallel_cores integer defining the number of available CPU cores for
#' parallelization. Defaults to 4 (min)
#'
#' @return tibble with at least columns "sime_name" and "config_file" defined.
#' If defined in params pseude parameters "order" and dependency are included.
#' Tibble in this format is required for \link[lpjmlKit]{submit_lpjml}.
#'
#' @details
#'
#' Supply a \link[tibble]{tibble} (or data frame) for `params` in the form of
#' (random example):
#'
#' | **sim_name**      | **random_seed** | **pftpar.1.name** |
#'  **param.k_temp** | **firewood** |
#' |:------------- |------------:|:------------- | -----------------:| ------------:|
#' | scen1         | 42          | first_tree    | NA                | TRUE         |
#' | scen2         | 404         | NA            | 0.03              | FALSE        |
#'
#' To set a macro (e.g. FROM_RESTART or CHECKPOINT) provide it as a flag in
#' in bash: "-DFROM_RESTART" "-DCHECKPOINT"
#'
#' | **sim_name**      | **random_seed** | **-DFROM_RESTART** |
#' |:----------------- | ---------------:|:------------------ |
#' | scen1_spinup      | 42              | FALSE              |
#' | scen1_transient   | 42              | TRUE               |
#'
#' Another option would be to set two pseudo parameters to link runs with each
#' other. The macro "-DFROM_RESTART" is not (!) required here, but is
#' automatically set. Also a complex order is possible, e.g. nested or > 2.
#'
#' | **sim_name**      | **random_seed** | **order** | **dependeny** |
#' |:----------------- | ---------------:|:--------- | -------------:|
#' | scen1_spinup      | 42              | 1         | NA            |
#' | scen1_transient   | 42              | 2         | scen1_spinup  |
#'
#' #### Important
#' * a **sim_name** has to be provided
#' * macros as well as the pseudo parameters ("order", "dependency") are
#'   optional but lay the basis for subsequent runs using
#'   \link[lpjmlKit]{submit_lpjml}
#' * use R booleans/logical constants, namely `TRUE` and `FALSE`
#' * make sure to set value types correctly, e.g. you may want to use
#'   `as.integer()` for integer value columns
#' * use a **\link[tibble]{tibble}** over `data.frame`, it does not convert
#'   and shows the type of each column, further advantages
#'   [here](https://tibble.tidyverse.org/)
#'
#' @examples
#' \dontrun{
#' library(tibble)
#'
#' my_params <- tibble(
#'  sim_name = c("scen1", "scen2"),
#'  random_seed = as.integer(c(42, 666)),
#'  pftpar.1.name = c("first_tree", NA),
#'  param.k_temp = c(NA, 0.03),
#'  firewood = c(TRUE, FALSE)
#' )
#'
#' config_names <- write_config(params = my_params)
#'
#'   sim_name  random_seed  pftpar.1.name  param.k_temp  `firewood`
#'   <chr>           <int>  <chr>                 <dbl>  <lgl>     
#' 1 scen1               5  first_tree               NA  TRUE      
#' 2 scen2               4  NA                     0.03  FALSE     
#' }
#' @md
#' @importFrom foreach "%dopar%"
#' @importFrom magrittr %>%
#' @export
write_config <- function(params,
                         model_path,
                         output_path = NULL,
                         output_list = c(),
                         output_format = "raw",
                         js_filename = "lpjml.js",
                         parallel_cores = 4) {

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

  # call function rowwise on dataframe/tibble
  config_tmp <- tibble::tibble(sim_name = NA,
                               config_file = NA,
                               order = NA,
                               dependency = NA)

  # parallelize write_single_config, parsing and replacing json takes some time
  # create and register cluster based on available CPU cores
  cl <- parallel::makeCluster(parallel_cores)
  doParallel::registerDoParallel(cl)

  row_id <- NULL
  # parallel foreach with rbinding each config_details
  config_details <- foreach::foreach(row_id = seq_len(nrow(params)),
                                     .combine = "rbind",
                                     .packages = "tibble"
  ) %dopar% {
    # write single call
    write_single_config(params = params[row_id, ],
                        model_path = model_path,
                        output_path = output_path,
                        output_format = output_format,
                        output_list = output_list,
                        js_filename = js_filename,
                        config_tmp = config_tmp)
  }
  # close cluster
  parallel::stopCluster(cl)

  # USE FOR DEBUGGING
  # config_details <- config_tmp
  # for (row_id in seq_len(dim(params)[1])) {
  #   config_details[row_id,] <- write_single_config(params[row_id, ],
  #                                              model_path = model_path,
  #                                              output_path = output_path,
  #                                              output_format = output_format,
  #                                              output_list = output_list,
  #                                              js_filename = js_filename,
  #                                              config_tmp = config_tmp)
  # }

  # return config_details with sim_names as well as config_names
  #   order and dependency are only returned if defined in the params
  if (any(is.na(config_details$order)) ||
      all(is.na(config_details$dependency))) {
    config_details$order <- NULL
    config_details$dependency <- NULL
  }
  return(config_details)
}
