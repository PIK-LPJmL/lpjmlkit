#' View a LPJmL Configuration file
#'
#' Open a tab and show all configuration file content in a tree view. Branches
#' can be folded and unfolded for a better overview or a detailed view.
#'
#' @param config_file character string providing the file name and the path to
#' the config file
#' @return opens a new `View` tab
#'
#' @export
view_config <- function(config_file) {
    tmp_config <- read_config(config_file)
    utils::View(x = tmp_config,
                title = paste0("LPJmL Config: ", tmp_config$sim_name))
}