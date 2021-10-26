#' Read a LPJmL config JSON file
#'
#' Reads a JSON file (not `lpjml.js`!, but its precompiled version
#' using `parseConfig` and turns it into a nested list object
#'
#' @param filename character string representing path
#' (if differs from current working directory) and filename
#'
#' @return nested list object representing the structure of `config.json`
#'
#' @examples
#' config <- readConfig(filename = "config.json")
#'
#' config[["version"]]
#' # [1] "5.3"
#'
#' config[["pftpar"]][[1]][["name"]]
#' # [1] "tropical broadleaved evergreen tree"
#'
#' config[["input"]][["coord"]][["name"]]
#' # [1] "input_VERSION2/grid.bin"
#'
#' @export
readConfig <- function(filename) {
    tmp_json <- jsonlite::read_json(path = filename, simplify = FALSE)
  return(tmp_json)
}
