#' Read an LPJmL config file (JSON)
#'
#' Reads a config JSON file (not `lpjml.js`!, but its precompiled version which
#' can be generated using `parse_config` or running `cpp -P filename.js`) and
#' turns it into a nested list object.
#'
#' @param filename Character string representing path
#' (if different from current working directory) and filename
#'
#' @return A nested list object representing the structure of `config_*.json`
#'
#' @examples
#' \dontrun{
#'  config <- read_config(filename = "config_spinup.json")
#'
#'  config[["version"]]
#'  # [1] "5.3"
#'
#'  config[["pftpar"]][[1]][["name"]]
#'  # [1] "tropical broadleaved evergreen tree"
#'
#'  config[["input"]][["coord"]][["name"]]
#'  # [1] "input_VERSION2/grid.bin"
#' }
#' @export
read_config <- function(filename) {
    tmp_json <- jsonlite::read_json(path = filename, simplify = FALSE)
  return(tmp_json)
}
