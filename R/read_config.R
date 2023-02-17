#' Read an LPJmL configuration file
#'
#' Reads a configuration (config) file (compilable js/csjon file or json file)
#' and turns it into a nested list object.
#'
#' @param filename Character string representing path
#' (if different from current working directory) and filename.
#'
#' @param from_restart Logical, if file is not pre-compiled (no json), defining
#' whether config files should be read as from_restart (transient run) or
#' without (spinup run). Defaults to `FALSE` (spinup run)
#'
#' @param macro Character string, if file is not pre-compiled (no json), pass
#' a macro to the pre compiler, e.g. ("-DFROM_RESTART")
#'
#' @return A nested list object representing the structure of `config_*.json`.
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
#'
#'  # visualize configuration as tree view
#'  View(config)
#' }
#' @export
read_config <- function(filename,
                        from_restart = FALSE,
                        macro = "") {

  file_ext <- function(x) {
    pos <- regexpr("\\.([[:alnum:]]+)$", x)
    ifelse(pos > -1L, substring(x, pos + 1L), "")
  }

  # Detect file type of config files - compiled json or not pre-compiled
  # (or not valid)
  file_type <- file_ext(filename)

  # Read compiled config files
  if (file_type == "json") {
    tmp_json <- jsonlite::read_json(path = filename, simplify = FALSE)

  # Read compilable cjson or js files - the standard default config files
  } else if (tolower(file_type) %in% c("cjson", "js")) {
    tmp_json <- parse_config(
      path = dirname(filename),
      js_filename = basename(filename),
      from_restart = from_restart,
      macro = macro)

  } else {
    stop("File type not supported.")
  }

  tmp_json
}
