#' Read an LPJmL configuration file
#'
#' Reads a configuration (config) file (compilable csjon/js file or json file)
#' and turns it into a nested list object.
#'
#' @param filename Character string representing path
#'   (if different from current working directory) and filename.
#'
#' @param from_restart Logical defining whether config files should be read as
#'   from_restart (transient run) or without (spinup run). Defaults to `FALSE`
#'   (spinup run). Used only if file is not pre-compiled (no json).
#'
#' @param macro Optional character string to pass one or several macros to the
#'   pre-compiler, e.g. ("-DFROM_RESTART"). Used only if file is not
#'   pre-compiled (no json).
#'
#' @return A nested list object representing the LPJmL configuration read from
#'   `filename`.
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

  # get file extension
  get_file_ext <- function(filename) {
    extension <- gsub("^.*\\.([^.]+)$", "\\1", filename)
    if (nchar(extension) == nchar(filename)) {
      return("")
    } else {
      return(extension)
    }
  }

  # Detect file type of config files - compiled json or not pre-compiled
  # (or not valid)
  file_type <- get_file_ext(filename)

  # Read compiled config files. detect_io_type returns "json" for pure JSON without
  # comments.
  if (tolower(file_type) == "json") {
    tmp_json <- jsonlite::read_json(path = filename, simplifyVector = FALSE)

  } else {
    # Read compilable cjson or js files - the standard default config files. These
    # should be detected as "text" by detect_io_type.
    tmp_json <- parse_config(
      path = dirname(filename),
      cjson_filename = basename(filename),
      from_restart = from_restart,
      macro = macro
    )
  }

  # Elements included in LPJmL configurations differ between model versions.
  # Define a minimum set of elements that must be present to conclude that
  # filename does indeed contain an LPJmL configuration.
  required_att <- c("sim_id", "param", "soilpar", "pftpar", "input", "output")

  if (any(sapply(tmp_json[required_att], is.null))) { # nolint:undesirable_function_linter.
    message(
      "File ", sQuote(filename),
      "does not appear to be a complete LPJmL configuration.\n",
      "Missing element(s): ",
      toString(dQuote(setdiff(required_att, names(tmp_json))))
    )
  }

  tmp_json
}
