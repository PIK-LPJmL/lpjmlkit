#' @title Detect the file type of an LPJmL file (input, output, configuration)
#'
#' @description This utility function tries to detect automatically if a
#' provided input/output file is of file type `"clm"`, `"meta"`, or `"raw"`.
#' NetCDFs and simple text formats such as ".txt" or ".csv" are also detected.\
#' Configuration files are detected if they include valid json syntax -> "json"
#' (pre-compiled) or c-json syntax if not yet pre-compiled -> "cjson" or "js".
#'
#' @param filename Character string naming the file to check.
#'
#' @param else_raw Logical, whether to return raw if no other file type
#' detected. Defaults to `TRUE`.
#'
#' @return Character vector of length 1 giving the file type:
#' * "cdf" for a NetCDF file (classic or NetCDF4/HDF5 format).
#' * "clm" for a binary LPJmL input/output file with header.
#' * "meta" for a JSON meta file describing a binary LPJmL input/output file.
#' * "raw" for a binary LPJmL input/output file without header. This is also the
#'     default if no other file type can be recognized.
#' * "text" for any type of text-only file, e.g. ".txt" or ".csv"
#' * "cjson" or "js" for non pre-complied configuration file
#' * "json" for pre-compiled configuration file
#'
#' @export
#'
#' @examples
#' \dontrun{
#' detect_type(filename = "filename.clm")
#' [1] "clm"
#' }
detect_type <- function(filename,
                        else_raw = TRUE) {

  if (!file.exists(filename)) {
    stop("File ", filename, " does not exist.")
  }

  # Load at most the first 1024 bytes of the file for checking
  file_check <- readBin(filename, raw(), n = min(file.size(filename), 4096))
  on.exit(rm(file_check)) # nolint:undesirable_function_linter.

  # First check for "clm". The file header should always start with "LPJ".
  if (length(file_check) > 3 && all(
    rawToChar(utils::head(file_check, 3), multiple = TRUE) == c("L", "P", "J")
  )) {
    return("clm")
  }

  # Next, check for NetCDF format
  if ((length(file_check) > 3 && all(
    rawToChar(utils::head(file_check, 3), multiple = TRUE) ==
    c("C", "D", "F") # Classic NetCDF format
  )) || (length(file_check) > 8 && all(
    utils::head(file_check, 8) ==
    c(0x89, 0x48, 0x44, 0x46, 0x0d, 0x0a, 0x1a, 0x0a) # HDF5/NetCDF4 format
  ))) {
    return("cdf")
  }

  # Next, check if file contains only text. This could be JSON or other text
  # formats such as .csv or .dat files.
  if (
    all(grepl("[[:print:][:space:]]", rawToChar(file_check, multiple = TRUE)))
  ) {

    # Check if the text file is a JSON file. JSON files normally start with "{".
    # Remove any white space or comments at the beginning of the file. This will
    # not detect a JSON if file has > 1024 bytes of white space or comments.
    first_strings <- scan(
      text = rawToChar(file_check),
      what = "char",
      strip.white = TRUE,
      nmax = 10,
      comment.char = "/",
      quiet = TRUE
    )

    if ("{" %in% first_strings) {
      if ("sim_id" %in% first_strings) {
        if (jsonlite::validate(paste(readLines(filename), collapse="\n"))) {
          return("json")
        } else {
          return(ifelse(tools::file_ext(filename) == "js", "js", "cjson"))
        }
      } else if ("history" %in% first_strings){
        return("meta")
      } else {
        stop("Non usable json structure.")
      }
    } else {
      # Generic text type, no further parsing
      return("text")
    }
  }
  # Otherwise, assume it is a "raw"
  if (else_raw) {
    return("raw")
  } else {
    stop("Non valid LPJmL file format.")
  }
}
