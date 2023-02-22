#' @title Detect the file type of an LPJmL input/output file
#'
#' @description This utility function tries to detect automatically if a
#'   provided file is of `"clm"`, `"meta"`, or `"raw"` file type. NetCDFs and
#'   simple text formats such as ".txt" or ".csv" are also detected.
#'
#' @param filename Character string naming the file to check.
#'
#' @param meta If `TRUE` return "meta" instead of json. Defaults to `FALSE`
#'
#' @return Character vector of length 1 giving the file type:
#' * "cdf" for a NetCDF file (classic or NetCDF4/HDF5 format).
#' * "clm" for a binary LPJmL input/output file with header.
#' * "json" for a JSON meta file describing a binary LPJmL input/output file.
#' * "raw" for a binary LPJmL input/output file without header. This is also the
#'     default if no other file type can be recognized.
#' * "text" for any type of text-only file, e.g. ".txt" or ".csv"

#' @examples
#' \dontrun{
#' detect_type(filename = "filename.clm")
#' [1] "clm"
#' }
#' @noRd
detect_type <- function(filename, meta = FALSE) {

  if (!file.exists(filename)) {
    stop("File ", filename, " does not exist.")
  }

  # Load at most the first 10 bytes of the file for checking
  file_check <- readBin(filename, raw(), n = min(file.size(filename), 10))
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
    # Remove any white space at the beginning of the file. This will not detect
    # a JSON if file has > 10 bytes of white space or includes comments.
    first_char <- scan(
      text = rawToChar(file_check),
      what = "char",
      strip.white = TRUE,
      nmax = 1,
      quiet = TRUE
    )

    if (length(first_char) == 1 && first_char == "{") {
      return(ifelse(meta, "meta", "json"))

    } else {
      # Generic text type, no further parsing
      return("text")
    }
  }
  # Otherwise, assume it is a "raw"
  "raw"
}
