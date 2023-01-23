#' @title Detect file type of an LPJmL file
#' @description This utility function tries to detect automatically if a
#' provided file is of "clm", "meta", or "raw" file type. NetCDFs and simple
#' text formats such as `.txt` or `.csv` are also detected.
#'
#' @param filename Character string naming the file to check
#' @return character vector of length 1 giving the file type:
#' * "cdf" for a NetCDF file (classic or NetCDF4/HDF5 format)
#' * "clm" for a binary LPJmL file with header
#' * "meta" for a JSON meta file describing an LPJmL binary file
#' * "raw" for a binary LPJmL file without header
#' * "text" for any type of text-only file, e.g. `.txt` or `.csv`
#' @export
detect_type <- function(filename) {
  if (!file.exists(filename)) {
    stop("File ", filename, " does not exist.")
  }
  # Load at most the first 1024 bytes of the file for checking.
  file_check <- readBin(filename, raw(), n = min(file.size(filename), 1024))
  on.exit(rm(file_check)) # nolint:undesirable_function_linter.
  # First check for "clm". The file header should always start with "LPJ".
  if (all(
    rawToChar(utils::head(file_check, 3), multiple = TRUE) == c("L", "P", "J")
  )) {
    return("clm")
  }
  # Next, check for NetCDF format.
  if (all(
    rawToChar(utils::head(file_check, 3), multiple = TRUE) ==
    c("C", "D", "F") # classic NetCDF format
  ) || all(
    utils::head(file_check, 8) ==
    c(0x89, 0x48, 0x44, 0x46, 0x0d, 0x0a, 0x1a, 0x0a) # HDF5/NetCDF4 format
  )) {
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
    if (length(a <- scan(text = rawToChar(file_check), what = "char",
      strip.white = TRUE, nmax = 1, comment.char = "/", quiet = TRUE)) == 1 &&
      a == "{") {
      return("meta")
    } else {
      # Generic text type, no further parsing
      return("text")
    }
  }
  # Otherwise, assume it is a "raw"
  return("raw")
}
