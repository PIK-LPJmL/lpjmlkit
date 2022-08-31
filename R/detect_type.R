#' @title Detect file type of an LPJmL file
#' @description This utility function tries to detect automatically if a
#' provided file is of "clm", "meta", or "raw" file type. NetCDFs and simple
#' text formats such as `.txt` or `.csv` are also detected.
#' 
#' @param file_name Character string naming the file to check
#' @return character vector of length 1 giving the file type:
#' * "cdf" for a NetCDF file (classic or NetCDF4/HDF5 format)
#' * "clm" for a binary LPJmL file with header
#' * "meta" for a JSON meta file describing an LPJmL binary file
#' * "raw" for a binary LPJmL file without header
#' * "text" for any type of text-only file, e.g. `.txt` or `.csv`
#' @export
detect_type <- function(file_name) {
  # First check for "clm". The file header should always start with "LPJ".
  if (all(
    rawToChar(readBin(file_name, raw(), n = 3), multiple = TRUE) ==
    c("L", "P", "J")
  )) {
    return("clm")
  }
  # Next, check for NetCDF format.
  if (all(
    rawToChar(readBin(file_name, raw(), n = 3), multiple = TRUE) ==
    c("C", "D", "F") # classic NetCDF format
  ) || all(
    readBin(file_name, raw(), n = 8) ==
    c(0x89, 0x48, 0x44, 0x46, 0x0d, 0x0a, 0x1a, 0x0a) # HDF5/NetCDF4 format
  )) {
    return("cdf")
  }
  # Next, check if file contains only text. This could be JSON or other text
  # formats such as .csv or .dat files.
  # maximum 1024 bytes.
  if (
    all(
      grepl(
        "[[:print:][:space:]]",
        rawToChar(
          readBin(file_name, raw(), min(file.size(file_name), 1024)),
          multiple = TRUE
        )
      )
    )
  ) {
    # Check if the text file is a JSON file. JSON files normally start with "{".
    # Remove any white space or comments at the beginning of the file.
    if (scan(file_name, "char", strip.white = TRUE, nmax = 1, comment.char = "/",
      quiet = TRUE) == "{") {
      return("meta")
    } else {
      # Generic text type
      return("text")
    }
  }
  # Otherwise, assume it is a "raw"
  return("raw")
}
