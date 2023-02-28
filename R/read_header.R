#' @title Read header (any version) from LPJmL input/output file
#'
#' @description Reads a header from an LPJmL clm file. CLM is the default
#'   format used for LPJmL input files and can also be used for output files.
#'
#' @param filename Filename to read header from.
#' @param force_version Manually set clm version. The default value `NULL` means
#'   that the version is determined automatically from the header. Set only if
#'   the version number in the file header is incorrect.
#' @param verbose If `TRUE` (the default), `read_header` provides some feedback
#'   when using default values for missing parameters. If `FALSE`, only errors
#'   are reported.
#'
#' @return The function returns a list with 3 components:
#' * name: Header name, e.g. "LPJGRID"; describes the type of data in the file.
#' * header: Vector of header values ('version', 'order', 'firstyear',
#'     'nyear', 'firstcell', 'ncell', 'nbands', 'cellsize_lon', 'scalar',
#'     'cellsize_lat', 'datatype', 'nstep', 'timestep') describing the file
#'     structure. If header version is <4, the header is partially filled with
#'     default values.
#' * endian: Endianness of file (`"little"` or `"big"`).
#'
#' @examples
#' \dontrun{
#' header <- read_header("filename.clm")
#' }
#'
#' @seealso
#' * [`create_header()`] for a more detailed description of the LPJmL header
#'     format.
#' * [`write_header()`] for writing headers to files.
#'
#' @export
read_header <- function(filename,
                        force_version = NULL,
                        verbose = !testthat::is_testing()) {

  if (!file.exists(filename)) {
    stop(paste(filename, "does not exist"))
  }
  # Open binary connection to file.
  zz <- file(filename, "rb")
  # Header name length was variable in header version 1. Read first 30 bytes.
  headername <- rawToChar(readBin(zz, raw(), n = 30), multiple = TRUE)
  # Determine actual header name by looking for alphanumeric characters. This
  # can lead to warnings if there are non-characters in the first 30 bytes.
  suppressWarnings({
    char_index <- 1:(min(which(!grepl("[[:alpha:]_]", headername))) - 1)
  })
  headername <- headername[char_index]
  headername <- paste(headername, collapse = "")
  # Header names start with "LPJ". Test if valid value.
  if (substr(headername, 1, 3) != "LPJ") {
    close(zz)
    stop(paste("Invalid header name", headername))
  }
  if (headername == "LPJRESTART") {
    close(zz)
    stop(
      paste(
        "LPJRESTART header detected.",
        "This function does not support restart headers at the moment."
      )
    )
  }
  # Skip over header
  seek(zz, nchar(headername))
  # Determine file endian. Try platform-specific endian as default.
  endian <- .Platform$endian
  # Read first integer value and test for byte order
  version <- readBin(zz, integer(), size = 4, n = 1, endian = endian)
  if (bitwAnd(version, 0xff) == 0) {
    endian <- ifelse(endian == "little", "big", "little")
    seek(zz, nchar(headername))
    version <- readBin(zz, integer(), size = 4, n = 1, endian = endian)
  }
  # Version usually determined from file header, but can also be forced
  if (!is.null(force_version)) {
    if (verbose) message("Forcing header version to ", force_version)
    version <- force_version
  }
  # Read main header attributes that are included in all header versions
  headerdata <- readBin(zz, integer(), size = 4, n = 6, endian = endian)
  names(headerdata) <- c(
    "order",
    "firstyear",
    "nyear",
    "firstcell",
    "ncell",
    "nbands"
  )
  if (version == 2) {
    # Header version 2 added two more parameters
    headerdata <- c(
      headerdata,
      readBin(zz, double(), size = 4, n = 2, endian = endian)
    )
    names(headerdata) <- c(names(headerdata[1:6]), "cellsize_lon", "scalar")
  }
  if (version >= 3) {
    # Header version 3 added two more parameters on top of parameters added in
    # version 2
    headerdata <- c(
      headerdata,
      readBin(zz, double(), size = 4, n = 3, endian = endian)
    )
    headerdata <- c(
      headerdata,
      readBin(zz, integer(), size = 4, n = 1, endian = endian)
    )
    names(headerdata) <- c(
      names(headerdata[1:(length(headerdata) - 4)]),
      "cellsize_lon",
      "scalar",
      "cellsize_lat",
      "datatype"
    )
  }
  if (version == 4) {
    # Header version 4 added two more parameters on top of parameters added in
    # version 3
    headerdata <- c(
      headerdata,
      nstep = readBin(zz, integer(), size = 4, n = 1, endian = endian)
    )
    headerdata <- c(
      headerdata,
      timestep = readBin(zz, integer(), size = 4, n = 1, endian = endian)
    )
  } else {
    # Add default values for parameters not included in header version 1
    if (length(headerdata) == 6) {
      headerdata <- c(
        headerdata,
        cellsize_lon = 0.5,
        scalar = 1,
        cellsize_lat = 0.5,
        datatype = 1,
        nstep = 1,
        timestep = 1
      )
      if (verbose)
        message(
          "Note: Type 1 header. Adding default values for cellsize, scalar, ",
          "datatype, nstep and timestep which may not be correct in all cases."
        )
    }
    # Add default values for parameters not included in header version 2`
    if (length(headerdata) == 8) {
      headerdata <- c(
        headerdata,
        cellsize_lat = as.double(headerdata["cellsize_lon"]),
        datatype = 1,
        nstep = 1,
        timestep = 1
      )
      if (verbose)
        message(
          "Note: Type 2 header. Adding default values for datatype, nstep and ",
          "timestep which may not be correct in all cases."
        )
    }
    if (length(headerdata) == 10) {
      headerdata <- c(headerdata, nstep = 1, timestep = 1)
      if (verbose)
        message(
          "Note: Type 3 header. Adding default values for nstep and timestep ",
          "which may not be correct in all cases."
      )
    }
  }
  if (verbose && is.null(get_datatype(headerdata, fail = FALSE))) {
    warning(
      "Invalid datatype ", sQuote(headerdata["datatype"]),
      " in header read from ", filename
    )
  }
  close(zz)
  list(
    name = headername,
    header = c(version = version, headerdata),
    endian = endian
  )
}
