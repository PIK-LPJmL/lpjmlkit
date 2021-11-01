#' @title Read header (any version) from LPJmL file.
#'
#' @description Reads a header from an LPJmL clm file. CLM is the default
#' format used for LPJmL input files and can also be used for output files.
#'
#' @param filename Filename to read header from
#' @param force_version Manually set clm version (default value 'NULL' means 
#' that version is determined automatically from header; set only if version
#' number in file is incorrect)
#'
#' @return The function returns a list with 3 components:
#' * name: header name, e.g. "LPJGRID"; describes the type of data in file
#' * header: vector of header values ('version', 'order', 'firstyear',
#'       'nyear', 'firstcell', 'ncell', 'nbands', 'cellsize_lon', 'scalar',
#'       'cellsize_lat', 'datatype') describing the file structure; if header
#'       version is <3, partially filled with default values
#' * endian: endianness of file (little or big)
#'
#' @examples
#' \dontrun{
#' header <- read_header("filename.clm")
#' }
#'
#' @seealso [new_header()] for a more detailed description of the LPJmL header format,
#'     [write_header()] for writing headers to files.
#' 
#' @export
read_header <- function(filename, force_version=NULL) {
  if(!file.exists(filename)) {
    stop(paste(filename, "does not exist"))
  }
  # open binary connection to file
  zz <- file(filename, "rb")
  # header name length was variable in header version 1, read first 30 bytes
  headername <- rawToChar(readBin(zz, raw(), n=30), multiple=TRUE)
  # determine actual header name by looking for alphanumeric characters
  headername <- headername[1:(min(which(!grepl("[[:alpha:]_]", headername)))-1)]
  headername <- paste(headername, collapse="")
  # header names start with "LPJ", test if valid value
  if(substr(headername, 1,3) != "LPJ") {
    close(zz)
    stop(paste("Invalid header name", headername))
  }
  if(headername == "LPJRESTART") {
    close(zz)
    stop("LPJRESTART header detected. This function does not support restart headers at the moment")
  }
  # skip over header
  seek(zz, nchar(headername))
  # determine file endian, try platform-specific endian as default
  endian <- .Platform$endian
  # read first integer value
  version <- readBin(zz, integer(), size=4, n=1, endian=endian)
  # determine endian used in file
  if(bitwAnd(version, 0xff)==0) {
    endian <- ifelse(endian=="little", "big", "little")
    seek(zz, nchar(headername))
    version <- readBin(zz, integer(), size=4, n=1, endian=endian)
  }
  # version usually determined from file header, but can also be forced
  if(!is.null(force_version)) {
    print(paste("Forcing header version to", force_version))
    version <- force_version
  }
  # read main header data that is included in all header versions
  headerdata <- readBin(zz, integer(), size=4, n=6, endian=endian)
  names(headerdata) <- c("order", "firstyear", "nyear", "firstcell", "ncell", "nbands")
  if(version == 2) {
    # header version 2 added two more parameters
    headerdata <- c(headerdata, readBin(zz, double(), size=4, n=2, endian=endian))
    names(headerdata) <- c(names(headerdata[1:6]), "cellsize_lon", "scalar")
  }
  if(version == 3) {
    # header version 3 added two more parameters on top of parameters added in version 2
    headerdata <- c(headerdata, readBin(zz, double(), size=4, n=3, endian=endian))
    headerdata <- c(headerdata, readBin(zz, integer(), size=4, n=1, endian=endian))
    names(headerdata) <- c(names(headerdata[1:(length(headerdata)-4)]), "cellsize_lon", "scalar", "cellsize_lat", "datatype")
  } else {
    # add default values for parameters not included in header version 1
    if(length(headerdata)==6) {
      headerdata <- c(headerdata, cellsize_lon=0.5, scalar=1, cellsize_lat=0.5, datatype=1)
      warning("Type 1 header. Adding default values for cellsize, scalar and datatype which may not be correct in all cases")
    }
    # add default values for parameters not included in header version 2
    if(length(headerdata)==8) {
      headerdata <- c(headerdata, cellsize_lat=as.double(headerdata["cellsize_lon"]), datatype=1)
      warning("Type 2 header. Adding default value for datatype which may not be correct in all cases")
    }
  }
  close(zz)
  return(list(name=headername, header=c(version=version, headerdata), endian=endian))
}
