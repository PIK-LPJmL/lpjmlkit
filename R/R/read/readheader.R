#' Reads a header from an LPJ input file
#'
#' tries to determine header version unless force_version is provided
#' return value: list with 3 components:                              ##
#' - header name, e.g. LPJGRID                                        ##
#' - header values (11 in total), if header version is <3, partially  ##
#'   filled with default values                                       ##
#' - endian of file (little or big)
#'
#' @param filename filename to read header from
#' @param force_version force clm version (default NULL - no)
#' @param endian endianness (default copied from local platform -- .Platform$endian)
#'
#' @return list-object with header content
#'
#' @examples
#' \dontrun{
#' readheader(filename, force_version=NULL)
#' }
#'
#' @export
readheader <- function(filename, force_version=NULL,endian=.Platform$endian) {
  if(!file.exists(filename)) {
    stop(paste("Error in readheader:", filename, "does not exist"))
  }
  zz <- file(filename, "rb")
  headername <- rawToChar(readBin(zz, raw(), n=30), multiple=TRUE)
  headername <- headername[1:(min(which(!grepl("[[:alpha:]_]", headername)))-1)]
  headername <- paste(headername, collapse="")
  if(substr(headername, 1,3) != "LPJ") {
    close(zz)
    stop(paste("Error in readheader: invalid header name", headername))
  }
  seek(zz, nchar(headername))
  endian <- .Platform$endian
  version <- readBin(zz, integer(), size=4, n=1, endian=endian)
  if(bitwAnd(version, 0xff)==0) {
    endian <- ifelse(endian=="little", "big", "little")
    seek(zz, nchar(headername))
    version <- readBin(zz, integer(), size=4, n=1, endian=endian)
  }
  if(!is.null(force_version)) {
    print(paste("Forcing header version to", force_version))
    version <- force_version
  }
  headerdata <- readBin(zz, integer(), size=4, n=6, endian=endian)
  names(headerdata) <- c("order", "firstyear", "nyear", "firstcell", "ncell", "nbands")
  if(version == 2) {
    headerdata <- c(headerdata, readBin(zz, double(), size=4, n=2, endian=endian))
    names(headerdata) <- c(names(headerdata[1:6]), "cellsize_lon", "scalar")
  }
  if(version == 3) {
    headerdata <- c(headerdata, readBin(zz, double(), size=4, n=3, endian=endian))
    headerdata <- c(headerdata, readBin(zz, integer(), size=4, n=1, endian=endian))
    names(headerdata) <- c(names(headerdata[1:(length(headerdata)-4)]), "cellsize_lon", "scalar", "cellsize_lat", "datatype")
  } else {
    if(length(headerdata)==6) {
      headerdata <- c(headerdata, cellsize_lon=0.5, scalar=1, cellsize_lat=0.5, datatype=1)
      warning("Type 1 header. Adding default values for cellsize, scalar and datatype which may not be correct in all cases")
    }
    if(length(headerdata)==8) {
      headerdata <- c(headerdata, cellsize_lat=as.double(headerdata["cellsize_lon"]), datatype=1)
      warning("Type 2 header. Adding default value for datatype which may not be correct in all cases")
    }
  }
  close(zz)
  return(list(name=headername, header=c(version=version, headerdata), endian=endian))
}
