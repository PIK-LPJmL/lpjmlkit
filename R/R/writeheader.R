#' write a header to a file
#'
#' expects a list following the structure returned by readheader() or
#' new_header()
#' will fail if output file exists, unless overwrite set to TRUE
#'
#' @param filename filename to write header into
#' @param header header list
#' @param overwrite overwrites output file if exists (default FALSE)
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' writeheader(filename="filename.clm", header=header, overwrite=FALSE)
#' }
#'
#' @export
writeheader <- function(filename, header, overwrite=FALSE) {
  if(!is.list(header)) stop("Error: header must be a list() object")
  if(is.null(header[["name"]]) || is.null(header[["header"]]) || is.null(header[["endian"]])) stop("Error: header must have elements name, header and endian")
  if(file.exists(filename)) {
    if(!overwrite) stop(paste("Error:", filename, "exists already. Set overwrite to TRUE if you want to force it to be overwritten"))
    print(paste("Warning:", filename, "exists already and will be overwritten."))
  }
  zz <- file(filename, "wb")
  writeBin(charToRaw(header$name), zz)
  writeBin(as.integer(header$header[c("version", "order", "firstyear", "nyear", "firstcell", "ncell", "nbands")]), zz, size=4, endian=header$endian)
  if(header$header["version"] > 1) {
    writeBin(as.double(header$header[c("cellsize_lon", "scalar")]), zz, size=4, endian=header$endian)
  }
  if(header$header["version"] > 2) {
    writeBin(as.double(header$header["cellsize_lat"]), zz, size=4, endian=header$endian)
    writeBin(as.integer(header$header["datatype"]), zz, size=4, endian=header$endian)
  }
  close(zz)
}
