#' @title Write header (any version) to LPJmL file
#'
#' @description Write an LPJmL clm header to a file. The header has to be
#' a list following the structure returned by readHeader() or newHeader().
#' The function will fail if the output file exists already unless overwrite is set to TRUE.
#'
#' @param filename filename to write header into
#' @param header The header to be written
#' @param overwrite Whether to overwrite an existing output file (default FALSE)
#'
#' @return None
#'
#' @seealso [newHeader()] for creating headers from scratch and for a more detailed
#' description of the LPJmL header format, [readHeader()] for reading headers from files.
#'
#' @examples
#' \dontrun{
#' header <- readHeader(filename="old_filename.clm")
#' writeHeader(filename="new_filename.clm", header=header, overwrite=FALSE)
#' }
#'
#' @export
writeHeader <- function(filename, header, overwrite=FALSE) {
  # check that header is valid
  if(!is.list(header)) 
    stop("Header must be a list() object")
  if(is.null(header[["name"]]) || is.null(header[["header"]]) || is.null(header[["endian"]])) 
    stop("Header must have elements name, header and endian")
  if(anyNA(header$header[c("version", "order", "firstyear", "nyear", "firstcell", "ncell", "nbands")])) 
    stop(paste0("Header values must not be set to NA. Please check: ", toString(sQuote(names(which(is.na(header$header[c("version", "order", "firstyear", "nyear", "firstcell", "ncell", "nbands")])))))))
  if(header$header["version"] > 1) {
    if(anyNA(header$header[c("cellsize_lon", "scalar")])) 
      stop(paste0("Header values must not be set to NA. Please check: ", toString(sQuote(names(which(is.na(header$header[c("cellsize_lon", "scalar")])))))))
  }
  if(header$header["version"] > 2) {
    if(anyNA(header$header[c("cellsize_lat", "datatype")])) 
      stop(paste0("Header values must not be set to NA. Please check: ", toString(sQuote(names(which(is.na(header$header[c("cellsize_lat", "datatype")])))))))
  }
  # if output file exists already
  if(file.exists(filename)) {
    if(!overwrite) 
      stop(paste(filename, "exists already. Set overwrite to TRUE if you want to force it to be overwritten"))
    warning(paste(filename, "exists already and will be overwritten."))
  }
  # write header to file
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
