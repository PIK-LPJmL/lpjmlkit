#' @title Data type and size
#'
#' @description Provide information on the data type used in an LPJmL file
#'     based on the 'datatype' attribute included in the file header.
#' 
#' @param header Header list object as returned by readHeader() or newHeader()
#'
#' @return The function returns a list object with three components:
#' * type: R data type; can be used with 'what' parameter of readBin()
#' * size: size of data type; can be used with 'size' parameter of readBin()
#' * signed: whether or not the data type is signed, only relevant for integers with size 1 or 2; can be used with 'signed' parameter of readBin()
#'
#' @examples
#' \dontrun{
#' # read file header
#' header <- readHeader("filename.clm")
#' # open file for reading
#' fp <- file("filename.clm", "rb")
#' # skip over file header
#' seek(fp, headerSize(header))
#' # read in file data
#' fileData <- readBin(fp, what=lpjDatatype(header)$type, size=lpjDatatype(header)$size, signed=lpjDatatype(header)$signed, n=header[["header"]]["ncell"]*header[["header"]]["nbands"]*header[["header"]]["nyear"], endian=header[["endian"]])
#' # close file
#' close(fp)
#' }
#'
#' @seealso [readHeader()] for reading headers from LPJmL files, [newHeader()] for creating headers from scratch
#'
#' @export
lpjDatatype <- function(header) {
  if(is.list(header) && !is.null(header$header)) {
    header <- header$header
  } 
  if(!is.finite(as.integer(header["datatype"]))) {
    stop("Header does not contain datatype field")
  }
  return(switch(as.integer(header["datatype"])+1, list(type=integer(), size=1, signed=FALSE), list(type=integer(), size=2, signed=TRUE), list(type=integer(), size=4, signed=TRUE), list(type=double(), size=4, signed=TRUE), list(type=double(), size=8, signed=TRUE)))
}
