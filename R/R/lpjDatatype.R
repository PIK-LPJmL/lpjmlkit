#' @title lpjDatatype
#'
#' @description Provide information on the data type used in an LPJmL file
#'     based on the 'datatype' attribute included in the file header.
#' 
#' @param header Header list object as returned by readHeader() or newHeader()
#'
#' @return The function returns a list object with three components:
#'     - type: R data type; can be used with 'what' parameter of readBin()
#'     - size: size of data type; can be used with 'size' parameter of readBin()
#'     - signed: whether or not the data type is signed, only relevant for integers with size 1 or 2; can be used with 'signed' parameter of readBin()
#'
#' @examples
#' \dontrun{
#' header <- readHeader("filename.clm")
#' lpjDatatype(header)
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
