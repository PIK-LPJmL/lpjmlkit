#' @title Data type and size
#'
#' @description Provide information on the data type used in an LPJmL file
#'     based on the 'datatype' attribute included in the file header.
#' 
#' @param header Header list object as returned by read_header() or new_header()
#'
#' @return The function returns a list object with three components:
#' * type: R data type; can be used with 'what' parameter of readBin()
#' * size: size of data type; can be used with 'size' parameter of readBin()
#' * signed: whether or not the data type is signed, only really relevant for integers with size 1 or 2; can be used with 'signed' parameter of readBin()
#'
#' @examples
#' \dontrun{
#' # read file header
#' header <- read_header("filename.clm")
#' # open file for reading
#' fp <- file("filename.clm", "rb")
#' # skip over file header
#' seek(fp, header_size(header))
#' # read in file data
#' file_data <- readBin(fp, what=lpj_datatype(header)$type, size=lpj_datatype(header)$size, signed=lpj_datatype(header)$signed, n=header[["header"]]["ncell"]*header[["header"]]["nbands"]*header[["header"]]["nyear"], endian=header[["endian"]])
#' # close file
#' close(fp)
#' }
#'
#' @seealso [read_header()] for reading headers from LPJmL files, [new_header()] for creating headers from scratch, [header_size()] for determining the size of file headers
#'
#' @export
lpj_datatype <- function(header) {
  if(is.list(header) && !is.null(header$header)) {
    header <- header$header
  } 
  if(!is.finite(as.integer(header["datatype"]))) {
    stop("Header does not contain datatype field")
  }
  return(switch(as.integer(header["datatype"])+1, list(type=integer(), size=1, signed=FALSE), list(type=integer(), size=2, signed=TRUE), list(type=integer(), size=4, signed=TRUE), list(type=double(), size=4, signed=TRUE), list(type=double(), size=8, signed=TRUE)))
}
