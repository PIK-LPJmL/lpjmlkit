#' @title Data type and size
#'
#' @description Provide information on the data type used in an LPJmL file
#'   based on the 'datatype' attribute included in the file header.
#'
#' @param header Header list object as returned by read_header() or
#'   create_header().
#'
#' @return The function returns a list object with three components:
#' * type: R data type; can be used with 'what' parameter of `readBin()`.
#' * size: size of data type; can be used with 'size' parameter of `readBin()`.
#' * signed: whether or not the data type is signed; can be used with 'signed'
#'   parameter of `readBin()`.
#'
#' @examples
#' \dontrun{
#' # Read file header
#' header <- read_header("filename.clm")
#' # Open file for reading
#' fp <- file("filename.clm", "rb")
#' # Skip over file header
#' seek(fp, get_headerize(header))
#' # Read in file data
#' file_data <- readBin(
#'   fp,
#'   what = get_datatype(header)$type,
#'   size = get_datatype(header)$size,
#'   signed = get_datatype(header)$signed,
#'   n = header$header["ncell"] * header$header["nbands"] *
#'       header$header["nyear"],
#'   endian = header[["endian"]]
#' )
#' # Close file
#' close(fp)
#' }
#'
#' @seealso
#' * [read_header()] for reading headers from LPJmL files.
#' * [create_header()] for creating headers from scratch.
#' * [get_headersize()] for determining the size of file headers.
#'
#' @export
get_datatype <- function(header) {
  if (is.list(header) && !is.null(header$header)) {
    header <- header$header
  }
  if (!is.finite(as.integer(header["datatype"]))) {
    stop("Header does not contain datatype field")
  }
  if (as.integer(header["datatype"]) < 0 ||
    as.integer(header["datatype"]) > 4) {
    stop(
      paste(
        "Invalid datatype",
        sQuote(as.integer(header["datatype"])),
        "in header"
      )
    )
  }
  switch(as.integer(header["datatype"]) + 1,
    list(type = integer(), size = 1, signed = FALSE),
    list(type = integer(), size = 2, signed = TRUE),
    list(type = integer(), size = 4, signed = TRUE),
    list(type = double(), size = 4, signed = TRUE),
    list(type = double(), size = 8, signed = TRUE)
  )
}
