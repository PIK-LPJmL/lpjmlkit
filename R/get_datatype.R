#' @title Data type and size
#'
#' @description Provide information on the data type used in an LPJmL file
#'   based on the 'datatype' attribute included in the file header.
#'
#' @param header Header list object as returned by read_header() or
#'   create_header(). Alternatively, can be a single integer just giving the
#'   data type code or a single character string giving one of the LPJmL type
#'   names c("byte", "short", "int", "float", "double").
#' @param fail Whether function should fail if datatype is invalid. Default: TRUE.
#'   If set to FALSE, function returns NULL if datatype is invalid.
#'
#' @return On success, the function returns a list object with three components:
#' * type: R data type; can be used with 'what' parameter of `readBin()`.
#' * size: size of data type; can be used with 'size' parameter of `readBin()`.
#' * signed: whether or not the data type is signed; can be used with 'signed'
#'   parameter of `readBin()`.
#' 
#' If `fail = FALSE`, the function returns NULL if an invalid datatype is
#' provided.
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
get_datatype <- function(header, fail = TRUE) {
  if (is.list(header) && !is.null(header$header)) {
    header <- header$header
  }
  # Also support data type string used in LPJmL meta files.
  if (is.character(header) && length(header) == 1) {
    header <- c(
      datatype = switch(
        header,
        byte = 0,
        short = 1,
        int = 2,
        float = 3,
        double = 4,
        header
      )
    )
    if (is.character(header)) {
      if (fail) {
        stop(paste("Invalid datatype string", sQuote(header)))
      } else {
        return(NULL)
      }
    }
  }
  # Also support single numeric value instead of full header
  # Possible header items besides datatype
  header_items <- c(
    "version", "order", "firstyear", "nyear", "firstcell", "ncell", "nbands",
    "cellsize_lon", "scalar", "cellsize_lat", "nstep", "timestep"
  )
  if (is.numeric(header) && length(header) == 1 &&
    (is.null(names(header)) || !names(header) %in% header_items)
  ) {
    names(header) <- "datatype"
  }
  if (!is.finite(as.integer(header["datatype"]))) {
    stop("Header does not contain datatype field")
  }
  if (as.integer(header["datatype"]) < 0 ||
    as.integer(header["datatype"]) > 4) {
    if (fail) {
      stop(
        paste(
          "Invalid datatype",
          sQuote(as.integer(header["datatype"])),
          "in header"
        )
      )
    } else {
      return(NULL)
    }
  }
  switch(as.integer(header["datatype"]) + 1,
    list(type = integer(), size = 1, signed = FALSE),
    list(type = integer(), size = 2, signed = TRUE),
    list(type = integer(), size = 4, signed = TRUE),
    list(type = double(), size = 4, signed = TRUE),
    list(type = double(), size = 8, signed = TRUE)
  )
}
