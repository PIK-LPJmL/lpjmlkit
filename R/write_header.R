#' @title Write header (any version) to LPJmL file
#'
#' @description
#' Write an LPJmL clm header to a file. The header has to be as list
#' following the structure returned by `read_header()` or `create_header()`.
#' The function will fail if the output file exists already unless 'overwrite'
#' is set to TRUE.
#'
#' @param filename Filename to write header into
#' @param header The header to be written
#' @param overwrite Whether to overwrite an existing output file (default FALSE)
#'
#' @return None
#'
#' @seealso
#' * [create_header()] for creating headers from scratch and for a more
#'   detailed description of the LPJmL header format.
#' * [read_header()] for reading headers from files.
#'
#' @examples
#' \dontrun{
#' header <- read_header(filename = "old_filename.clm")
#' write_header(
#'   filename = "new_filename.clm",
#'   header = header,
#'   overwrite = FALSE
#' )
#' }
#'
#' @export
write_header <- function(filename, header, overwrite = FALSE) {
  # check that header is valid
  if (!is.list(header)) {
    stop("Header must be a list() object")
  }
  if (is.null(header[["name"]]) || is.null(header[["header"]]) ||
    is.null(header[["endian"]])
  ) {
    stop("Header must have elements name, header and endian")
  }

  header_elements <- c(
    "version",
    "order",
    "firstyear",
    "nyear",
    "firstcell",
    "ncell",
    "nbands"
  )
  if (anyNA(header$header[header_elements])) {
    stop(
      paste0(
        "Header values must not be set to NA. Please check: ",
        toString(sQuote(names(which(is.na(header$header[header_elements])))))
      )
    )
  }
  if (header$header["version"] > 1) {
    if (anyNA(header$header[c("cellsize_lon", "scalar")])) {
      stop(
        paste0(
          "Header values must not be set to NA. Please check: ",
          toString(
            sQuote(
              names(which(is.na(header$header[c("cellsize_lon", "scalar")])))
            )
          )
        )
      )
    }
  }
  if (header$header["version"] > 2) {
    if (anyNA(header$header[c("cellsize_lat", "datatype")])) {
      stop(
        paste0(
          "Header values must not be set to NA. Please check: ",
          toString(
            sQuote(
              names(which(is.na(header$header[c("cellsize_lat", "datatype")])))
            )
          )
        )
      )
    }
  }
  # if output file exists already
  if (file.exists(filename)) {
    if (!overwrite) {
      stop(
        paste(
          filename,
          "exists already. Set overwrite to TRUE",
          "if you want to force it to be overwritten"
        )
      )
    }
    warning(paste(filename, "exists already and will be overwritten"))
  }
  # write header to file
  zz <- file(filename, "wb")
  writeBin(charToRaw(header$name), zz)
  writeBin(
    as.integer(header$header[header_elements]), zz,
    size = 4, endian = header$endian
  )
  if (header$header["version"] > 1) {
    writeBin(
      as.double(header$header[c("cellsize_lon", "scalar")]), zz,
      size = 4, endian = header$endian
    )
  }
  if (header$header["version"] > 2) {
    writeBin(
      as.double(header$header["cellsize_lat"]), zz,
      size = 4, endian = header$endian
    )
    writeBin(
      as.integer(header$header["datatype"]), zz,
      size = 4, endian = header$endian
    )
  }
  close(zz)
  invisible(filename)
}