#' @title Write LPJmL header object to an LPJmL input (or output) file
#'
#' @description
#' Write an LPJmL clm header to a file. The header has to be a list
#' following the structure returned by [`read_header()`] or [`create_header()`].
#' The function will fail if the output file exists already unless `overwrite`
#' is set to `TRUE`.
#'
#' @param filename Filename to write header into.
#' @param header The header to be written.
#' @param overwrite Whether to overwrite an existing output file
#'   (default `FALSE`).
#'
#' @return Returns `filename` invisibly.
#'
#' @seealso
#' * [`create_header()`] for creating headers from scratch and for a more
#'   detailed description of the LPJmL header format.
#' * [`read_header()`] for reading headers from files.
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
  # Check that header is valid
  is_valid_header(header)

  # Check whether output file exists already
  if (file.exists(filename)) {
    if (!overwrite) {
      stop(
        filename,
        " exists already. Set overwrite to TRUE",
        " if you want to force it to be overwritten"
      )
    }
    warning(filename, " exists already and will be overwritten")
  }
  # Write header to file
  fp <- file(filename, "wb")
  writeBin(charToRaw(header$name), fp)
  writeBin(
    as.integer(header$header[base_header_items]), fp,
    size = 4, endian = header$endian
  )
  if (header$header["version"] > 1) {
    writeBin(
      as.double(header$header[c("cellsize_lon", "scalar")]), fp,
      size = 4, endian = header$endian
    )
  }
  if (header$header["version"] > 2) {
    writeBin(
      as.double(header$header["cellsize_lat"]), fp,
      size = 4, endian = header$endian
    )
    writeBin(
      as.integer(header$header["datatype"]), fp,
      size = 4, endian = header$endian
    )
  }
  if (header$header["version"] > 3) {
    writeBin(
      as.integer(header$header[c("nstep", "timestep")]), fp,
      size = 4, endian = header$endian
    )
  }
  close(fp)
  invisible(filename)
}
