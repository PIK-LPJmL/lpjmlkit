#' @title Retrieve information from LPJmL file header
#'
#' @description Convenience function to extract information from a header object
#' as returned by `read_header()` or `create_header()`. Returns one item per
#'  call.
#'
#' @param header LPJmL file header as returned by `read_header()` or
#'   `create_header()`.
#' @param item Header information item to retrieve. One of `c("name", "version",
#'   "order", "firstyear", "nyear", "firstcell", "ncell", "nbands",
#'   "cellsize_lon", "scalar", "cellsize_lat", "datatype", "nstep", "timestep",
#'   "endian")`.
#'
#' @return Requested header item. Character string in case of "name" and
#'  "endian", otherwise numeric value.
#'
#' @seealso
#' * [create_header()] for creating headers from scratch and for a more
#'   detailed description of the LPJmL header format.
#' * [read_header()] for reading headers from files.
#'
#' @export
get_header_item <- function(header, item) {
  # Check header structure
  # Expect a list with elements "name", "header" and "endian"
  if (!is.list(header) || any(is.null(header[c("name", "header", "endian")]))) {
    stop(
      paste(
        "Header has invalid structure. Must be a list with elements",
        "'name', 'header', 'endian'"
      )
    )
  }
  # Confirm that no other elements are in list
  if (length(header) != 3) {
    stop(
      paste(
        "Header has invalid structure. Must be a list with elements",
        "'name', 'header', 'endian'"
      )
    )
  }
  # Expect only a single "name" and "endian"
  if (any(sapply(header[c("name", "endian")], length) != 1)) {
    stop("Header has invalid structure. More than one 'name' or 'endian'")
  }
  # Expect header$header to have 13 values (some of which may be defaults)
  if (length(header$header) != 13) {
    stop("Header has invalid structure. Invalid header$header")
  }
  # Valid items that can be queried from a header
  valid_items <- c(
    "name", "version", "order", "firstyear", "nyear", "firstcell", "ncell",
    "nbands", "cellsize_lon", "scalar", "cellsize_lat", "datatype", "nstep",
    "timestep", "endian"
  )
  # Check that user provided valid item (and no more than one item)
  if (length(item) != 1 || any(!item %in% valid_items)) {
    stop(
      paste(
        "Invalid item", toString(sQuote(item)), "\n",
        "item must be one of:", toString(sQuote(valid_items))
      )
    )
  }
  # Check that item is also in header (header elements are named correctly)
  if (!item %in% names(header) && !item %in% names(header$header)) {
    stop(
      paste("Header has invalid structure: item", sQuote(item), "is missing")
    )
  }
  switch(
    item,
    name = c(name = header$name),
    endian = c(endian = header$endian),
    header$header[item]
  )
}
