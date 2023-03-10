#' @title Retrieve information from an LPJmL input/output file header
#'
#' @description Convenience function to extract information from a header object
#'   as returned by [`read_header()`] or [`create_header()`]. Returns one item
#'   per call.
#'
#' @param header LPJmL file header as returned by [`read_header()`] or
#'   [`create_header()`].
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
#' * [read_header()] for reading headers from LPJmL input/output files.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read file header
#' header <- read_header("filename.clm")
#' nyear <- get_header_item(header = header, item = "nyear")
#' }
get_header_item <- function(header, item) {
  # Check header structure.
  is_valid_header(header)

  # Check that user provided valid item (and no more than one item)
  if (length(item) != 1 || any(!item %in% valid_header_items)) {
    stop(
      paste(
        "Invalid item", toString(sQuote(item)), "\n",
        "item must be one of:", toString(sQuote(valid_header_items))
      )
    )
  }
  switch(
    item,
    name = c(name = header$name),
    endian = c(endian = header$endian),
    header$header[item]
  )
}
