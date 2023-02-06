#' @title Set information in an LPJmL input (or output) file header
#'
#' @description Convenience function to set information in a header object as
#'   returned by [`read_header()`] or [`create_header()`]. One or several
#    header items can be set at once.
#'
#' @param header An LPJmL file header as returned by [`read_header()`] or
#'   [`create_header()`].
#' @param ... Named header items to set. Can be one or several of 'name',
#'   'version', 'order', 'firstyear', 'nyear', 'firstcell', 'ncell', 'nbands',
#'   'cellsize_lon', 'scalar', 'cellsize_lat', 'datatype', 'nstep', 'timestep',
#'   'endian'.
#'
#' @return Header `header` where header items supplied through the ellipsis
#'   have been changed.
#'
#' @seealso
#' * [`create_header()`] for creating headers from scratch and for a more
#'   detailed description of the LPJmL header format.
#' * [`read_header()`] for reading headers from files.
#'
#'@examples
#'\dontrun{
#' header <- create_header(
#'   name = "LPJGRID",
#'   version = 3,
#'   order = 1,
#'   firstyear = 1901,
#'   nyear = 1,
#'   firstcell = 0,
#'   ncell = 67420,
#'   nbands = 2,
#'   cellsize_lon = 0.5,
#'   scalar = 1.0,
#'   cellsize_lat = 0.5,
#'   datatype = 3,
#'   nstep = 1,
#'   timestep = 1,
#'   endian = .Platform$endian,
#'   verbose = TRUE
#' )
#'
#' header
#' $name
#' [1] "LPJGRID"
#'
#' $header
#'      version        order    firstyear        nyear    firstcell        ncell
#'          3.0          1.0       1901.0          1.0          0.0      67420.0
#'        nbands cellsize_lon       scalar cellsize_lat     datatype       nstep
#'          2.0          0.5          1.0          0.5          3.0          1.0
#'     timestep
#'          1.0
#'
#' $endian
#' [1] "little"
#'
#' # Change number of cells to 1
#' set_header_item(header = header, ncell = 1)
#' $name
#' [1] "LPJGRID"
#'
#' $header
#'      version        order    firstyear        nyear    firstcell        ncell
#'          3.0          1.0       1901.0          1.0          0.0          1.0
#'        nbands cellsize_lon       scalar cellsize_lat     datatype       nstep
#'          2.0          0.5          1.0          0.5          3.0          1.0
#'     timestep 
#'          1.0 
#'
#' $endian
#' [1] "little"
#'}
#'
#' @export
set_header_item <- function(header, ...) {
  # Check header structure. Expect a list with elements "name", "header" and
  # "endian".
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
  if (any(sapply(header[c("name", "endian")], length) != 1)) { # nolint:undesirable_function_linter.
    stop("Header has invalid structure. More than one 'name' or 'endian'")
  }
  # Expect header$header to have 13 values (some of which may be defaults)
  if (length(header$header) != 13) {
    stop("Header has invalid structure. Invalid header$header")
  }
  # Valid items that can be set in header
  valid_items <- c(
    "name", "version", "order", "firstyear", "nyear", "firstcell", "ncell",
    "nbands", "cellsize_lon", "scalar", "cellsize_lat", "datatype", "nstep",
    "timestep", "endian"
  )
  # Check that all items are present in header or header$header
  if (any(!setdiff(valid_items, names(header)) %in% names(header$header))) {
    stop(
      paste(
        "Header has invalid structure: item(s)",
        toString(
          sQuote(
            setdiff(setdiff(valid_items, names(header)), names(header$header))
          )
        ),
        "missing in header$header"
      )
    )
  }
  # Arguments provided to function
  args <- list(...)
  # Check that all arguments are in valid_items
  if (any(! names(args) %in% valid_items)) {
    stop(
      paste(
        "Invalid item(s)",
        toString(sQuote(setdiff(names(args), valid_items))),
        "provided to function.\n",
        "You can set the following header items through this function:",
        toString(sQuote(valid_items))
      )
    )
  }
  # Check that each item has been supplied no more than once
  if (any(table(names(args)) > 1)) {
    stop(
      paste(
        "You have provided the following header item(s) more than once:",
        toString(names(which(table(names(args)) > 1))), "\n",
        "Only one value per header item allowed."
      )
    )
  }
  # Check that each argument has a length of one
  if (any(sapply(args, length) != 1)) { # nolint:undesirable_function_linter.
    stop(
      paste(
        "The following item(s) contain(s) more than one value:",
        toString(sQuote(names(which(sapply(args, length) != 1)))), "\n", # nolint:undesirable_function_linter.
        "You can only provide one value for each header item."
      )
    )
  }
  # Switch on verbose output in create_header if setting name, version, or
  # datatype (these parameters can cause warnings/info prints). Otherwise,
  # suppress output of these messages.
  if (any(!sapply(args[c("name", "version", "datatype")], is.null))) { # nolint:undesirable_function_linter.
    verbose <- TRUE
  } else {
    verbose <- FALSE
  }
  tmpheader <- create_header(
    name = ifelse(is.null(args[["name"]]), header$name, args[["name"]]),
    version = ifelse(
      is.null(args[["version"]]),
      header$header["version"],
      args[["version"]]
    ),
    order = ifelse(
      is.null(args[["order"]]),
      header$header["order"],
      args[["order"]]
    ),
    firstyear = ifelse(
      is.null(args[["firstyear"]]),
      header$header["firstyear"],
      args[["firstyear"]]
    ),
    nyear = ifelse(
      is.null(args[["nyear"]]),
      header$header["nyear"],
      args[["nyear"]]
    ),
    firstcell = ifelse(
      is.null(args[["firstcell"]]),
      header$header["firstcell"],
      args[["firstcell"]]
    ),
    ncell = ifelse(
      is.null(args[["ncell"]]),
      header$header["ncell"],
      args[["ncell"]]
    ),
    nbands = ifelse(
      is.null(args[["nbands"]]),
      header$header["nbands"],
      args[["nbands"]]
    ),
    cellsize_lon = ifelse(
      is.null(args[["cellsize_lon"]]),
      header$header["cellsize_lon"],
      args[["cellsize_lon"]]
    ),
    scalar = ifelse(
      is.null(args[["scalar"]]),
      header$header["scalar"],
      args[["scalar"]]
    ),
    cellsize_lat = ifelse(
      is.null(args[["cellsize_lat"]]),
      header$header["cellsize_lat"],
      args[["cellsize_lat"]]
    ),
    datatype = ifelse(
      is.null(args[["datatype"]]),
      header$header["datatype"],
      args[["datatype"]]
    ),
    nstep = ifelse(
      is.null(args[["nstep"]]),
      header$header["nstep"],
      args[["nstep"]]
    ),
    timestep = ifelse(
      is.null(args[["timestep"]]),
      header$header["timestep"],
      args[["timestep"]]
    ),
    endian = ifelse(is.null(args[["endian"]]), header$endian, args[["endian"]]),
    verbose = verbose
  )
  tmpheader
}
