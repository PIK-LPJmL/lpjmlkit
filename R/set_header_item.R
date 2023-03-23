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
#'   'endian'. Parameter 'verbose' can be used to control verbosity, as in
#'   [`create_header()`].
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
#' # $name
#' # [1] "LPJGRID"
#' #
#' # $header
#' #      version        order    firstyear        nyear    firstcell        ncell
#' #          3.0          1.0       1901.0          1.0          0.0      67420.0
#' #        nbands cellsize_lon       scalar cellsize_lat     datatype       nstep
#' #          2.0          0.5          1.0          0.5          3.0          1.0
#' #     timestep
#' #          1.0
#' #
#' # $endian
#' # [1] "little"
#'
#' # Change number of cells to 1
#' set_header_item(header = header, ncell = 1)
#' # $name
#' # [1] "LPJGRID"
#' #
#' # $header
#' #      version        order    firstyear        nyear    firstcell        ncell
#' #          3.0          1.0       1901.0          1.0          0.0          1.0
#' #        nbands cellsize_lon       scalar cellsize_lat     datatype       nstep
#' #          2.0          0.5          1.0          0.5          3.0          1.0
#' #     timestep
#' #          1.0
#' #
#' # $endian
#' # [1] "little"
#'
#' @export
set_header_item <- function(header, ...) {
  # Check header structure.
  is_valid_header(header)

  # Arguments provided to function
  args <- list(...)
  # Check that all arguments are in valid_header_items
  if (any(! names(args) %in% c(valid_header_items, "verbose"))) {
    stop(
      paste(
        "Invalid item(s)",
        toString(sQuote(setdiff(names(args), valid_header_items))),
        "provided to function.\n",
        "You can set the following header items through this function:",
        toString(sQuote(valid_header_items))
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
    verbose <- default(args[["verbose"]], TRUE)
  } else {
    verbose <- default(args[["verbose"]], FALSE)
  }
  tmpheader <- create_header(
    name = ifelse(is.null(args[["name"]]), header$name, args[["name"]]),
    version = default(args[["version"]], header$header["version"]),
    order = default(args[["order"]], header$header[["order"]]),
    firstyear = default(args[["firstyear"]], header$header["firstyear"]),
    nyear = default(args[["nyear"]], header$header["nyear"]),
    firstcell = default(args[["firstcell"]], header$header["firstcell"]),
    ncell = default(args[["ncell"]], header$header["ncell"]),
    nbands = default(args[["nbands"]], header$header["nbands"]),
    cellsize_lon = default(
      args[["cellsize_lon"]],
      header$header["cellsize_lon"]
    ),
    scalar = default(args[["scalar"]], header$header["scalar"]),
    cellsize_lat = default(
      args[["cellsize_lat"]],
      header$header["cellsize_lat"]
    ),
    datatype = default(args[["datatype"]], header$header["datatype"]),
    nstep = default(args[["nstep"]], header$header["nstep"]),
    timestep = default(args[["timestep"]], header$header["timestep"]),
    endian = default(args[["endian"]], header$endian),
    verbose = verbose
  )
  tmpheader
}
