#' @title Calculate the cell area of LPJmL cells
#'
#' @description Calculate the cell area of LPJmL cells based on an [`LPJmLData`]
#'   object or latitude coordinates and grid resolution.
#'   Uses a spherical representation of the Earth.
#'
#' @param x `LPJmLData` object with `$grid` attribute, an LPJmLData object of
#'   variable `"grid"` (`"LPJGRID"`) or a vector of cell-center latitude
#'   coordinates in degrees.
#' @param cellsize_lon Grid resolution in longitude direction in degrees
#'   (default: `0.5`). If `x` is an LPJmLData object the resolution will be
#'   taken from the meta data included in `x` if available.
#' @param cellsize_lat Grid resolution in latitude direction in degrees (default:
#'   same as `cellsize_lon`). If `x` is an LPJmLData object the resolution will be
#'   taken from the meta data included in `x` if available.
#' @param earth_radius Radius of the sphere (in \eqn{m}) used to calculate the
#'   cell areas.
#' @param return_unit Character string describing the area unit of the returned
#'   cell areas. Defaults to `"m2"`, further options: `"ha"` or `"km2"`.
#' @return A vector or array matching the space dimension(s) of `x` if `x` is an
#'   LPJmLData object. A vector of the same length as `x` if `x` is a vector of
#'   latitude coordinates. Cell areas are returned in the unit `return_unit`.
#'
#' @examples
#' grid <- matrix(
#'   data = c(-179.75, 89.75, -0.25, 0.25, 0.25, -0.25, 179.75, -89.75),
#'   ncol = 2,
#'   byrow = TRUE,
#'   dimnames = list(NULL, c("lon", "lat"))
#' )
#' gridarea <- calc_cellarea(grid[,"lat"])
#'
#' @export
calc_cellarea <- function(x, # nolint:cyclocomp_linter.
                          cellsize_lon = 0.5,
                          cellsize_lat = cellsize_lon,
                          earth_radius = 6371000.785,
                          return_unit = "m2"
                         ) {

  # Workflow for LPJmLData objects
  if (methods::is(x, "LPJmLData") || methods::is(x, "LPJmLGridData")) {


    # Check if grid is available as attribute
    if (!is.null(x$grid)) {
      x <- x$grid

    # Handle LPJmLData objects of variable grid as LPJmLGridData objects to
    # allow for calc_cellarea
    } else if (methods::is(x, "LPJmLData") &&
        !methods::is(x, "LPJmLGridData") &&
        any(c("grid", "LPJGRID") %in% x$meta$variable)) {
      x <- LPJmLGridData$new(x) # nolint:object_usage_linter.
    }

    if (!methods::is(x, "LPJmLGridData")) {
      stop("Grid attribute is missing. Use method add_grid() to add it.")
    }

    if (!is.null(x$meta$cellsize_lon) && any(cellsize_lon != x$meta$cellsize_lon)) {
      cellsize_lon <- x$meta$cellsize_lon
      warning("Using x$meta$cellsize_lon instead of supplied cellsize_lon.")
    }

    if (!is.null(x$meta$cellsize_lat) && any(cellsize_lat != x$meta$cellsize_lat)) {
      cellsize_lat <- x$meta$cellsize_lat
      warning("Using x$meta$cellsize_lat instead of supplied cellsize_lat.")
    }

    # Check for format of space dimensions, apply different processing
    if (x$meta$._space_format_ == "cell") {
      # For format "cell" latitudes are supplied as data in band dimension
      #   ("lat")
      x <- asub(x$data, band = "lat")
    } else {

      # For format "lon_lat" latitudes are supplied by dimnames. For calculation
      #   original array is overwritten with corresponding latitudes.
      x <- check <- x$data
      dim_order <- names(dim(x))

      x <- check <- aperm(x, c("lat", setdiff(dim_order, "lat")))
      asub(x, lat = dimnames(x)$lat) <- as.numeric(dimnames(x)$lat)

      x[is.na(check)] <- NA
      x <- aperm(x, dim_order)

    }

  } else {
    # Make sure supplied vector is numeric
    x <- as.double(x)
  }

  # Check for irregular grid resolution arguments
  if (length(cellsize_lon) > 1) {
    warning("cellsize_lon has length ", length(cellsize_lon), ". Using first element.")
    cellsize_lon <- cellsize_lon[1]
  }
  if (length(cellsize_lon) == 0 || is.na(cellsize_lon)) {
    stop("Invalid longitude grid resolution 'cellsize_lon'")
  }
  cellsize_lon <- as.double(cellsize_lon)
  if (length(cellsize_lat) > 1) {
    warning("cellsize_lat has length ", length(cellsize_lat), ". Using first element.")
    cellsize_lat <- cellsize_lat[1]
  }
  if (length(cellsize_lat) == 0 || is.na(cellsize_lat)) {
    stop("Invalid latitude grid resolution 'cellsize_lat'")
  }
  cellsize_lat <- as.double(cellsize_lat)

  # Check for irregular latitude coordinates
  if (any(x < -90 | x > 90, na.rm = TRUE)) {
    stop("Invalid latitude values in 'x'. Values must be within +- 90 degrees")
  }

  cellwidth <- earth_radius * pi / 180

  cellwidth * cellsize_lon * cellwidth * cellsize_lat * cos(x / 180 * pi) %>%

    # Apply conversion factor based on return_unit parameter
    switch(return_unit,
           m2 = .,
           ha = . * 1e-4,
           km2 = . * 1e-6,
           stop("Unsupported return_unit ", dQuote(return_unit))) %>%
    return()
}
