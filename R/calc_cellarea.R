#' @title Calculate cell area of LPJmL cells
#'
#' @description Calculate cell area of LPJmL cells based on a LPJmLData object
#' or cell coordinates and grid resolution.
#' Uses a spherical representation of Earth.
#'
#' @param x `LPJmLData` object with `$grid` attribute, a LPJmLData object of
#' variable `"grid"` (`"LPJGRID"`) or vector of cell-center latitude
#' coordinates in degrees.
#' @param res_lon Grid resolution in longitude direction in degrees
#'   (default: 0.5). If `x` is a LPJmLData object the resolution will be taken
#'   from the meta data included in `x`.
#' @param res_lat Grid resolution in latitude direction in degrees (default:
#'   same as res_lon). If `x` is a LPJmLData` object the resolution will be
#'   taken from the meta data included in `x`.
#' @param earth_radius Radius of sphere (in \eqn{m}) used to calculate cell
#'   areas.
#' @param return_unit Character string describing the area unit of the returned
#'   cell areas. Defaults to `"m2"`, further options: `"ha"` or `"km2"`.
#' @return A vector or array matching the space dimension(s) of `x` if `x` is a
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
calc_cellarea <- function(x,
                          res_lon = 0.5,
                          res_lat = res_lon,
                          earth_radius = 6371000.785,
                          return_unit = "m2"
                         ) {
  # workflow for LPJmLData objects
  if (methods::is(x, "LPJmLData")) {

    # check if grid is available as attribute
    if (!is.null(x$grid)) {
      x <- x$grid

    # check if LPJmLData object is of variable grid or LPJ_GRID (header file)
    } else if (!any(c("grid", "LPJGRID") %in% x$meta$variable)) {
      stop("Grid attribute is missing. Use method add_grid() to add it.")
    }

    if (!is.null(x$meta$cellsize_lon) && res_lon != x$meta$cellsize_lon) {
      res_lon <- x$meta$cellsize_lon
      warning("Using x$meta$cellsize_lon instead of supplied res_lon.")
    }

    if (!is.null(x$meta$cellsize_lat) && res_lat != x$meta$cellsize_lat) {
      res_lat <- x$meta$cellsize_lat
      warning("Using x$meta$cellsize_lat instead of supplied res_lat.")
    }

    # check for format of space dimensions, apply different processing
    if (x$meta$._space_format_ == "cell") {
      # for format "cell" latitudes are supplied as data in band dimension
      #   ("lat")
      x <- asub(x$data, band = "lat")
    } else {

      # for format "lon_lat" latitudes are supplied by dimnames, for calculation
      #   original array is overwritten with corresponding latitudes
      x <- check <- x$data
      asub(x, lat = dimnames(x)$lat) <- as.numeric(dimnames(x)$lat)
      x[is.na(check)] <- NA
    }

  } else {
    # make sure supplied vector is numeric
    x <- as.double(x)
  }

  cellwidth <- earth_radius * pi / 180

  if (any(x < -90 | x > 90, na.rm = TRUE)) {
    stop("Invalid latitude values in 'x'. Values must be within +- 90 degrees")
  }

  cellwidth * res_lon * cellwidth * res_lat * cos(x / 180 * pi) %>%

    # apply conversion factor based on return_unit parameter
    switch(return_unit,
           m2 = .,
           ha = . * 1e-4,
           km2 = . * 1e-6,
           stop("Unsupported return_unit ", dQuote(return_unit))) %>%
    return()
}
