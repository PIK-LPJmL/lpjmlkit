#' @title Calculate cell area of LPJmL cells
#'
#' @description Calculate cell area of LPJmL cells based on a LPJmLData object
#' or cell coordinates and grid resolution.
#' Uses a spherical representation of Earth.
#'
#' @param x `LPJmLData` object with `$grid` attribute, a LPJmLData object of
#' variable `"grid"` (`"LPJ_GRID"`) or vector of cell-center latitude
#' coordinates in degrees.
#' @param res_lon Grid resolution in longitude direction in degrees
#'   (default: 0.5). If x LPJmLData` object it will be ignored if different.
#' @param res_lat Grid resolution in latitude direction in degrees (default:
#'   same as res_lon). If x LPJmLData` object it will be ignored if different.
#' @param unit Character string, defaults to `"m2"`. Further available options
#' are `"ha"` or `"km2"`
#' @return Either if x is an LPJmLData object a vector or array matching the
#' space dimension(s) of this object or if x is a latitude vector a vector of
#' cell areas in \eqn{m^2} corresponding to cells in `lat` is returned.
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
                          unit = "m2"
                         ) {
  # worklfow for LPJmLData objects
  if (methods::is(x, "LPJmLData")) {
    # check if grid is available as attribute
    if (!is.null(x$grid)) {
      x <- x$grid
    # check if LPJmLData object is of variable grid or LPJ_GRID (header file)
    } else if (!any(c("grid", "LPJGRID") %in% x$meta$variable)) {
      stop("Grid attribute is missing. Use method add_grid() to add it.")
    }
    if (res_lon != x$meta$cellsize_lon) {
      res_lon <- x$meta$cellsize_lon
      warning("res_lon is ignored, instead cellsize_lon of x$meta is used.")
    }
    if (res_lat != x$meta$cellsize_lat) {
      res_lat <- x$meta$cellsize_lat
      warning("res_lat is ignored, instead cellsize_lat of x$meta is used.")
    }
    # check for format of space dimensions apply different processing
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
    # make sure supplied vector is numeric (lat dimnames of LPJmLData are
    #   character string)
    x <- as.double(x)
  }
  cellwidth <- earth_radius * pi / 180
  if (any(x < -90 | x > 90, na.rm = TRUE)) {
    stop("Invalid latitude values in lat. Values must be within +- 90 degrees")
  }
  cellwidth * res_lon * cellwidth * res_lat * cos(x / 180 * pi) %>%
    # for each unit possiblity apply conversion factor
    switch(unit,
           m2 = .,
           ha = . * 1e-4,
           km2 = . * 1e-6) %>%
    return()
}
