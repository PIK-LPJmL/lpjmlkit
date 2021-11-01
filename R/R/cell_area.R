#' @title Calculate cell area of LPJmL cells
#'
#' @description Calculate cell area of LPJmL cells based on
#' cell coordinates and grid resolution. Uses a spherical representation of Earth.
#'
#' @param lat Vector of cell-center latitude coordinates in degrees
#' @param res_lon Grid resolution in longitude direction in degrees (default: 0.5)
#' @param res_lat Grid resolution in latitude direction in degrees (default: same as res_lon)
#' @param earth_radius Radius of sphere (in \eqn{m}) used to calculate cell areas
#'
#' @return Vector of cell areas in \eqn{m²} corresponding to cells in `lat`
#'
#' @examples
#' grid <- matrix(data=c(-179.75, 89.75, -0.25, 0.25, 0.25, -0.25, 179.75, -89.75),
#'                ncol=2,
#'                byrow=TRUE,
#'                dimnames=list(NULL, c("lon", "lat"))
#'               )
#' gridarea <- cell_area(grid[,"lat"])
#'
#' @export
cell_area <- function(lat, res_lon=0.5, res_lat=res.lon, earth_radius=6371000.785) {
  cellwidth <- earth_radius*pi/180
  return(as.double(cellwidth*res_lon)*as.double(cellwidth*res_lat)*as.double(cos(lat/180*pi)))
}
