#' @title Calculate cell area of LPJmL cells
#'
#' @description Calculate cell area of LPJmL cells based on
#' cell coordinates and grid resolution. Uses a spherical representation of Earth.
#'
#' @param lat Vector of cell-center latitude coordinates in degrees
#' @param res.lon Grid resolution in longitude direction in degrees (default: 0.5)
#' @param res.lat Grid resolution in latitude direction in degrees (default: same as res.lon)
#' @param earth.radius Radius of sphere (in \eqn{m}) used to calculate cell areas
#'
#' @return Vector of cell areas in \eqn{m²} corresponding to cells in `lat`
#'
#' @examples
#' grid <- matrix(data=c(-179.75, 89.75, -0.25, 0.25, 0.25, -0.25, 179.75, -89.75),
#'                ncol=2,
#'                byrow=TRUE,
#'                dimnames=list(NULL, c("lon", "lat"))
#'               )
#' gridarea <- cellArea(grid[,"lat"])
#'
#' @export
cellArea <- function(lat, res.lon=0.5, res.lat=res.lon, earth.radius=6371000.785) {
  cellwidth <- earth.radius*pi/180
  return(as.double(cellwidth*res.lon)*as.double(cellwidth*res.lat)*as.double(cos(lat/180*pi)))
}
