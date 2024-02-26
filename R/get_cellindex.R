#' @title Get Cell Index
#'
#' @description This function returns the cell index from a grid file based on
#' the provided extent or coordinates. If neither extent nor coordinates are
#' provided, the full grid will be returned. If both extent and coordinates are
#' provided, the function will stop and ask for only one of them. The extent
#' should be a vector of length 4 in the form c(lonmin, lonmax, latmin, latmax).
#' If the extent is not in the correct form, the function will swap the values
#' to correct it.
#'
#' @param grid_filename A string representing the grid file name.
#' @param extent A numeric vector of length 4 representing the extent
#' (lonmin, lonmax, latmin, latmax).
#' @param coordinates A list of two numeric vectors representing the coordinates.
#'
#'
#' @return The cell index from the grid file based on the provided extent or
#' coordinates.
#' @export
#'
#' @examples
#' \dontrun{
#' get_cellindex(
#'   grid_filename = "my_grid.bin.json",
#'   extent = c(-123.25, -122.75, 49.25, 49.75)
#' )
#' get_cellindex(
#'   grid_filename = "my_grid.bin.json",
#'   coordinates = list(c(-123.25, -122.75), c(49.25, 49.75))
#' )
#' }
#' @details
#' The function reads a grid file specified by `grid_filename` and creates a
#' data frame with columns for longitude, latitude, and cell number. The cell
#' number is a sequence from 1 to the number of rows in the data frame.
#'
#' If an `extent` is provided, the function filters the cells to include only
#' those within the specified longitude and latitude range. The `extent` should
#' be a numeric vector of length 4 in the form c(lonmin, lonmax, latmin, latmax).
#'
#' If a list of `coordinates` is provided, the function filters the cells to
#' include only those that match the specified coordinates. The `coordinates`
#' should be a list of two numeric vectors representing the longitude and
#' latitude values.
#'
#' If both `extent` and `coordinates` are provided, the function will stop and
#' ask for only one of them. If neither `extent` nor `coordinates` are provided,
#' the function will return the cell numbers for all cells in the grid.
#'
#' The function also includes checks for input types and values, and gives
#' specific error messages for different error conditions. For example, it
#' checks if the `grid_filename` exists, if the `extent` vector has the correct
#' length, and if the `coordinates` list contains two vectors of equal length.
get_cellindex <- function(grid_filename, extent = NULL, coordinates = NULL) {
  # Check input types and values
  check_filepath(grid_filename)
  check_extent(extent)
  check_coordinates(coordinates)
  check_extent_and_coordinates(extent, coordinates)
  extent <- correct_extent(extent)
  check_coordinates_length(coordinates)

  # Read the grid file and create a data frame
  cells <- as.data.frame(read_io(filename = grid_filename)$data)
  colnames(cells) <- c("lon", "lat")
  cells$cellnumber <- seq_len(nrow(cells))

  # Get the range of longitude and latitude in the cells
  lon_range <- range(cells$lon)
  lat_range <- range(cells$lat)

  # Check if extent values are within the longitude and latitude range in the cells
  if (!is.null(extent)) {
    out_of_bounds_lon <- extent[c(1, 2)][extent[c(1, 2)] < lon_range[1] |
                                           extent[c(1, 2)] > lon_range[2]]
    out_of_bounds_lat <- extent[c(3, 4)][extent[c(3, 4)] < lat_range[1] |
                                           extent[c(3, 4)] > lat_range[2]]

    if (length(out_of_bounds_lon) > 0 || length(out_of_bounds_lat) > 0) {
      warning(paste(
        "Extent values out of bounds: longitude",
        paste(out_of_bounds_lon, collapse = ", "),
        "latitude",
        paste(out_of_bounds_lat, collapse = ", ")
      ))
    }
  }

  # Check if coordinates are within the longitude and latitude range in the cells
  if (!is.null(coordinates)) {
    out_of_bounds <- coordinates[[1]] < lon_range[1] |
      coordinates[[1]] > lon_range[2] |
      coordinates[[2]] < lat_range[1] |
      coordinates[[2]] > lat_range[2]

    out_of_bounds_coords <- paste0(
      "(",
      coordinates[[1]][out_of_bounds],
      ", ",
      coordinates[[2]][out_of_bounds],
      ")"
    )
  }
  if (length(out_of_bounds_coords) > 0) {
    warning(paste(
      "Coordinates out of bounds:",
      paste(out_of_bounds_coords, collapse = ", ")
    ))
  }

  # Filter cells based on extent
  if (!is.null(extent)) {
    cells <- cells[cells$lon >= extent[1] &
                     cells$lon <= extent[2] &
                     cells$lat >= extent[3] & cells$lat <= extent[4], ]
  }

  # Filter cells based on coordinates
  if (!is.null(coordinates)) {
    coord_df <- data.frame(
      lon = unlist(coordinates[1]),
      lat = unlist(coordinates[2])
    )
    cells <- cells[match(
      paste(cells$lon, cells$lat),
      paste(coord_df$lon, coord_df$lat)
    ), ]
  }

  # Return the filtered cellindexes
  cells$cellnumber
}

# Check if the input is a valid file path
check_filepath <- function(grid_filename) {
  if (!is.character(grid_filename) || grid_filename == "") {
    stop("grid_filename must be a string representing a valid file path.")
  }
  if (!file.exists(grid_filename)) {
    stop("grid_filename does not exist.")
  }
}

# Check if the extent is a numeric vector of length 4
check_extent <- function(extent) {
  if (!is.null(extent) && (length(extent) != 4 || !is.numeric(extent))) {
    stop("extent must be a numeric vector of length 4.")
  }
}

# Check if the coordinates are a list of two numeric vectors of equal length
check_coordinates <- function(coordinates) {
  if (!is.null(coordinates) &&
        (!is.list(coordinates) || length(coordinates) != 2 ||
           !is.numeric(coordinates[[1]]) || !is.numeric(coordinates[[2]]) ||
           length(coordinates[[1]]) != length(coordinates[[2]]))) {
    stop("coordinates must be a list of two numeric vectors of equal length.")
  }
}

# Check if both extent and coordinates are provided
check_extent_and_coordinates <- function(extent, coordinates) {
  if (is.null(extent) && is.null(coordinates)) {
    warning("Neither extent or coordinates provided. Full grid will be returned.")
  }
  if (!is.null(extent) && !is.null(coordinates)) {
    stop(paste(
      "Both extent and coordinates are provided.",
      " Please provide only one of them."
    ))
  }
}

# Check and correct the extent if necessary
correct_extent <- function(extent) {
  if (extent[1] > extent[2] || extent[3] > extent[4]) {
    warning("Extent should be in the form c(lonmin, lonmax, latmin, latmax).")
    if (extent[1] > extent[2]) {
      extent[1:2] <- sort(extent[1:2])
      warning("Swapped values of lonmin and lonmax.")
    }
    if (extent[3] > extent[4]) {
      extent[3:4] <- sort(extent[3:4])
      warning("Swapped values of latmin and latmax.")
    }
  }
  return(extent)
}

# Check the length of coordinates
check_coordinates_length <- function(coordinates) {
  if (length(coordinates[[1]]) != length(coordinates[[2]])) {
    stop("The two vectors in coordinates must have the same length.")
  }
}
