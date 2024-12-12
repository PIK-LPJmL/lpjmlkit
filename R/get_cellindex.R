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
#' @param extent A numeric vector (lonmin, lonmax, latmin, latmax) containing the
#'  longitude and latitude boundaries between which values included in the subset.
#' @param coordinates A list of two named (lon, lat) numeric vectors representing the coordinates.
#' @param shape A terra SpatVector object in the WGS 84 coordinate reference system
#' representing the shape to subset the grid.
#' @param simplify A logical indicating whether to simplify the output to a vector.
#'
#'
#' @return Either an LPJmLData object containing the grid or a vector subsetted
#' to the provided extent, coordinates or shape.
#' @export
#'
#' @examples
#' \dontrun{
#' get_cellindex(
#'   grid_filename = "my_grid.bin.json",
#'   extent = c(-123.25, -122.75, 49.25, 49.75) # (lonmin, lonmax, latmin, latmax)
#' )
#' get_cellindex(
#'   grid_filename = "my_grid.bin.json",
#'   coordinates = list(lon = c(-123.25, -122.75), lat = c(49.25, 49.75))
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
#' should be a list of two character vectors representing the longitude and
#' latitude values as for [`subset()`].
#'
#' If a shape is provided as a SpatVector object, the function will return the
#' cell index for the cells that intersect with the shape.
#'
#' If more than on of `extent`, `coordinates` `shape` are provided, the function
#' will stop and ask for only one of them. If neither `extent` nor `coordinates`
#' nor `shape` are provided, the function will return the cell numbers for all
#' cells in the grid.
#'
#' The function also includes checks for input types and values, and gives
#' specific error messages for different error conditions. For example, it
#' checks if the `grid_filename` exists, if the `extent` vector has the correct
#' length, and if the `coordinates` list contains two vectors of equal length.
get_cellindex <- function(grid_filename, extent = NULL, coordinates = NULL, shape = NULL, simplify = TRUE) {
  # check if filepath is valid
  check_filepath(grid_filename)
  # check if (only) one of extent or coordinates is provided
  check_multiple(extent, coordinates, shape)

  grid_lonlat <- read_io(filename = grid_filename) |>
    LPJmLGridData$new()

  if (!is.null(extent)) {
    extent <- check_extent(extent) |>
      correct_extent()
  } else if (!is.null(coordinates)) {
    coordinates <- check_coordinates(coordinates)
  } else if (!is.null(shape)) {
    if (!("SpatVector" %in% class(shape))) {
      stop("shape must be a SpatVector object.")
    }
  }

  # Read the grid file and create a data frame
  cells <- as.data.frame(grid_lonlat$data)

  # Get the range of longitude and latitude in the cells
  lon_range <- range(cells$lon)
  lat_range <- range(cells$lat)

  # Check if extent values are within the longitude and latitude range in the cells
  if (!is.null(extent)) {
    cells$cellindex <- as.numeric(row.names(cells)) + 1

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

    cells <- cells[cells$lon >= extent[1] &
                     cells$lon <= extent[2] &
                     cells$lat >= extent[3] & cells$lat <= extent[4], ]

    if (!simplify) {
      grid_cell <- transform(grid_lonlat, "lon_lat")

      cells <- grid_cell$subset(coordinates = lapply(X = list(lon = cells$lon,
                                                              lat = cells$lat),
                                                     FUN = as.character))
    } else {
      cells <- cells$cellindex
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
    if (any(out_of_bounds)) {
      warning(paste(
        "Coordinates out of bounds:",
        paste(out_of_bounds_coords, collapse = ", ")
      ))
    }

    grid_cell <- transform(grid_lonlat, "lon_lat")

    cells <- grid_cell$subset(coordinates = lapply(X = coordinates,
                                                   FUN = as.character))

    message(
      col_note(
        "Note: Possible duplicates of cell ids are removed."
      )
    )
  }

  if (!is.null(shape)) {
    grid_lonlat <- grid_lonlat$transform("lon_lat")

    cell_coords <- grid_lonlat |>
      as_terra() |>
      terra::mask(shape) |>
      terra::as.data.frame(xy = TRUE) |>
      dplyr::select("x", "y")

    cells <- grid_lonlat |>
      subset(coordinates = lapply(list(lon = cell_coords$x, lat = cell_coords$y),
                                  FUN = as.character))
  }

  if (simplify && is.null(extent)) {
    cells <- c(stats::na.omit(c(cells$data + 1)))
  }

  cells
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

  if (!is.null(extent) && (length(extent) != 4)) {
    stop("extent must be a numeric vector of length 4.")
  }

  # check if character vector present and convert to numeric (to be consistent with coordinates) # nolint
  if (is.character(extent)) {
    extent <- as.numeric(extent)
  }

  # Check if any NA's are present in extent
  if (any(is.na(extent))) {
    stop("NA's (induced by conversion) are not allowed in extent.")
  }

  extent
}



# Check if both extent and coordinates are provided
check_multiple <- function(extent, coordinates, shape) {
  if (is.null(extent) && is.null(coordinates) && is.null(shape)) {
    warning("Neither extent, coordinates or shape provided. Full grid will be returned.")
  }
  if ((!is.null(extent) && !is.null(coordinates)) ||
        (!is.null(extent) && !is.null(shape)) ||
        (!is.null(coordinates) && !is.null(shape))) {
    stop(
      "Multiple subset options provided.",
      " Please provide only one of coordinates, extent and shape."
    )
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

  extent
}

# Check if the coordinates are a list of two numeric vectors and of equal length
check_coordinates <- function(coordinates) {
  if (!is.list(coordinates) || length(coordinates) != 2) {
    stop("coordinates must be a list of two vectors.")
  }

  coordinates <- lapply(coordinates, function(coord) {
    if (!is.numeric(coord)) {
      warning("Non-numeric coordinates detected, attempting to convert to numeric.")
      coord <- as.numeric(coord)
      if (any(is.na(coord))) {
        stop("Unable to convert all coordinates to numeric.")
      }
    }
    coord
  })

  if (length(coordinates[[1]]) != length(coordinates[[2]])) {
    stop("The two vectors in coordinates must have the same length.")
  }

  coordinates
}
