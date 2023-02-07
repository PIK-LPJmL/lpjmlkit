#' Transform an LPJmLData object
#'
#' Function to transform an [`LPJmLData`] data object into another
#' space or another time format. Combinations of space and time formats are also
#' possible.
#'
#' @param x An [LPJmLData] object.
#'
#' @param to A character vector defining space and/or time format into which
#'   the corresponding data dimensions should be transformed. Choose from space
#'   formats `c("cell", "lon_lat")` and time formats
#'   `c("time","year_month_day")`.
#'
#' @return An [`LPJmLData`] object in the selected format.
#'
#' @examples
#' \dontrun{
#'
#' runoff <- read_io(filename = "runoff.bin.json"),
#'                   subset = list(year = 1991:2000))
#'
#' # Transform into space format "lon_lat". This assumes a "grid.bin.json" file
#' # is present in the same directory as "runoff.bin.json".
#' transform(runoff, to = "lon_lat")
#' # [...]
#' # $data %>%
#' #   dimnames() %>%
#' #     .$lat  "-55.75" "-55.25" "-54.75" "-54.25" ... "83.75"
#' #     .$lon  "-179.75" "-179.25" "-178.75" "-178.25" ... "179.75"
#' #     .$time  "1991-01-31" "1991-02-28" "1991-03-31" "1991-04-30" ...
#' #     .$band  "1"
#' # [...]
#'
#' # Transform time format from a single time dimension into separate dimensions
#' # for years, months, and days. Dimensions for time steps not present in the
#' # data are omitted, i.e. no "day" dimension for monthly data.
#' transform(runoff, to = "year_month_day")
#' # [...]
#' # $data %>%
#' #   dimnames() %>%
#' #     .$lat  "-55.75" "-55.25" "-54.75" "-54.25" ... "83.75"
#' #     .$lon  "-179.75" "-179.25" "-178.75" "-178.25" ... "179.75"
#' #     .$month  "1" "2" "3" "4" ... "12"
#' #     .$year  "1991" "1992" "1993" "1994" ... "2000"
#' #     .$band  "1"
#' # [...]
#' }
#'
#' @md
#' @export
transform <- function(x,
                      to) {
  y <- x$clone(deep = TRUE)
  y$transform(to)
  y
}

# transform method roxygen documentation in LPJmlData.R
LPJmLData$set("private",
              ".transform",
              function(to) {

    # Check for locked objects
    check_method_locked(self, "transform")

    # Transform time format
    if (any(to %in% private$.meta$._dimension_map_$time)) {
      private$transform_time(to = "time")
      to <- to[!to %in% private$.meta$._dimension_map_$time]
    } else if (any(to %in% private$.meta$._dimension_map_$year_month_day)) {
      private$transform_time(to = "year_month_day")
      to <- to[!to %in% private$.meta$._dimension_map_$year_month_day]
    }

    # Transform space format
    if (any(to %in% private$.meta$._dimension_map_$cell)) {
      private$transform_space(to = "cell")
      to <- to[!to %in% private$.meta$._dimension_map_$cell]
    } else if (any(to %in% private$.meta$._dimension_map_$lon_lat)) {
      private$transform_space(to = "lon_lat")
      to <- to[!to %in% private$.meta$._dimension_map_$lon_lat]
    }

    if (length(to) > 0) {
      stop(
        paste0(
          "\u001b[0m",
          ifelse(length(to) > 1, "Formats ", "Format "),
          "\u001b[34m",
          paste0(to, collapse = ", "),
          "\u001b[0m",
          ifelse(length(to) > 1, " are ", " is "),
          "not valid. Please choose from available space formats ",
          "\u001b[34m",
          paste0(private$.meta$._dimension_map_$space_format, collapse = ", "),
          "\u001b[0m",
          " and available time formats ",
          "\u001b[34m",
          paste0(private$.meta$._dimension_map_$time_format, collapse = ", "),
          "\u001b[0m."
        ),
        call. = FALSE
      )
    }

    return(invisible(self))
  }
)

# Method to transform the dimensions of a grid array from "cell" to "lon_lat"
# or the other way around
LPJmLData$set("private",
              ".transform_grid",
              function(to = NULL) {
    if (is.null(private$.meta$variable) ||
        private$.meta$variable != "grid") {
      stop(paste("not legit for variable", private$.meta$variable))
    }

    # Convenience function - if to == NULL automatically switch to other to
    if (is.null(to)) {
      if (private$.meta$._space_format_ == "cell") {
        to <- "lon_lat"
      } else {
        to <- "cell"
      }
    }

    # Case 1: Transformation from cell dimension to lon, lat dimensions
    if (private$.meta$._space_format_ == "cell" &&
        to == "lon_lat") {

      # calculate grid extent from range to span raster
      grid_extent <- apply(
          self$data,
          "band",
          range
      )

      # Calculate dimnames for full 2 dimensional grid
      spatial_dimnames <- mapply(seq, # nolint:undesirable_function_linter.
                                 grid_extent[1, c("lat", "lon")],
                                 grid_extent[2, c("lat", "lon")] +
                                   c(private$.meta$cellsize_lat,
                                     private$.meta$cellsize_lon) / 2,
                                 by = c(private$.meta$cellsize_lat,
                                        private$.meta$cellsize_lon),
                                 SIMPLIFY = FALSE)

      # Initialize grid array
      grid_array <- array(NA,
                          dim = lapply(spatial_dimnames, length),
                          dimnames = spatial_dimnames)

      # Get indices of lat and lon dimnames
      ilon <- round((asub(self$data, band = "lon") -
        min(grid_extent[, "lon"])) / private$.meta$cellsize_lon) + 1
      ilat <- round((asub(self$data, band = "lat") -
        min(grid_extent[, "lat"])) / private$.meta$cellsize_lat) + 1

      # Set dimnames of grid_array to actual coordinates instead of dummy
      # spatial_dimnames
      coordlon <- asub(self$data, band = "lon")[
        match(seq_len(dim(grid_array)["lon"]), ilon)
      ]
      dimnames(grid_array)$lon <- ifelse(
        is.na(coordlon),
        dimnames(grid_array)$lon,
        coordlon
      )
      coordlat <- asub(self$data, band = "lat")[
        match(seq_len(dim(grid_array)["lat"]), ilat)
      ]
      dimnames(grid_array)$lat <- ifelse(
        is.na(coordlat),
        dimnames(grid_array)$lat,
        coordlat
      )

      # Replace cell of lon and lat by cell index
      grid_array[cbind(ilat, ilon)] <- as.integer(
        dimnames(self$data)$cell
      )
      self$.__set_data__(grid_array)
      private$.meta$.__transform_space_format__("lon_lat")

    # Case 2: Transformation from lon, lat dimensions to cell dimension
    } else if (private$.meta$._space_format_ == "lon_lat" &&
        to == "cell") {

      # Get indices of actual cells
      grid_indices <- which(!is.na(self$data), arr.ind = TRUE)
      grid_dimnames <- lapply(dimnames(self$data), as.numeric)

      # Set values to latitude and longitude coordinates and dimnames to cell
      # indices
      self$.__set_data__(
        array(
          cbind(
            lon = grid_dimnames[["lon"]][
              grid_indices[, which(names(dim(self$data)) == "lon")]
            ],
            lat = grid_dimnames[["lat"]][
              grid_indices[, which(names(dim(self$data)) == "lat")]
            ]
          ),
          dim = c(cell = length(self$data[grid_indices]),
                  band = 2),
          dimnames = list(cell = self$data[grid_indices],
                          band = c("lon", "lat"))
        )
      )

      private$.meta$.__transform_space_format__("cell")
    }

    return(invisible(self))
  }
)


# Method to transform space dimension of data array from "cell" to "lon_lat" or
#   the other way around. If required add_grid to LPJmLData along the way.
LPJmLData$set("private",
              "transform_space",
              function(to = NULL) {

    # Use transform_grid method on the object itself if it is a grid
    if (!is.null(private$.meta$variable) &&
        private$.meta$variable == "grid") {
      self$.__transform_grid__(to = to)
      return(invisible(self))
    }

    # Check for locked objects
    check_method_locked(self, "transform_space")

    # Convenience function - if to == NULL automatically switch to other to
    if (is.null(to)) {
      if (private$.meta$._space_format_ == "cell") {
        to <- "lon_lat"
      } else {
        to <- "cell"
      }
    } else {
      # If to equals current format return directly
      if (private$.meta$._space_format_ == to) {
        return(invisible(self))
      }
    }

    # Support lazy loading of grid for meta files. This throws an error if no
    # suitable grid file is detected.
    if (is.null(private$.grid)) {
      self$add_grid()
    }

    # Create new data array based on disaggregated time dimension
    other_dimnames <- dimnames(self$data) %>%
      `[<-`(unlist(strsplit(private$.meta$._space_format_, "_")), NULL)
    other_dims <- dim(self$data) %>%
      `[`(names(other_dimnames))

    # Case 1: Transformation from cell dimension to lon, lat dimensions
    if (private$.meta$._space_format_ == "cell" &&
        to == "lon_lat") {

      private$.grid$.__transform_grid__(to = to)

      # Append new space dimension where they have been before
      new_dimnames <- append(other_dimnames,
                             values = dimnames(private$.grid$data)[c("lat", "lon")], # nolint
                             after = get_predim(self$data, "cell"))
      new_dims <- append(other_dims,
                         values = dim(private$.grid$data)[c("lat", "lon")],
                         after = get_predim(self$data, "cell"))

      # fit_grid recycles data if necessary to fit into the array
      data_array <- fit_grid(self$data,
                             private$.grid$data,
                             "cell") %>%
        # Assign data to transformed array by grid
        array(dim = new_dims,
              dimnames = new_dimnames)

      data_array[!is.na(data_array)] <- self$data

      # Set corresponding meta data entry
      private$.meta$.__transform_space_format__("lon_lat")

    # Case 2: Transformation between lon, lat dimensions and cell dimension
    } else if (private$.meta$._space_format_ == "lon_lat" &&
        to == "cell") {


      # fit_grid recycles data if necessary to fit into the array
      mask_array <- fit_grid(self$data,
                             private$.grid$data,
                             c("lon", "lat")) %>%
        # Create array for masking data
        array(dim = dim(self$data),
              dimnames = dimnames(self$data))

      private$.grid$.__transform_grid__(to = to)

      # append new space dimension where they have been before
      new_dimnames <- append(other_dimnames,
                             values = dimnames(private$.grid$data)["cell"],
                             after = get_predim(self$data, c("lon", "lat")))
      new_dims <- append(other_dims,
                         values = dim(private$.grid$data)["cell"],
                         after = get_predim(self$data, c("lon", "lat")))

      # Assign data to transformed array by grid
      data_array <- array(NA,
                          dim = new_dims,
                          dimnames = new_dimnames)

      data_array[] <- self$data[!is.na(mask_array)]

      # Set corresponding meta data entry
      private$.meta$.__transform_space_format__("cell")

      if (!is.null(private$.grid)) {
        private$.grid$meta$.__transform_space_format__("cell")
      }

    } else {
      return(invisible(self))
    }
    # Overwrite internal data with same data but new dimensions
    self$.__set_data__(data_array)

    return(invisible(self))
  }
)


# Method to transform the time dimension of the data array from "time" to
# "year_month_cell" or the other way around
LPJmLData$set("private",
              "transform_time",
              function(to = NULL) {

    # Special case: LPJmLData objects containing "grid" variable do not have any
    # time information. Fail if attempting to switch time format.
    if (!is.null(private$.meta$variable) &&
        private$.meta$variable == "grid") {
      stop(paste("not legit for variable", dQuote(private$.meta$variable)))
    }

    # Check for locked objects
    check_method_locked(self, "transform_time")

    # Convenience function - if to == NULL automatically switch to other to
    if (is.null(to)) {
      if (private$.meta$._time_format_ == "time") {
        to <- "year_month_day"
      } else {
        to <- "time"
      }
    } else {
      # If to equals current format return directly
      if (private$.meta$._time_format_ == to) {
        return(invisible(self))
      }
    }

    # Case 1: Transformation from "time" dimension to "year", "month", "day"
    #   dimensions (if available)
    if (private$.meta$._time_format_ == "time" &&
        to == "year_month_day") {

      # Possible ndays of months
      ndays_in_month <- c(31, 30, 28)

      # Split time string "year-month-day" into year, month, day integer vector,
      #   reverse it to get it into the right order for array conversion
      time_dimnames <- split_time_names(self$dimnames()[["time"]]) %>% rev()

      # Remove day dimension if all entries fall on last day of month
      if (all(time_dimnames[["day"]] %in% ndays_in_month)) {
        time_dimnames[["day"]] <- NULL
      }

      # Remove month dimension if there is only one month in data -> annual
      if (length(time_dimnames$month) == 1 &&
          is.null(time_dimnames[["day"]])) {
        time_dimnames[["month"]] <- NULL
      }

      private$.meta$.__transform_time_format__("year_month_day")

    # Case 2: Transformation from dimensions "year", "month", "day"
    # (if available) to "time" dimension
    } else if (private$.meta$._time_format_ == "year_month_day" &&
               to == "time") {

      # Convert time dimnames back to time
      pre_dimnames <- self$dimnames() %>%
        lapply(as.integer) %>%
        suppressWarnings()

      time_dimnames <- list(
        time = create_time_names(nstep = private$.meta$nstep,
                                 years = pre_dimnames$year,
                                 months = pre_dimnames$month,
                                 days = pre_dimnames$day)
      )

      private$.meta$.__transform_time_format__("time")

    # Return directly if no transformation is necessary
    } else {
      return(invisible(self))
    }

    spatial_dims <- unlist(strsplit(private$.meta$._space_format_, "_"))

    # Create new data array based on disaggregated time dimension
    time_dims <- lapply(time_dimnames, length)
    self$.__set_data__(
      array(
        self$data,
        dim = c(dim(self$data)[spatial_dims],
                time_dims,
                dim(self$data)["band"]),
        dimnames = do.call(list,
                           args = c(dimnames(self$data)[spatial_dims],
                                    time_dimnames,
                                    dimnames(self$data)["band"]))
      )
    )

    return(invisible(self))
  }
)


# Function to recycle grid if necessary if array where data should be assigned
# to has space dimension not as the first dimension(s)
fit_grid <- function(x, grid, space_dim) {

  # Get position of last dimension before first space dimension
  pre_dims <- get_predim(x, space_dim)

  # check if space dimension(s) are not the first dimension
  if (pre_dims > 0) {

    # for all dimension before recycle the value of each cell corresponding
    # times
    grid <- dim(x)[seq_len(pre_dims)] %>%
      prod() %>%
      rep(grid, each = .)
  }

  grid
}


# Function to get position of last dimension before first space dimension in
# array
get_predim <- function(x, space_dim) {
  which(names(dim(x)) %in% space_dim)[1] - 1
}
