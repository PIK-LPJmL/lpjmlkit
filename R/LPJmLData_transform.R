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

    # Transform time format
    if (any(to %in% private$.meta$._dimension_map_$time)) {
      private$.transform_time(to = "time")
      to <- to[!to %in% private$.meta$._dimension_map_$time]
    } else if (any(to %in% private$.meta$._dimension_map_$year_month_day)) {
      private$.transform_time(to = "year_month_day")
      to <- to[!to %in% private$.meta$._dimension_map_$year_month_day]
    }

    # Transform space format
    if (any(to %in% private$.meta$._dimension_map_$cell)) {
      private$.transform_space(to = "cell")
      to <- to[!to %in% private$.meta$._dimension_map_$cell]
    } else if (any(to %in% private$.meta$._dimension_map_$lon_lat)) {
      private$.transform_space(to = "lon_lat")
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


# Method to transform space dimension of data array from "cell" to "lon_lat" or
#   the other way around. If required add_grid to LPJmLData along the way.
LPJmLData$set("private",
              ".transform_space",
              function(to = NULL) {

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

    # Support lazy loading of grid files. This throws an error if no suitable
    # grid file is detected.
    self$add_grid()

    # Extract dimensions other than space dimension(s) from self
    other_dimnames <- dimnames(self$data) %>%
      `[<-`(unlist(strsplit(private$.meta$._space_format_, "_")), NULL)
    other_dims <- dim(self$data) %>%
      `[`(names(other_dimnames))

    # Case 1: Transformation from cell dimension to lon, lat dimensions
    if (private$.meta$._space_format_ == "cell" &&
        to == "lon_lat") {

      private$.grid$.__transform_space__(to = to)

      # Matrix with ilon and ilat indices of cells in new array
      ilonilat <- arrayInd(
        match(as.integer(dimnames(self)[["cell"]]), private$.grid$data),
        dim(private$.grid)
      )
      dimnames(ilonilat) <- list(cell = dimnames(self)[["cell"]],
                                 band = c("lon", "lat"))

      # Index matrix to access elements from source data
      index_source <- expand.grid(sapply(dim(self), seq_len)) # nolint:undesirable_function_linter.

      # Transform index matrix from source to target
      index_target <- mapply( # nolint:undesirable_function_linter.
        function(index, name, ilonilat) {
          if (name == "cell") {
            ilonilat[index, ]
          } else {
            index
          }
        },
        index = index_source,
        name = names(dim(self)),
        MoreArgs = list(ilonilat = ilonilat)
      ) %>%
        unlist(recursive = TRUE, use.names = FALSE) %>%
        matrix(nrow = nrow(index_source))

      rm(index_source, ilonilat)
      gc(full = TRUE)

      # Append new space dimension where they have been before
      new_dimnames <- append(other_dimnames,
                             values = dimnames(private$.grid$data)[c("lat", "lon")], # nolint
                             after = get_predim(self$data, "cell"))
      new_dims <- append(other_dims,
                         values = dim(private$.grid$data)[c("lat", "lon")],
                         after = get_predim(self$data, "cell"))

      # Replace source space dimension with target space dimensions in dim and
      # dimnames attribute
      new_dimnames <- append(
        other_dimnames,
        values = dimnames(private$.grid$data)[c("lon", "lat")],
        after = get_predim(self$data, "cell")
      )
      new_dims <- append(
        other_dims,
        values = dim(private$.grid$data)[c("lon", "lat")],
        after = get_predim(self$data, "cell")
      )

      # Create target data array
      target_array <- array(NA, dim = new_dims, dimnames = new_dimnames)

      # Insert data from source array into target array
      target_array[index_target] <- self$data

    # Case 2: Transformation between lon, lat dimensions and cell dimension
    } else if (private$.meta$._space_format_ == "lon_lat" &&
        to == "cell") {

      # Matrix with ilon and ilat indices of cells in new array
      ilonilat <- arrayInd(match(sort(private$.grid$data), private$.grid$data),
                           dim(private$.grid))

      dimnames(ilonilat) <- list(cell = format(sort(private$.grid$data),
                                               trim = TRUE, scientific = FALSE),
                                 band = c("lon", "lat"))

      # Transform grid to target space format
      private$.grid$.__transform_space__(to = to)

      new_dimnames <- append(
        other_dimnames,
        values = dimnames(private$.grid$data)["cell"],
        after = get_predim(self$data, c("lon", "lat"))
      )
      new_dims <- append(
        other_dims,
        values = dim(private$.grid$data)["cell"],
        after = get_predim(self$data, c("lon", "lat"))
      )


      # Index matrix to access elements from source data
      index_source <- expand.grid(sapply(new_dims, seq_len)) # nolint:undesirable_function_linter.

      # Transform index matrix from source to target
      index_target <- mapply( # nolint:undesirable_function_linter.
        function(index, name, ilonilat) {
          if (name == "cell") {
            ilonilat[index, ]
          } else {
            index
          }
        },
        index = index_source,
        name = names(new_dimnames),
        MoreArgs = list(ilonilat = ilonilat)
      ) %>%
        unlist(recursive = TRUE, use.names = FALSE) %>%
          matrix(nrow = nrow(index_source))

      rm(index_source, ilonilat)
      gc(full = TRUE)

      target_array <- array(self$data[index_target], dim = new_dims,
                         dimnames = new_dimnames)

    } else {
      return(invisible(self))
    }

    # Overwrite internal data with same data but new dimensions
    self$.__set_data__(target_array)

    # Set space format in meta data entry
    private$.meta$.__transform_space_format__(to)

    return(invisible(self))
  }
)


# Method to transform the time dimension of the data array from "time" to
# "year_month_day" or the other way around
LPJmLData$set("private",
              ".transform_time",
              function(to = NULL) {

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

# Function to get position of last dimension before first space dimension in
# array
get_predim <- function(x, space_dim) {
  which(names(dim(x)) %in% space_dim)[1] - 1
}
