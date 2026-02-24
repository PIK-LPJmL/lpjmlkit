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
#' runoff <- read_io(
#'   filename = "runoff.bin.json",
#'   subset = list(year = as.character(1991:2000))
#' )
#'
#' # Transform into space format "lon_lat". This assumes a "grid.bin.json" file
#' # is present in the same directory as "runoff.bin.json".
#' transform(runoff, to = "lon_lat")
#' # [...]
#' # $data |>
#' #   dimnames() |>
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
#' # $data |>
#' #   dimnames() |>
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
LPJmLData$set(
  "private",
  ".transform",
  function(to) {
    # Transform time format
    if (any(to %in% private$.meta$._dimension_map_$time)) {
      private$.transform_time(to = "time")
      to <- setdiff(to, private$.meta$._dimension_map_$time)
    } else if (any(to %in% private$.meta$._dimension_map_$year_month_day)) {
      private$.transform_time(to = "year_month_day")
      to <- setdiff(to, private$.meta$._dimension_map_$year_month_day)
    }

    # Transform space format
    if (any(to %in% private$.meta$._dimension_map_$cell)) {
      private$.transform_space(to = "cell")
      to <- setdiff(to, private$.meta$._dimension_map_$cell)
    } else if (any(to %in% private$.meta$._dimension_map_$lon_lat)) {
      private$.transform_space(to = "lon_lat")
      to <- setdiff(to, private$.meta$._dimension_map_$lon_lat)
    }

    if (length(to) > 0) {
      stop(
        ifelse(length(to) > 1, "Formats ", "Format "),
        toString(col_var(to)),
        ifelse(length(to) > 1, " are ", " is "),
        "not valid. Please choose from available space formats ",
        toString(col_var(private$.meta$._dimension_map_$space_format)),
        " and available time formats ",
        toString(col_var(private$.meta$._dimension_map_$time_format)),
        call. = FALSE
      )
    }

    return(invisible(self))
  }
)


# Method to transform space dimension of data array from "cell" to "lon_lat" or
#   the other way around. If required add_grid to LPJmLData along the way.
LPJmLData$set(
  "private",
  ".transform_space",
  function(to) {
    # If to equals current format return directly
    if (private$.meta$._space_format_ == to) {
      return(invisible(self))
    }

    # Support lazy loading of grid files. This throws an error if no suitable
    # grid file is detected.
    self$add_grid()

    # Extract dimensions other than space dimension(s) from self
    other_dimnames <- dimnames(self$data) %>%
      `[<-`(unlist(strsplit(private$.meta$._space_format_, "_")), NULL)
    other_dims <- dim(self$data)[names(other_dimnames)]

    # Case 1: Transformation from cell dimension to lon, lat dimensions
    if (private$.meta$._space_format_ == "cell" &&
          to == "lon_lat") {
      private$.grid$transform(to = to)
      # Matrix with ilon and ilat indices of cells in new array
      ilonilat <- arrayInd(
        match(
          as.integer(dimnames(self)[["cell"]]),
          drop_omit(private$.grid$data, c("lon", "lat"))
        ),
        dim(private$.grid)[c("lon", "lat")]
      )
      dimnames(ilonilat) <- list(
        cell = dimnames(self)[["cell"]],
        band = c("lon", "lat")
      )

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
      new_dimnames <- append(
        other_dimnames,
        values = dimnames(private$.grid$data)[c("lat", "lon")], # nolint
        after = get_predim(self$data, "cell")
      )
      new_dims <- append(
        other_dims,
        values = dim(private$.grid$data)[c("lat", "lon")],
        after = get_predim(self$data, "cell")
      )

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
    } else if (private$.meta$._space_format_ == "lon_lat" && to == "cell") {
      # Matrix with ilon and ilat indices of cells in new array
      ilonilat <- arrayInd(
        match(
          sort(private$.grid$data),
          drop_omit(private$.grid$data, c("lon", "lat"))
        ),
        dim(private$.grid)[c("lon", "lat")]
      )

      dimnames(ilonilat) <- list(
        cell = format(sort(private$.grid$data),
          trim = TRUE, scientific = FALSE
        ),
        band = c("lon", "lat")
      )

      # Transform grid to target space format
      private$.grid$transform(to = to)

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

      target_array <- array(self$data[index_target],
        dim = new_dims,
        dimnames = new_dimnames
      )
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
LPJmLData$set(
  "private",
  ".transform_time",
  function(to) {
    # If to equals current format return directly
    if (private$.meta$._time_format_ == to) {
      return(invisible(self))
    }

    # Case 1: Transformation from "time" dimension to "year", "month", "day"
    #   dimensions (if available)
    if (private$.meta$._time_format_ == "time" && to == "year_month_day") {
      # Split time string "year-month-day" into year, month, day integer vector,
      #   reverse it to get it into the right order for array conversion
      time_dimnames <- split_time_names(self$dimnames()[["time"]]) %>% rev()

      # Check that time axis is in sequence. Re-sequence if necessary.
      valid_timestamps <- create_time_names(
        nstep = private$.meta$nstep,
        years <- as.integer(time_dimnames[["year"]]),
        months = as.integer(time_dimnames[["month"]]),
        days = as.integer(time_dimnames[["day"]])
      )
      invalid_timestamps <- setdiff(self$dimnames()[["time"]], valid_timestamps)
      if (length(invalid_timestamps) > 0) {
        # This may need revising for NetCDF files if monthly or annual values
        # are set to a different date.
        stop(
          length(invalid_timestamps),
          " unexpected time stamp(s) in time axis: ",
          ifelse(
            length(invalid_timestamps) > 5,
            paste(toString(invalid_timestamps[seq_len(5)]), "[...]"),
            toString(invalid_timestamps)
          )
        )
      }
      if (!identical(valid_timestamps, self$dimnames()[["time"]])) {
        if (!all(valid_timestamps %in% self$dimnames()[["time"]])) {
          warning(
            "Prior subsetting induced gaps in time dimension that will be",
            " filled with NAs during transformation.",
            call. = FALSE
          )
        } else {
          warning(
            "Time dimension is out of sequence. Re-sequencing before",
            " transformation.",
            call. = FALSE
          )
        }
        # Re-sequence from start to end
        private$.subset(
          time = intersect(valid_timestamps, self$dimnames()[["time"]])
        )
      }

      # Remove sub-dimensions not necessary for monthly or annual data
      if (private$.meta$nstep == 1) {
        time_dimnames[["day"]] <- time_dimnames[["month"]] <- NULL
      } else if (private$.meta$nstep == 12) {
        time_dimnames[["day"]] <- NULL
      }

      new_time_dim <- "year_month_day"

      # Case 2: Transformation from dimensions "year", "month", "day"
      # (if available) to "time" dimension
    } else if (private$.meta$._time_format_ == "year_month_day" && to == "time") { # nolint:line_length_linter

      # Check if time axis is in sequence and re-sequence if necessary
      if ("day" %in% names(self$dimnames()) &&
            any(diff(as.integer(self$dimnames()$day)) < 1)) {
        warning(
          "Prior subsetting created day axis out of sequence. Days will be",
          " re-sequenced during transformation.",
          call. = FALSE
        )
        private$.subset(
          day = as.character(sort(as.integer(self$dimnames()$day)))
        )
      }
      if ("month" %in% names(self$dimnames()) &&
            any(diff(as.integer(self$dimnames()$month)) < 1)) {
        warning(
          "Prior subsetting created month axis out of sequence. Months will be",
          " re-sequenced during transformation.",
          call. = FALSE
        )
        private$.subset(
          month = as.character(sort(as.integer(self$dimnames()$month)))
        )
      }

      # Convert time dimnames back to time
      old_dimnames <- self$dimnames() %>%
        lapply(as.integer) %>%
        suppressWarnings()

      time_dimnames <- list(
        time = create_time_names(
          nstep = private$.meta$nstep,
          years = old_dimnames$year,
          months = {
            if (is.null(old_dimnames$month)) NULL else sort(old_dimnames$month)
          },
          days = {
            if (is.null(old_dimnames$day)) NULL else sort(old_dimnames$day)
          }
        )
      )

      new_time_dim <- "time"

      # Return directly if no transformation is necessary
    } else {
      return(invisible(self))
    }

    # Extract dimensions other than space dimension(s) from self
    other_dimnames <- dimnames(self$data) %>%
      `[<-`(unlist(strsplit(private$.meta$._time_format_, "_")), NULL)
    other_dims <- dim(self$data)[names(other_dimnames)]

    time_dims <- lapply(time_dimnames, length)

    new_dimnames <- append(
      other_dimnames,
      values = time_dimnames,
      after = get_predim(
        self$data,
        unlist(strsplit(private$.meta$._time_format_, "_"))
      )
    )

    new_dims <- append(
      other_dims,
      values = time_dims,
      after = get_predim(
        self$data,
        unlist(strsplit(private$.meta$._time_format_, "_"))
      )
    )

    # Create new data array based on disaggregated time dimension
    self$.__set_data__(
      assign_data_dimensions(
        x = self,
        new_dims = new_dims,
        new_dimnames = new_dimnames,
        to_format = to
      )
    )
    private$.meta$.__transform_time_format__(new_time_dim)

    return(invisible(self))
  }
)

# Function to get position of last dimension before first space dimension in
# array
get_predim <- function(x, dims) {
  which(names(dim(x)) %in% dims)[1] - 1
}
# Function to get position of first dimension after last space dimension in
# array
get_postdim <- function(x, dims) {
  max(which(names(dim(x)) %in% dims)) + 1
}

# Function to handle correct data dimensions, especially for daily data
#   that comes with months of different length, so NAs are assigned in day
#   dimension for day 28/29/30/31 where necessary. If transformed from
#   "year_month_day" format, invalid dates are removed again. Also correct
#   transformation in case of incomplete time series.
assign_data_dimensions <- function(x, new_dims, new_dimnames, to_format = "time") { # nolint:line_length_linter
  old_dimnames <- dimnames(x)
  if (x$meta$nstep == 365 ||
    (to_format == "year_month_day" &&
       dim(x)["time"] != x$meta$nstep * x$meta$nyear) ||
    (to_format == "time" &&
      any(
        dim(x)[unlist(strsplit(x$meta$._time_format_, "_"))] !=
          c(x$meta$nyear, 12, 31),
        na.rm = TRUE
      )
    )
  ) {
    # Only use complex transformation for daily data or incomplete time series.
    if (to_format == "year_month_day") {
      use_dimnames <- new_dimnames
    } else {
      use_dimnames <- old_dimnames
    }

    # Compute full timestamps including invalid dates for array handling
    full_timestamps <- create_time_names(
      nstep = x$meta$nstep,
      years = as.integer(use_dimnames$year),
      months = {
        if (is.null(use_dimnames$month))
          NULL
        else
          sort(as.integer(use_dimnames$month))
      },
      days = {
        if (is.null(use_dimnames$day))
          NULL
        else
          sort(as.integer(use_dimnames$day))
      },
      only_valid = x$meta$nstep != 365
    )
    # Compute valid timestamps to match against full timestamps to filter
    # invalids.
    valid_timestamps <- create_time_names(
      nstep =  x$meta$nstep,
      years = as.integer(use_dimnames$year),
      months = {
        if (is.null(use_dimnames$month))
          NULL
        else
          sort(as.integer(use_dimnames$month))
      },
      days = {
        if (is.null(use_dimnames$day))
          NULL
        else
          sort(as.integer(use_dimnames$day))
      },
      only_valid = TRUE
    )

    if (to_format == "time") {
      # Assign NAs to invalid timestamps
      full_timestamps[!full_timestamps %in% valid_timestamps] <- NA

      # Expand time axis to cover full array grid
      pre_dim <- seq_len(
        get_predim(x, unlist(strsplit(x$meta$._time_format_, "_")))
      )
      if (length(pre_dim) > 0 && any(dim(x)[pre_dim] > 1)) {
        expanded_time <- rep(
          full_timestamps,
          each = prod(dim(x)[pre_dim])
        )
      } else {
        expanded_time <- full_timestamps
      }
      if (get_postdim(x, unlist(strsplit(x$meta$._time_format_, "_"))) <=
          length(dim(x))
      ) {
        post_dim <- seq(
          get_postdim(x, unlist(strsplit(x$meta$._time_format_, "_"))),
          length(dim(x))
        )
        if (any(dim(x)[post_dim] > 1)) {
          expanded_time <- rep(
            expanded_time,
            times = prod(dim(x)[post_dim])
          )
        }
      }
      # Initialize the new_data array without NAs (filter invalid timestamps)
      new_data <- array(
        x$data[which(!is.na(expanded_time))],
        dim = new_dims,
        dimnames = new_dimnames
      )
    } else if (to_format == "year_month_day") {

      # Assign NAs to invalid timestamps
      index <- which(
        !full_timestamps %in% valid_timestamps |
          !full_timestamps %in% old_dimnames$time
      )
      full_timestamps[index] <- NA

      # Expand time axis to cover full array grid
      pre_dim <- seq_len(
        get_predim(x, unlist(strsplit(x$meta$._time_format_, "_")))
      )
      expanded_time <- match(full_timestamps, old_dimnames$time)
      if (length(pre_dim) > 0 && any(dim(x)[pre_dim] > 1)) {
        expanded_time <- rep(
          expanded_time,
          each = prod(dim(x)[pre_dim])
        )
      }
      if (get_postdim(x, unlist(strsplit(x$meta$._time_format_, "_"))) <=
          length(dim(x))
      ) {
        post_dim <- seq(
          get_postdim(x, unlist(strsplit(x$meta$._time_format_, "_"))),
          length(dim(x))
        )
        if (any(dim(x)[post_dim] > 1)) {
          expanded_time <- rep(
            expanded_time,
            times = prod(dim(x)[post_dim])
          )
        }
      }
      valid <- which(!is.na(expanded_time))
      expanded_time[valid] <- seq_len(length(valid))
      rm(valid)

      # Assign data only to valid timestamps of new data array
      new_data <- array(
        x$data[expanded_time],
        dim = new_dims,
        dimnames = new_dimnames
      )
    }
  } else {
    # Default case
    new_data <- array(x$data, dim = new_dims, dimnames = new_dimnames)
  }

  return(new_data)
}
