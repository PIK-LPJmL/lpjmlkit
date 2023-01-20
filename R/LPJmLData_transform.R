#' Transform LPJmLData object
#'
#' Function to transform inner [`LPJmLData`] array into another
#' space or another time format. Combinations are also possible.
#'
#' @param x [LPJmLData] object
#'
#' @param to character vector defining space and/or time format into which
#' corresponding data dimensions should be transformed. Choose from space
#' formats `c("cell", "lon_lat")` and time formats `c("time","year_month_day")`.
#'
#' @return [`LPJmLData`] object in selected format
#'
#' @examples
#' \dontrun{
#'
#' runoff <- read_io(filename = glue("{output_path}/runoff.bin.json"),
#'                   subset = list(year = 1991:2000))
#'
#' # transform into space format "lon_lat"
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
#' # transform -> split time format into years, months (, days)
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
  return(y)
}

# transform method roxygen documentation in LPJmlData.R
LPJmLData$set("private",
              ".transform",
              function(to) {
    # check for locked objects
    check_method_locked(self, "transform")
    if (any(to %in% private$.meta$._dimension_map_$time)) {
      private$transform_time(to = "time")
      to <- to[!to %in% private$.meta$._dimension_map_$time]
    } else if (any(to %in% private$.meta$._dimension_map_$year_month_day)) {
      private$transform_time(to = "year_month_day")
      to <- to[!to %in% private$.meta$._dimension_map_$year_month_day]
    }
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

# method to transform dimensions of grid array from "cell" to "lon_lat" or the other
#   way around
LPJmLData$set("private",
              ".transform_grid",
              function(to = NULL) {
    if (is.null(private$.meta$variable) ||
        private$.meta$variable != "grid") {
      stop(paste("not legit for variable", private$.meta$variable))
    }
    # convenience function - if null automatically switch to other to
    if (is.null(to)) {
      if (private$.meta$._space_format_ == "cell") {
        to <- "lon_lat"
      } else {
        to <- "cell"
      }
    }

    # case for transform from cell dimension to lon, lat dimensions
    if (private$.meta$._space_format_ == "cell" &&
        to == "lon_lat") {
      # calculate grid extent from range to span raster
      grid_extent <- apply(
          self$data,
          "band",
          range
      )
      # calculate dimnames for full 2 dimensional grid
      spatial_dimnames <- mapply(seq,
                                 rev(grid_extent[1, ]),
                                 rev(grid_extent[2, ]) +
                                   c(private$.meta$cellsize_lat,
                                     private$.meta$cellsize_lon) / 2,
                                 by = c(private$.meta$cellsize_lat,
                                        private$.meta$cellsize_lon),
                                 SIMPLIFY = FALSE)
      # init grid array
      grid_array <- array(NA,
                        dim = lapply(spatial_dimnames, length),
                        dimnames = spatial_dimnames)
      # get indices of lat and lon dimnames
      ilon <- round((self$data[, "lon"] - min(grid_extent[, "lon"])) /
        private$.meta$cellsize_lon) + 1
      ilat <- round((self$data[, "lat"] - min(grid_extent[, "lat"])) /
        private$.meta$cellsize_lat) + 1
      # now set dimnames of grid_array to actual coordinates instead of dummy
      # spatial_dimnames
      coordlon <- self$data[match(seq_len(dim(grid_array)["lon"]), ilon), "lon"]
      dimnames(grid_array)$lon <- ifelse(
        is.na(coordlon),
        dimnames(grid_array)$lon,
        coordlon
      )
      rm(coordlon)
      coordlat <- self$data[match(seq_len(dim(grid_array)["lat"]), ilat), "lat"]
      dimnames(grid_array)$lat <- ifelse(
        is.na(coordlat),
        dimnames(grid_array)$lat,
        coordlat
      )
      rm(coordlat)
      # replace cell of lon and lat by cell index
      grid_array[cbind(ilat, ilon)] <- as.integer(
        dimnames(self$data)$cell
      )
      self$.__set_data__(grid_array)
      private$.meta$.__transform_space_format__("lon_lat")

    # case for transform from lon, lat dimensions to cell dimension
    } else if (private$.meta$._space_format_ == "lon_lat" &&
        to == "cell") {
      # get indices of actual cells
      grid_indices <- which(!is.na(self$data), arr.ind = TRUE)
      grid_dimnames <- lapply(dimnames(self$data), as.numeric)
      # select actual cells latitude and longitude and set cells as dimnames
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


# method to transform space dimension of array from "cell" to "lon_lat" or the
#   other way around. If required add_grid to LPJmLData along the way
LPJmLData$set("private",
              "transform_space",
              function(to = NULL) {

    # check if grid then use transform_grid method
    if (!is.null(private$.meta$variable) &&
        private$.meta$variable == "grid") {
      self$.__transform_grid__(to = to)
      return(invisible(self))
    }
    # check for locked objects
    check_method_locked(self, "transform_space")
    # if to is not specified switch to avail other format else if to equals
    #   current format return directly
    if (is.null(to)) {
      if (private$.meta$._space_format_ == "cell") {
        to <- "lon_lat"
      } else {
        to <- "cell"
      }
    } else {
      if (private$.meta$._space_format_ == to) {
        return(invisible(self))
      }
    }
    # support of lazy loading of grid for meta files else add explicitly
    if (is.null(private$.grid)) {
      self$add_grid()
    }
    # create new data array based on disaggregated time dimension
    other_dimnames <- dimnames(self$data) %>%
      `[<-`(unlist(strsplit(private$.meta$._space_format_, "_")), NULL)
    other_dims <- dim(self$data) %>%
      `[`(names(other_dimnames))

    # case for transform from cell dimension to lon, lat dimensions
    if (private$.meta$._space_format_ == "cell" &&
        to == "lon_lat") {
      private$.grid$.__transform_grid__(to = to)
      data_array <- array(
        private$.grid$data,
        dim = c(dim(private$.grid$data), other_dims),
        dimnames = do.call(list,
                           args = c(dimnames(private$.grid$data),
                                    other_dimnames))
      )
      data_array[!is.na(data_array)] <- self$data
      # set corresponding meta data entry
      private$.meta$.__transform_space_format__("lon_lat")

    # ref between lon, lat dimensions and single cell dimension
    } else if (private$.meta$._space_format_ == "lon_lat" &&
        to == "cell") {
      mask_array <- array(private$.grid$data,
                          dim = dim(self$data),
                          dimnames = dimnames(self$data))
      private$.grid$.__transform_grid__(to = to)
      data_array <- array(
        NA,
        dim = c(dim(private$.grid$data)["cell"], other_dims),
        dimnames = do.call(list,
                           args = c(dimnames(private$.grid$data)["cell"],
                                    other_dimnames))
      )
      data_array[] <- self$data[!is.na(mask_array)]

      # set corresponding meta data entry
      private$.meta$.__transform_space_format__("cell")

      if (!is.null(private$.grid)) {
        private$.grid$meta$.__transform_space_format__("cell")
      }

    } else {
      return(invisible(self))
    }
    # overwrite internal data with same data but new dimensions
    self$.__set_data__(data_array)

    return(invisible(self))
  }
)


# method to transform time dimension of array from "time" to "year_month_cell"
#   or the other way around.
LPJmLData$set("private",
              "transform_time",
              function(to = NULL) {
    # check if grid then use .transform_grid method
    if (!is.null(private$.meta$variable) &&
        private$.meta$variable == "grid") {
      stop(paste("not legit for variable", private$.meta$variable))
    }
    # check for locked objects
    check_method_locked(self, "transform_time")
    # if to is not specified switch to avail other format else if to equals
    #   current format return directly
    if (is.null(to)) {
      if (private$.meta$._time_format_ == "time") {
        to <- "year_month_day"
      } else {
        to <- "time"
      }
    } else {
      if (private$.meta$._time_format_ == to) {
        return(invisible(self))
      }
    }
    # case for transform from "time" dimensions to year, month, day
    #   dimensions (if available)
    if (private$.meta$._time_format_ == "time" &&
        to == "year_month_day") {
      # possible ndays of months
      ndays_in_month <- c(31, 30, 28)
      # split time string "year-month-day" into year, month, day int vector
      #   reverse it to get it into the right order for array conversion
      time_dimnames <- split_time_names(self$dimnames()[["time"]]) %>% rev()

      # assume no daily data - remove day dimension
      if (all(time_dimnames[["day"]] %in% ndays_in_month)) {
        time_dimnames[["day"]] <- NULL
      }
      # assume no monthly data - remove month dimension
      if (length(time_dimnames$month) == 1 &&
          is.null(time_dimnames[["day"]])) {
        time_dimnames[["month"]] <- NULL
      }
      private$.meta$.__transform_time_format__("year_month_day")

    # case for transform from dimensions year, month, day (if available) to
    #   time dimension
    } else if (private$.meta$._time_format_ == "year_month_day" &&
               to == "time") {
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
    # else return without execution
    } else {
      return(invisible(self))
    }

    spatial_dims <- unlist(strsplit(private$.meta$._space_format_, "_"))
    # create new data array based on disaggregated time dimension
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
