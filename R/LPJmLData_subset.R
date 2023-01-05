#' Subset LPJmLData object
#'
#' Function to use dimension names of \link[lpjmlkit]{LPJmLData} array directly
#' to subset each by simply using supplying vectors.
#'
#' @param ... Provide dimension to be used to subset `LPJmLData` objects
#' underlying data array in combination with indices vectors,
#' e.g. `cell = c(27411:27416)`, or ``band = -c(14:16, 19:32) or subset
#' using a "character" vector like `band = c("rainfed rice","rainfed maize")`
#'
#' @return \link[lpjmlkit]{LPJmLData} object with/without subset cell of
#' dimension
#'
#' @examples
#' \dontrun{
#'
#' vegc <- read_io(filename = "./vegc.bin.json")
#'
#' # subset cells by index
#' subset(vegc, cell = c(27409, 27415))
#' # [...]
#' # $data %>%
#' #   dimnames() %>%
#' #     .$cell  "27409" "27410" "27411" "27412" "27413" "27414"
#' #     .$time  "1901-12-31" "1902-12-31" "1903-12-31" "1904-12-31" ...
#' #     .$band  "1"
#' # [...]
#'
#' #' # subset time by character vector
#' subset(vegc, cell = c("2001-12-31", "2002-12-31", "2003-12-31"))
#' # [...]
#' # $data %>%
#' #   dimnames() %>%
#' #     .$cell  "0" "1" "2" "3" ... "67419"
#' #     .$time  "2001-12-31" "2002-12-31" "2003-12-31"
#' #     .$band  "1"
#' # [...]
#' }
#'
#' @md
#' @export
subset.LPJmLData <- function(x, ...) {
  y <- x$clone(deep = TRUE)
  y$subset(...)
  return(y)
}

# subset method roxygen documentation in LPJmlData.R
LPJmLData$set("private",
              ".subset",
              #' @description
              #' Method to use dimension names of link[lpjmlkit]{LPJmLData}
              #' array directly to subset each by simply using supplying
              #' vectors. See also \link[lpjmlkit]{subset}
              function(...) {
    # check for locked objects
    check_method_locked(self, "subset")
    # function to throw error if subset dimension does not fit the format
    stop_format <- function(subset_dim, format) {
      stop(
        paste0(
          "\u001b[34m",
          paste0(subset_dim, collapse = ", "),
          "\u001b[0m",
          " is defined in subset_list, but x has the wrong format. Use ",
          "\u001b[34m",
          "format(\"",
          format,
          "\")",
          "\u001b[0m",
          " to convert into suitable format."
        ),
        call. = FALSE
      )
    }

    lon_lat <- c("lon", "lat")
    subset_list <- list(...)

    # if coords/coordinates are provided use subset pair function first
    if (any(c("coords", "coordinates") %in% names(subset_list))) {
      # get term beeing used for subsetting ("coords" or "coordinates" legit)
      coords <- c("coords", "coordinates")[
        c("coords", "coordinates") %in% names(subset_list)
      ]
      # check if current space_format is "lon_lat"
      if (self$meta$space_format != "lon_lat") {
        stop_format(coords, "lon_lat")
      }
      # subset pairs for both data and grid data
      self$.__set_data__(
        subset_array_pair(x = self$data,
                          pair = subset_list[[coords]])
      )
      private$.grid$.__set_data__(
        subset_array_pair(x = self$grid$data,
                          pair = subset_list[[coords]])
      )
    } else {
      # to avoid errors when subsetting list with coords
      coords <- "none"
    }

    # assign subset_space_dim for fomrat "cell"
    if ("cell" %in% names(subset_list)) {
      subset_space_dim <- "cell"
      # check if current space_format is "cell"
      if (self$meta$space_format != "cell") {
        stop_format(subset_space_dim, "cell")
      }
    } else {
      subset_space_dim <- NULL
    }
    # assign subset_space_dim for fomrat "lat_lon"
    if (any(lon_lat %in% names(subset_list))) {
      subset_space_dim <- lon_lat[lon_lat %in% names(subset_list)]
      # check if current space_format is "lat_lon"
      if (self$meta$space_format != "lon_lat") {
        stop_format(subset_space_dim, "lon_lat")
      }
    }
    # do subset without coords (if provided - done already in the beginning)
    self$.__set_data__(
      subset_array(self$data,
                   subset_list[names(subset_list) != coords],
                   drop = FALSE)
    )

    # same for grid but only for space dimensions
    if (!is.null(self$grid) && !is.null(subset_space_dim)) {
      private$.grid$.__set_data__(
        subset_array(self$grid$data,
                     subset_list[subset_space_dim],
                     drop = FALSE)
      )
    }

    if ("time" %in% names(subset_list)) {
      if (self$meta$time_format != "time") {
        # check if current time_format is "time"
        stop_format("time", "time")
      }
      # if time should be converted by time string, it has to be passed
      #   to .__update_subset__ method in LPJmLMetaData which does not have
      #   time strings of the data
      time_dimnames <- self$dimnames()$time
    } else {
      time_dimnames <- NULL
    }

    if (any(c(lon_lat, "cell", coords) %in% names(subset_list))) {
      # if space dimensions are subsetted convert .__update_subset__ method
      #   in LPJmLMetaData needs to know the resulting number of cells
      #   as well as the (new) firstcell - pass resulting cell_dimnames
      if (self$meta$space_format == "cell") {
        cell_dimnames <- self$dimnames()$cell
      } else {
        grid <- private$.grid$clone(deep = TRUE)
        grid$.__set_lock__(is_locked = FALSE)
        cell_dimnames <- transform(grid, to = "cell") %>%
          dimnames() %>%
          .$cell
      }
    } else {
      cell_dimnames <- NULL
    }
    # workaround for coords - sufficient to pass corresponding lat, lon to
    #   to update subset in meta data
    if (coords %in% names(subset_list)) {
      subset_list$lat <- subset_list[[coords]]$lat
      subset_list$lon <- subset_list[[coords]]$lon
      subset_space_dim <- c("lon", "lat")
      subset_list[[coords]] <- NULL
    }
    # update corresponding meta data for subsets
    self$meta$.__update_subset__(subset_list,
                              time_dimnames,
                              cell_dimnames)
    if (!is.null(self$grid)) {
      private$.grid$meta$.__update_subset__(subset_list[subset_space_dim],
                                         cell_dimnames = cell_dimnames)
    }

    return(invisible(self))
  }
)