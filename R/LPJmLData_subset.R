#' Subset an LPJmLData object
#'
#' Function to extract a subset of the full data in an [`LPJmLData`] object by
#' applying selections along one or several of its dimensions.
#'
#' @param x An [LPJmLData] object
#'
#' @param ... One or several key-value combinations where keys represent the
#'   dimension names and values represent the requested elements along these
#'   dimensions. Subsets may either specify integer indices, e.g.
#'   `cell = c(27411:27416)`, `band = -c(14:16, 19:32)`, or character vectors if
#'   the dimension has a dimnames attribute, e.g.
#'   `band = c("rainfed rice", "rainfed maize")`.\
#'   Coordinates, so pairs of lon and lat can be subsetted by providing a tibble
#'   in the form of `coords = tibble(lon = ..., lat =...)`. The argument can
#'   `"coordinates"`
#'
#' @return An [`LPJmLData`] object with dimensions resulting from the selection
#'   in `subset`. Meta data are updated as well.
#'
#' @examples
#' \dontrun{
#'
#' vegc <- read_io(filename = "./vegc.bin.json")
#'
#' # Subset cells by index
#' subset(vegc, cell = seq(27410, 27415))
#' # [...]
#' # $data %>%
#' #   dimnames() %>%
#' #     .$cell  "27409" "27410" "27411" "27412" "27413" "27414"
#' #     .$time  "1901-12-31" "1902-12-31" "1903-12-31" "1904-12-31" ...
#' #     .$band  "1"
#' # [...]
#'
#' # Subset time by character vector
#' subset(vegc, time = c("2001-12-31", "2002-12-31", "2003-12-31"))
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
  y
}

# subset method roxygen documentation in LPJmlData.R
LPJmLData$set("private",
              ".subset",
              function(...) {

    # Function to throw error if subset dimension does not fit the format
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

    # Use subset pair function first if coords/coordinates are provided
    if (any(c("coords", "coordinates") %in% names(subset_list))) {

      # Get term used for subsetting ("coords" or "coordinates" legit)
      coords <- c("coords", "coordinates")[
        c("coords", "coordinates") %in% names(subset_list)
      ]

      # Fail if current space_format is not "lon_lat"
      if (private$.meta$._space_format_ != "lon_lat") {
        stop_format(coords, "lon_lat")
      }

      # Subset pairs for both data and grid data
      self$.__set_data__(
        subset_array_pair(x = self$data,
                          pair = subset_list[[coords]])
      )
      private$.grid$.__set_data__(
        subset_array_pair(x = private$.grid$data,
                          pair = subset_list[[coords]])
      )

    } else {
      # Avoid errors when subsetting list with coords
      coords <- "none"
    }

    # Assign subset_space_dim for format "cell"
    if ("cell" %in% names(subset_list)) {
      subset_space_dim <- "cell"

      # Fail if current space_format is not "cell"
      if (private$.meta$._space_format_ != "cell") {
        stop_format(subset_space_dim, "cell")
      }

    } else {
      subset_space_dim <- NULL
    }

    # Assign subset_space_dim for format "lat_lon"
    if (any(lon_lat %in% names(subset_list))) {

      subset_space_dim <- lon_lat[lon_lat %in% names(subset_list)]

      # Fail if current space_format is not "lon_lat"
      if (private$.meta$._space_format_ != "lon_lat") {
        stop_format(subset_space_dim, "lon_lat")
      }
    }

    # Apply subset without coords (subsetting by coords done already)
    self$.__set_data__(
      subset_array(self$data,
                   subset_list[names(subset_list) != coords],
                   drop = FALSE)
    )

    # Apply subset also for grid, but only for space dimensions
    if (!is.null(private$.grid) && !is.null(subset_space_dim)) {
      private$.grid$.__set_data__(
        subset_array(private$.grid$data,
                     subset_list[subset_space_dim],
                     drop = FALSE)
      )
    }

    if ("time" %in% names(subset_list)) {
      if (private$.meta$._time_format_ != "time") {
        # Fail if current time_format is not "time"
        stop_format("time", "time")
      }

      # dimnames of "time" dimension need to be passed to the
      # `.__update_subset__` method in LPJmLMetaData for a subset by time
      time_dimnames <- self$dimnames()$time
    } else {
      time_dimnames <- NULL
    }

    year_dimnames <- create_year_dimnames(subset_list, self$data)

    if (any(c(lon_lat, "cell", coords) %in% names(subset_list))) {

      # New dimnames attribute of cell dimension needs to be passed to the
      # `.__update_subset__` method in LPJmLMetaData to update the number of
      # cells as well as the (new) firstcell if subsetting by cell
      if (private$.meta$._space_format_ == "cell") {
        cell_dimnames <- self$dimnames()$cell

      } else {

        if (is.null(private$.grid)) {
          stop("Missing $grid attribute. Add via $add_grid()")
        }
        cell_dimnames <- sort(private$.grid$data) %>%
          format(trim = TRUE, scientific = FALSE, justify = "none")
      }

    } else {
      cell_dimnames <- NULL
    }

    # Workaround for coords - sufficient to pass corresponding lat, lon to
    #   to update subset in meta data
    if (coords %in% names(subset_list)) {
      subset_list$lat <- subset_list[[coords]]$lat
      subset_list$lon <- subset_list[[coords]]$lon
      subset_space_dim <- c("lon", "lat")
      subset_list[[coords]] <- NULL
    }

    # Update corresponding meta data for subsets
    private$.meta$.__update_subset__(subset_list,
                                     cell_dimnames = cell_dimnames,
                                     time_dimnames = time_dimnames,
                                     year_dimnames = year_dimnames)

    # Update space subsets in grid
    if (!is.null(private$.grid)) {
      private$.grid$meta$.__update_subset__(subset_list[subset_space_dim],
                                            cell_dimnames = cell_dimnames)
    }

    return(invisible(self))
  }
)


create_year_dimnames <- function(subset_list, data) {

  if ("year" %in% names(subset_list) && is.numeric(subset_list[["year"]])) {
    year_dimnames <- data$dimnames()$year
  } else {
    year_dimnames <- NULL
  }

  year_dimnames
}
