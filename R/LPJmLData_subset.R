#' Subset LPJmLData object
#'
#' Use dimension names of LPJmLData array directly to subset each by simply
#' using subset vectors.
#'
#' @param ... dimensions of each
#'
#' @return LPJmLData object
#' @export
#'
#' @examples
subset <- function(x, ...) {
  y <- x$clone(deep = TRUE)
  y$subset(...)
  return(y)
}

LPJmLData$set(
  "public",
  "subset",
  # TODO: INSERT ROXYGEN DOC
  function(...) {
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
      self$data <- subset_array_pair(x = self$data,
                                     pair = subset_list[[coords]])
      self$grid$data <- subset_array_pair(x = self$grid$data,
                                          pair = subset_list[[coords]])
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
    self$data <- subset_array(self$data,
                              subset_list[names(subset_list) != coords],
                              drop = FALSE)

    # same for grid but only for space dimensions
    if (!is.null(self$grid) && !is.null(subset_space_dim)) {
      self$grid$data <- subset_array(self$grid$data,
                                     subset_list[subset_space_dim],
                                     drop = FALSE)
    }

    if ("time" %in% names(subset_list)) {
      if (self$meta$time_format != "time") {
        # check if current time_format is "time"
        stop_format("time", "time")
      }
      # if time should be converted by time string, it has to be passed
      #   to ._update_subset method in LPJmLMetaData which does not have
      #   time strings of the data
      time_dimnames <- self$dimnames()$time
    } else {
      time_dimnames <- NULL
    }

    if (any(c(lon_lat, "cell", coords) %in% names(subset_list))) {
      # if space dimensions are subsetted convert ._update_subset method
      #   in LPJmLMetaData needs to know the resulting number of cells
      #   as well as the (new) firstcell - pass resulting cell_dimnames
      if (self$meta$space_format == "cell") {
        cell_dimnames <- self$dimnames()$cell
      } else {
        cell_dimnames <- transform_space(self$grid) %>%
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
    self$meta$._update_subset(subset_list,
                              time_dimnames,
                              cell_dimnames)
    if (!is.null(self$grid)) {
      self$grid$meta$._update_subset(subset_list[subset_space_dim],
                                     cell_dimnames = cell_dimnames)
    }

    return(invisible(self))
  }
)