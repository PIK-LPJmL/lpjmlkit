#' @title LPJmL data class
#'
#' @description A data container for LPJmL input and output. Container - because
#' a LPJmLData object is a environment in which the data array as well as the
#' meta data are stored after [`read_io`].
#' The data array can be accessed via `$data`, the meta data via `$meta`.
#' The enclosing environment is locked and cannot be altered by any
#' other than the available modify methods and thus ensures its integrity and
#' validity.
#' Please use base stats methods like [`print`, [`summary.LPJmLData`] or
#' [`plot.LPJmLData`] to get insights and export methods like [`as_tibble`] or
#' [`as_raster`] to export it into common working formats.
#'
LPJmLData <- R6::R6Class(
  classname = "LPJmLData",
  lock_objects = TRUE,
  public = list(
    # modify methods --------------------------------------------------------- #

    #' @description
    #' Method to add a grid to a `LPJmLData`.
    #' See also [`add_grid`]
    #'
    #' @param ... See [`add_grid`]
    add_grid = function(...) {
      # check for locked objects
      check_method_locked(self, "add_grid")
      # check if meta file for grid is located in output location
      grid_file <- list.files(self$meta$._data_dir_,
                              pattern = "grid.bin.json")
      if (length(grid_file) == 1) {
        # if so get concatenate existing file and data_dir to read grid
        filename <- paste(self$meta$._data_dir_, grid_file, sep = "/")
        # add support for cell subsets - this is a rough filter since $subset
        #   does not say if cell is subsetted - but ok for now
        if (self$meta$._subset_space_) {
          self$.__set_grid__(
            read_io(
              filename = filename,
              subset = list(cell = self$dimnames()[["cell"]])
            )
          )
        } else {
          self$.__set_grid__(
            read_io(filename = filename)
          )
        }
      } else {
        # all arguments have to be provided manually via read_io args
        #   ellipsis (...) does that
        # check if arguments are provided
        if (length(as.list(match.call())) > 1) {
          self$.__set_grid__(
            read_io(...)
          )
        } else {
          stop(paste("If no meta file is available $add_grid",
                     "has to be called explicitly with args as read_io."))
        }
      }
      private$.grid$.__set_lock__(is_locked = TRUE)
    },

    #' @description
    #' Method to use dimension names of `LPJmLData`
    #' array directly to subset each by simply using supplying
    #' vectors.
    #'
    #' @param ... See [`subset.LPJmLData`]
    subset = function(...) {
      private$.subset(...)
    },

    #' @description
    #' Method to transform inner `LPJmLData` array
    #' into another space or another time format.
    #'
    #' @param ... See [`transform`]
    transform = function(...) {
      private$.transform(...)
    },

    # export methods --------------------------------------------------------- #

    #' @description
    #' Method to coerce (convert) a `LPJmLData` object into a
    #' \link[base]{array}.
    #'
    #' @param ... See [`as_array`]
    as_array = function(...) {
      private$.as_array(...)
    },

    #' @description
    #' Method to coerce (convert) a `LPJmLData` object into a
    #' \link[tibble]{tibble} (modern \link[base]{data.frame}).
    #'
    #' @param ... See [`as_tibble`]
    as_tibble = function(...) {
      private$.as_tibble(...)
    },

    #' @description
    #' Method to coerce (convert) a `LPJmLData` object into a
    #' \link[raster]{raster} or \link[raster]{brick} object, that opens the
    #' space for any GIS based raster operations.
    #'
    #' @param ... See [`as_raster`]
    as_raster = function(...) {
      private$.as_raster(...)
    },

    #' @description
    #' Method to coerce (convert) a `LPJmLData` object into a
    #' \link[terra]{rast}, that opens the space for any GIS based raster
    #' operations.
    #'
    #' @param ... See [`as_terra`]
    as_terra = function(...) {
      private$.as_terra(...)
    },

    #' @description
    #' Method to plot a time-series or raster map of a `LPJmLData`
    #' object.
    #'
    #' @param ... See [`plot.LPJmLData`]
    plot = function(...) {
      private$.plot(...)
    },

    # stats methods ---------------------------------------------------------- #

    #' @description
    #' Method to get the length of the array of a `LPJmLData`
    #' object. \cr
    #' See also \link[base]{length}
    length = function() {
      private$.length()
    },

    #' @description
    #' Method to get the dimension names and lengths of the array of a
    #' `LPJmLData` object. \cr
    #' See also \link[base]{dim}
    dim = function() {
      private$.dim()
    },

    #' @description
    #' Method to get the dimensions (list) of the array of a
    #' `LPJmLData` object.
    #'
    #' @param ... See [dimnames.LPJmLData]
    dimnames = function(...) {
      private$.dimnames(...)
    },

    #' @description
    #' Method to get the summary of the array of a
    #' `LPJmLData` object.
    #'
    #' @param ... See [summary.LPJmLData]
    summary = function(...) {
      private$.summary(...)
    },

    #' @description
    #' Method to print the `LPJmLData`. \cr
    #' See also \link[base]{print}
    print = function() {
      # set colour higlighting
      blue_col <- "\u001b[34m"
      unset_col <- "\u001b[0m"
      # print meta data
      cat(paste0("\u001b[1m", blue_col, "$meta %>%", unset_col, "\n"))
      self$meta$print(all = FALSE, spaces = "  .")
      # not all meta data are printed 
      cat(paste0("\u001b[33;3m",
                 "Note: not printing all meta data, use $meta to get all.",
                 unset_col,
                 "\n"))
      # print grid only if available
      if (!is.null(self$grid)) {
        cat(paste0("\u001b[1m\u001b[31m",
                   "$grid",
                   unset_col,
                   "\u001b[31m",
                   " ...",
                   unset_col,
                   "\n"))
      }
      # print data attribute
      cat(paste0("\u001b[1m",
                 blue_col,
                 "$data %>%",
                 unset_col,
                 "\n"))
      # dimnames
      dim_names <- self$dimnames()
      cat(paste0(blue_col, "  dimnames() %>%", unset_col, "\n"))
      for (sub in seq_along(dim_names)) {
        to_char2 <- ifelse(is.character(dim_names[[sub]]), "\"", "")
        if (length(dim_names[[sub]]) > 6) {
          abbr_dim_names <- paste0(c(paste0(to_char2,
                                            dim_names[[sub]][1:4],
                                            to_char2),
                                   "...",
                                   paste0(to_char2,
                                          utils::tail(dim_names[[sub]], n = 1),
                                          to_char2)))
        } else {
          abbr_dim_names <- paste0(to_char2, dim_names[[sub]], to_char2)
        }
        cat("  ",
            blue_col,
            paste0(".$", names(dim_names[sub])),
            unset_col,
            abbr_dim_names)
        cat("\n")
      }
      # summary
      cat(paste0(blue_col, "$summary()", unset_col, "\n"))
      print(self$summary(cutoff = TRUE))
      if (is.null(self$meta$variable) ||
      self$meta$variable != "grid") {
        cat(paste0("\u001b[33;3m",
                   "Note: summary is not weighted by grid area.",
                   unset_col,
                   "\n")
        )
      } else {
        cat(paste0("\u001b[33;3m",
                   ifelse(self$meta$._space_format_ == "cell",
                          "Note: only min & max printed as equivalent to spatial extent.", #nolint
                          "Note: inverted grid (cell as value)! Only min & max printed for sequence of cells."), #nolint
                   unset_col,
                   "\n"))
      }
    },

    #' @description
    #' Internal method only to be used for package development.
    #'
    #' @param ... ...
    .__transform_grid__ = function(...) {
      private$.transform_grid(...)
    },

    # set data attribute only to be used internally or explicitly
    #   on purpose
    #' @description
    #' !Internal method only to be used for package development!
    #'
    #' @param data ...
    .__set_data__ = function(data) {
      private$.data <- data
    },

    # set grid attribute only to be used internally or explicitly
    #   on purpose
    #' @description
    #' !Internal method only to be used for package development!
    #'
    #' @param grid ...
    .__set_grid__ = function(grid) {
      private$.grid <- grid
    },

    # set is_locked attribute only to be used internally or explicitly
    #   on purpose
    #' @description
    #' !Internal method only to be used for package development!
    #'
    #' @param is_locked ...
    .__set_lock__ = function(is_locked) {
      private$.is_locked <- is_locked
    },

    # Create a new LPJmLData object only to be used internally or explicitly
    #' @description
    #' !Internal method only to be used for package development!
    #'
    #' @param data `array` with LPJmL data
    #'
    #' @param meta_data meta_data `LPJmLMetaData` Object
    initialize = function(data, meta_data = NULL) {
      if (methods::is(meta_data, "LPJmLMetaData") |
          methods::is(meta_data, "NULL")) {
        private$.meta <- meta_data
      } else {
        stop("Provide a LPJmLMetaData object for meta data.")
      }
      private$.data <- data
      if (!is.null(self$meta$variable)) {
        if (self$meta$variable == "grid") {
          private$init_grid()
        } else if (self$meta$variable == "LPJGRID") {
          private$.meta$.__set_attribute__(".variable", "grid")
          private$init_grid()
        }
      }
    }
  ),
  active = list(
    #' @field meta [`LPJmLMetaData`] object to store corresponding meta data
    meta = function() {
      return(private$.meta)
    },
    #' @field data \link[base]{array} containing the underlying data
    data = function() {
      return(private$.data)
    },
    #' @field grid *optional* - `LPJmLData` containing the underlying grid,
    grid = function() {
      return(private$.grid)
    },
    #' @field ._is_locked_ *internal* Logical. Is object locked (no method can
    #' be performed directly on the object)
    ._is_locked_ = function() {
      return(private$.is_locked)
    }
  ),
  private = list(
    # init grid if variable == "grid"
    init_grid = function() {
      # update grid data
      dimnames(private$.data)[["band"]] <- c("lon", "lat")
      # update grid meta data
      self$.__set_data__(
        drop_omit(self$data, omit = "cell")
      )
      self$meta$.__init_grid__()
      return(invisible(self))
    },
    .meta = NULL,
    .data = NULL,
    .grid = NULL,
    .is_locked = FALSE
  )
)

# set up method dispatch ----------------------------------------------------- #

#   https://stackoverflow.com/questions/50842251/define-a-bracket-operator-on-an-r6-class # nolint
# add additional (important) functions for method dispatch with deep copying
#   x, execute function on copied object and return ("traditional way")


#' Add grid to LPJmLData object
#'
#' Function to add a grid to an [`LPJmLData`]. The function acts
#' as a [`read_io`] function for the grid file and adds it as an
#' `LPJmLData` object itself to the the main object as an attribute `$grid`
#'
#' @details
#' **Important:** If "file_type" == "raw` and data should be recognized as a
#' grid, prescribe `variable = "grid"`!
#' @param x [LPJmLData] object
#'
#' @param ... arguments passed to [`read_io`] if no grid file and or meta
#' file in corresponding output directory available.
#'
#' @return [`LPJmLData`] object in selected format
#'
#' @examples
#' \dontrun{
#'
#' # read in vegetation carbon data with meta file
#' vegc <- read_io(filename = "./vegc.bin.json")
#'
#' # add grid as attribute (via meta file in output directory)
#' vegc_with_grid <- add_grid(vegc)
#'
#' }
#'
#' @md
#' @export
add_grid <- function(x, ...) {
  y <- x$clone(deep = TRUE)
  y$add_grid(...)
  return(y)
}


# utility functions ---------------------------------------------------------- #

# aggregation function, only to be applied for conversions (as_raster, plot)
#   do not apply to self to not violate data integrity !
aggregate_array <- function(x,
                            aggregate_list = NULL,
                            ...) {
  data <- x$data
  if (!is.null(aggregate_list)) {
    for (idx in seq_along(aggregate_list)) {
      dim_names <- names(dim(data))
      data <- apply(X = data,
                    MARGIN = dim_names[
                      !dim_names %in% names(aggregate_list)[idx]
                    ],
                    FUN = aggregate_list[[idx]],
                    ...)
    }
  }
  return(data)
}


check_method_locked <- function(x, method_name) {
  if (x$._is_locked_) {
    stop(
      paste0(
        "\u001b[0m",
        "The attribute ",
        "\u001b[34m",
        ifelse(is.null(x$meta$variable), "???", x$meta$variable),
        "\u001b[0m",
        " is locked. You cannot use method ",
        "\u001b[34m",
        method_name,
        "\u001b[0m",
        " on this object.",
        "\n"
      )
    )
  }
}

# avoid note for "."...
utils::globalVariables(".") # nolintr:undesirable_function_linter