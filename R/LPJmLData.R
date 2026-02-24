#' @title LPJmL data class
#'
#' @description
#' A data container for LPJmL input and output. Container - because an
#' LPJmLData object is an environment in which the data array as well as the
#' meta data are stored after [`read_io()`].
#' The data array can be accessed via `$data`, the meta data via `$meta`.
#' The enclosing environment is locked and cannot be altered by any
#' other than the available modify methods to ensure its integrity and
#' validity.
#' Use base stats methods like [`print()`], [`summary.LPJmLData()`] or
#' [`plot.LPJmLData()`] to get insights and export methods like [`as_tibble()`]
#' or [`as_raster()`] to export it into common working formats.
#'
#' @md
#' @export
LPJmLData <- R6::R6Class( # nolint:object_name_linter

  classname = "LPJmLData",

  lock_objects = TRUE,

  public = list(

    # modify methods --------------------------------------------------------- #

    #' @description
    #' Method to add a grid to an `LPJmLData` object.
    #' See also [`add_grid`]
    #'
    #' @param ... See [`add_grid()`].
    add_grid = function(...) {
      # Skip if grid is already attached
      if (!is.null(private$.grid)) {
        return(invisible(self))
      }

      if (...length() == 0) {
        # If user has not supplied any parameters try to find a grid file in the
        # same directory as data. This throws an error if no suitable file is
        # found.
        if (!is.null(private$.meta$grid)) {
          if (dirname(private$.meta$grid$filename) == ".") {
            filename <- file.path(
              private$.meta$._data_dir_,
              private$.meta$grid$filename
            )
          } else {
            filename <- private$.meta$grid$filename
          }
        } else {
          filename <- find_varfile(private$.meta$._data_dir_, "grid")
        }
        message(
          paste0(
            col_var("grid"),
            " read from ",
            sQuote(basename(filename))
          )
        )

        # Add support for cell subsets. This is a rough filter since $subset
        #   does not say if cell is subsetted - but ok for now.
        if (private$.meta$._subset_space_) {
          lpjml_data <- read_io(
            filename = filename,
            subset = list(cell = self$dimnames()[["cell"]]),
            silent = TRUE
          )
        } else {
          lpjml_data <- read_io(filename = filename, silent = TRUE)
        }
      } else {
        # All arguments have to be provided manually to read_io.
        #   Ellipsis (...) does that.
        if (private$.meta$._subset_space_) {
          lpjml_data <- read_io(
            ...,
            subset = list(cell = self$dimnames()[["cell"]])
          )
        } else {
          lpjml_data <- read_io(...)
        }
      }
      # Create LPJmLData object and bring together data and meta_data
      lpjml_grid <- LPJmLGridData$new(lpjml_data)

      # set grid attribute
      self$.__set_grid__(lpjml_grid)
    },


    #' @description
    #' Method to use dimension names of `LPJmLData$data`
    #' array directly to subset each dimension to match the supplied vectors.
    #'
    #' @param ... See [`subset.LPJmLData()`]
    subset = function(...) {
      private$.subset(...)
    },


    #' @description
    #' Method to transform inner `LPJmLData$data` array
    #' into another space or time format.
    #'
    #' @param ... See [`transform()`].
    transform = function(...) {
      private$.transform(...)
    },


    # export methods --------------------------------------------------------- #

    #' @description
    #' Method to coerce (convert) an `LPJmLData` object into an
    #' \link[base]{array}.
    #'
    #' @param ... See [`as_array()`].
    as_array = function(...) {
      private$.as_array(...)
    },


    #' @description
    #' Method to coerce (convert) an `LPJmLData` object into a
    #' \link[tibble]{tibble} (modern \link[base]{data.frame}).
    #'
    #' @param ... See [`as_tibble()`].
    as_tibble = function(...) {
      private$.as_tibble(...)
    },


    #' @description
    #' Method to coerce (convert) an `LPJmLData` object into a
    #' \link[raster]{raster} or \link[raster]{brick} object that can be used
    #' for any GIS-based raster operations.
    #'
    #' @param ... See [`as_raster()`].
    as_raster = function(...) {
      private$.as_raster(...)
    },


    #' @description
    #' Method to coerce (convert) an `LPJmLData` object into a
    #' \link[terra]{rast} object that can be used for any GIS-based raster
    #' operations.
    #'
    #' @param ... See [`as_terra()`].
    as_terra = function(...) {
      private$.as_terra(...)
    },


    #' @description
    #' Method to plot a time-series or raster map of an `LPJmLData`
    #' object.
    #'
    #' @param ... See [`plot.LPJmLData()`].
    plot = function(...) {
      private$.plot(...)
    },


    # stats methods ---------------------------------------------------------- #

    #' @description
    #' Method to get the length of the data array of an `LPJmLData`
    #' object. \cr
    #' See also \link[base]{length}.
    length = function() {
      private$.length()
    },


    #' @description
    #' Method to get the dimensions of the data array of an
    #' `LPJmLData` object. \cr
    #' See also \link[base]{dim}.
    dim = function() {
      private$.dim()
    },


    #' @description
    #' Method to get the dimnames (list) of the data array of an
    #' `LPJmLData` object.
    #'
    #' @param ... See [`dimnames.LPJmLData()`].
    dimnames = function(...) {
      private$.dimnames(...)
    },


    #' @description
    #' Method to get the summary of the data array of an
    #' `LPJmLData` object.
    #'
    #' @param ... See [`summary.LPJmLData()]`.
    summary = function(...) {
      private$.summary(...)
    },


    #' @description
    #' Method to print the `LPJmLData` object. \cr
    #' See also \link[base]{print}.
    print = function() {
      # Print meta data
      cat(paste0(bold_head(col_var("$meta |>"))[1], "\n"))
      private$.meta$print(all = FALSE, spaces = "  .")

      # Not all meta data are printed
      cat(
        col_note("Note: not printing all meta data, use $meta to get all.\n")
      )

      # Print grid only if available
      if (!is.null(private$.grid)) {
        cat(col_var(paste0(bold_head("$grid")[1], " ...", "\n")))
      }

      # Print data attribute
      cat(bold_head("$data |>\n")[1])

      # Dimnames
      dim_names <- self$dimnames()
      cat(col_var("  dimnames() |>\n"))

      for (sub in seq_along(dim_names)) {
        to_char2 <- ifelse(is.character(dim_names[[sub]]), "\"", "")

        if (length(dim_names[[sub]]) > 6) {
          abbr_dim_names <- paste0(c(
            paste0(to_char2,
                   dim_names[[sub]][1:4],
                   to_char2),
            "...",
            paste0(to_char2,
                   utils::tail(dim_names[[sub]], n = 1),
                   to_char2)
          ))

        } else {
          abbr_dim_names <- paste0(to_char2, dim_names[[sub]], to_char2)
        }

        cat(
          "  ",
          col_var(
            paste0(".$", names(dim_names[sub]))
          ),
          abbr_dim_names
        )
        cat("\n")
      }

      # Summary
      cat(col_var("$summary()\n"))
      print(self$summary(cutoff = TRUE))
      # default handling of LPJmLData objects else inherited like LPJmLGridData
      if (class(self)[1] == "LPJmLData") {
        cat(
          col_note("Note: summary is not weighted by grid area.\n")
        )
      }
    },


    # Set data attribute; only to be used internally or explicitly
    #   on purpose
    #' @description
    #' !Internal method only to be used for package development!
    #'
    #' @param data Data array.
    .__set_data__ = function(data) {
      private$.data <- data
    },


    # Set grid attribute; only to be used internally or explicitly
    #   on purpose
    #' @description
    #' !Internal method only to be used for package development!
    #'
    #' @param grid An `LPJmLData` object holding grid coordinates.
    .__set_grid__ = function(grid) {

      if (!methods::is(grid, "LPJmLGridData")) {
        stop("Provide an LPJmLGridData object to set grid attribute.")
      }

      if (grid$meta$._space_format_ != self$meta$._space_format_) {
        grid$transform(to = self$meta$._space_format_)
      }

      if (self$meta$._space_format_ == "lon_lat") {
        data_dim_names <- dimnames(self$data)
        grid_dim_names <- dimnames(grid$data)

        # Check if data dimensions match grid dimensions, no subsetting
        #   supported - too error prone
        if (!all(dimnames(self)[["lat"]] %in% dimnames(grid)[["lat"]]) &&
            all(data_dim_names$lon == grid_dim_names$lon)) {
          warning("Cropping LPJmLData object with new LPJmLGridData")               
        } else if (!all(data_dim_names$lon == grid_dim_names$lon) ||
              !all(data_dim_names$lat == grid_dim_names$lat)) {
          stop(
            "Data dimensions do not match LPJmL grid dimensions. ",
            "Please assure data and grid are consistent."
          )
        }

        # get LPJmL grid cells from grid data
        cell_dimnames <- sort(grid$data) %>%
          format(trim = TRUE, scientific = FALSE, justify = "none")

        # Update meta data of data object
        private$.meta$.__update_subset__(
          subset = list(cell = cell_dimnames),
          cell_dimnames = cell_dimnames
        )
      }

      # Set grid attribute
      private$.grid <- grid
      if (!all(dimnames(self)[["lat"]] %in% dimnames(grid)[["lat"]])) {
        self$subset(lat=dimnames(grid)[["lat"]])        
      }
    },


    # Create a new LPJmLData object; only to be used internally or explicitly
    #' @description
    #' !Internal method only to be used for package development!
    #'
    #' @param data `array` with LPJmL data.
    #'
    #' @param meta_data An `LPJmLMetaData` object.
    initialize = function(data, meta_data = NULL) {

      if (methods::is(meta_data, "LPJmLMetaData") |
          methods::is(meta_data, "NULL")) { #nolint:indentation_linter
        private$.meta <- meta_data
      } else {
        stop("Provide an LPJmLMetaData object for meta data.")
      }

      private$.data <- data
    }
  ),


  # Active bindings
  active = list(

    #' @field meta [`LPJmLMetaData`] object to store corresponding meta data.
    meta = function(...) {
      check_change(self, "meta", ...)
      # Clone meta object so that if meta is changed outside of the LPJmLData
      #   instance it will not change this instance
      return(private$.meta$clone())
    },

    #' @field data \link[base]{array} containing the underlying data.
    data = function(...) {
      check_change(self, "data", ...)
      return(private$.data)
    },

    #' @field grid Optional `LPJmLData` object containing the underlying grid.
    grid = function(...) {
      check_change(self, "grid", ...)

      if (!is.null(private$.grid)) {
        # Clone grid object so that if grid is changed outside of the LPJmLData
        #   instance it will not change this instance. `deep = TRUE` because
        #   grid includes another R6 class object (meta) which is another
        #   environment.
        grid <- private$.grid$clone(deep = TRUE)

        return(grid)

      } else {
        # If NULL make sure NULL is returned directly and not tried to clone
        return(private$.grid)
      }
    }
  ),


  private = list(

    .meta = NULL,

    .data = NULL,

    .grid = NULL

  )
)

# Set up method dispatch ----------------------------------------------------- #

# https://stackoverflow.com/questions/50842251/define-a-bracket-operator-on-an-r6-class # nolint
# Add additional (important) functions for method dispatch which create a deep
# copy of x, execute function on copied object and return ("traditional way").


#' Add grid to an LPJmLData object
#'
#' Function to add a grid to an [`LPJmLData`] object. The function acts
#' as a [`read_io()`] wrapper for the grid file and adds it as an
#' `LPJmLData` object itself to the `$grid` attribute of the main object.
#'
#' @details
#' **Important:**
#' * If `"file_type" == "raw"` prescribe `variable = "grid"` to ensure data are
#'   recognized as a grid.
#' * Do not use [`read_io()`] argument `subset` here. `add_grid` will use the
#'   `subset` of the parent [`LPJmLData`] object `x`.
#'
#' @param x [LPJmLData] object.
#'
#' @param ... Arguments passed to [`read_io()`]. Without any arguments,
#'   `add_grid()` will search for a file name starting with "grid" in the same
#'   directory that `x` was loaded from. This supports grid files in `"meta"`
#'   and `"clm"` format. If the grid file is in `"raw"` format or should be
#'   loaded from a different directory, supply all necessary `read_io()`
#'   parameters.
#'
#' @return A copy of `x` ([`LPJmLData`] object) with added `$grid` attribute.
#'
#' @examples
#' \dontrun{
#'
#' # Read in vegetation carbon data with meta file
#' vegc <- read_io(filename = "./vegc.bin.json")
#'
#' # Add grid as attribute (via grid file in output directory)
#' vegc_with_grid <- add_grid(vegc)
#' }
#'
#' @md
#' @export
add_grid <- function(x, ...) {
  y <- x$clone(deep = TRUE)
  y$add_grid(...)
  y
}


# Utility functions ---------------------------------------------------------- #

# Aggregation function, only to be applied for conversions (as_raster, plot).
#   Do not apply to self to not violate data integrity !
aggregate_array <- function(x,
                            aggregate_list = NULL,
                            ...) {

  data <- x$data

  if (!is.null(aggregate_list)) {

    for (idx in seq_along(aggregate_list)) {
      idx_name <- names(aggregate_list)[idx]
      dims <- dim(data)
      dim_names <- names(dim(data))

      if (!idx_name %in% dim_names) {
        warning("Dimension ",
                col_var(idx_name),
                " does not exist.")
        next

      } else if (dims[idx_name] == 1) {
        data <- abind::adrop(data, idx_name)

      } else {
        data <- apply(X = data,
                      MARGIN = dim_names[!dim_names %in% idx_name],
                      FUN = aggregate_list[[idx]],
                      ...)
      }
    }
  }
  data
}
