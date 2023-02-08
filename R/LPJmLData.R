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

      # Check for locked objects
      check_method_locked(self, "add_grid")
      
      if (...length() == 0) {
        # If user has not supplied any parameters try to find a grid file in the
        # same directory as data. This throws an error if no suitable file is
        # found.
        filename <- find_gridfile(private$.meta$._data_dir_)

        # Add support for cell subsets. This is a rough filter since $subset
        #   does not say if cell is subsetted - but ok for now.
        if (private$.meta$._subset_space_) {
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

        # All arguments have to be provided manually to read_io.
        #   Ellipsis (...) does that.

        # Add support for cell subsets. This is a rough filter since $subset
        #   does not say if cell is subsetted - but ok for now.
        if (private$.meta$._subset_space_) {
          self$.__set_grid__(
            read_io(...,
                    subset = list(cell = self$dimnames()[["cell"]]))
          )
        } else {
          self$.__set_grid__(read_io(...))
        }

      }

      private$.grid$.__set_lock__(is_locked = TRUE)
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

      # Set color higlighting
      blue_col <- "\u001b[34m"
      unset_col <- "\u001b[0m"

      # Print meta data
      cat(paste0("\u001b[1m", blue_col, "$meta %>%", unset_col, "\n"))
      private$.meta$print(all = FALSE, spaces = "  .")

      # Not all meta data are printed
      cat(paste0("\u001b[33;3m",
                 "Note: not printing all meta data, use $meta to get all.",
                 unset_col,
                 "\n"))

      # Print grid only if available
      if (!is.null(private$.grid)) {
        cat(paste0("\u001b[1m\u001b[31m",
                   "$grid",
                   unset_col,
                   "\u001b[31m",
                   " ...",
                   unset_col,
                   "\n"))
      }

      # Print data attribute
      cat(paste0("\u001b[1m",
                 blue_col,
                 "$data %>%",
                 unset_col,
                 "\n"))

      # Dimnames
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

      # Summary
      cat(paste0(blue_col, "$summary()", unset_col, "\n"))
      print(self$summary(cutoff = TRUE))

      if (is.null(private$.meta$variable) ||
      private$.meta$variable != "grid") {
        cat(paste0("\u001b[33;3m",
                   "Note: summary is not weighted by grid area.",
                   unset_col,
                   "\n")
        )

      } else {
        cat(paste0("\u001b[33;3m",
                   ifelse(private$.meta$._space_format_ == "cell",
                          "Note: only min & max printed as equivalent to spatial extent.", # nolint
                          "Note: inverted grid (cell as value)! Only min & max printed for sequence of cells."), # nolint
                   unset_col,
                   "\n"))
      }
    },


    #' @description
    #' !Internal method only to be used for package development!
    #'
    #' @param ... See [`transform()`].
    .__transform_grid__ = function(...) {
      private$.transform_grid(...)
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
      private$.grid <- grid
    },


    # Set is_locked attribute; only to be used internally or explicitly
    #   on purpose
    #' @description
    #' !Internal method only to be used for package development!
    #'
    #' @param is_locked Bolean.
    .__set_lock__ = function(is_locked) {
      private$.is_locked <- is_locked
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
          methods::is(meta_data, "NULL")) {
        private$.meta <- meta_data
      } else {
        stop("Provide an LPJmLMetaData object for meta data.")
      }

      private$.data <- data

      if (!is.null(private$.meta$variable)) {

        if (private$.meta$variable == "grid") {
          private$init_grid()

        } else if (private$.meta$variable == "LPJGRID") {
          private$.meta$.__set_attribute__("variable", "grid")
          private$init_grid()
        }
      }
    }
  ),


  # Active bindings
  active = list(

    #' @field meta [`LPJmLMetaData`] object to store corresponding meta data.
    meta = function() {
      # Clone meta object so that if meta is changed outside of the LPJmLData
      #   instance it will not change this instance
      return(private$.meta$clone())
    },

    #' @field data \link[base]{array} containing the underlying data.
    data = function() {
      return(private$.data)
    },

    #' @field grid Optional `LPJmLData` object containing the underlying grid.
    grid = function() {

      if (!is.null(private$.grid)) {

        # Clone grid object so that if grid is changed outside of the LPJmLData
        #   instance it will not change this instance. `deep = TRUE` because
        #   grid includes another R6 class object (meta) which is another
        #   environment.
        grid <- private$.grid$clone(deep = TRUE)

        # Allow using methods on grid outside of LPJmLData instance
        grid$.__set_lock__(is_locked = FALSE)

        return(grid)

      } else {
        # If NULL make sure NULL is returned directly and not tried to clone
        return(private$.grid)
      }
    },

    #' @field ._is_locked_ *Internal* logical. If an object is locked no method
    #'   can be performed directly on the object.
    ._is_locked_ = function() {
      return(private$.is_locked)
    }
  ),


  private = list(

    # Init grid if variable == "grid"
    init_grid = function() {

      # Update grid data
      if (dim(private$.data)[["band"]] == 2) {
        dimnames(private$.data)[["band"]] <- c("lon", "lat")
      } else {
        stop("Unknown number of bands for grid initialization.")
      }

      self$.__set_data__(
        drop_omit(self$data, omit = "cell")
      )

      # Update grid meta data
      private$.meta$.__init_grid__()

      return(invisible(self))
    },

    .meta = NULL,

    .data = NULL,

    .grid = NULL,

    .is_locked = FALSE
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
#'
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
        warning(paste0("\u001b[0m",
                       "Dimension ",
                       "\u001b[34m",
                       idx_name,
                       "\u001b[0m",
                       " does not exist."))
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

#' Search for a grid file in a directory
#'
#' Function to search for a grid file in a specific directory.
#'
#' @param searchdir Directory where to look for a grid file.
#' @return Character string with the file name of a grid file upon success.
#'   Function fails if no matching grid file can be detected.
#'
#' @details This function looks for file names in `searchdir` that match the
#'   `pattern` parameter in its [`list.files()`] call. Files of type "meta" are
#'   preferred. Files of type "clm" are also accepted. The function returns an
#'   error if no suitable file or multiple files are found. Otherwise, the file
#'   name of the grid file including the full path is returned.
#' @noRd
find_gridfile <- function(searchdir) {
  # The pattern will match any file name that starts with "grid*".
  # Alternative stricter pattern: pattern = "^grid(\\.[[:alpha:]]{3,4})+$"
  # This will only match file names "grid.*", where * is one or two file
  # extensions with 3 or 4 characters, e.g. "grid.bin" or "grid.bin.json".
  grid_files <- list.files(
    path = searchdir,
    pattern = "^grid",
    full.names = TRUE
  )
  if (length(grid_files) > 0) {
    grid_types <- sapply(grid_files, detect_type) # nolint:undesirable_function_linter.
    # Prefer "meta" file_type if present
    if (length(which(grid_types == "meta")) == 1) {
      filename <- grid_files[match("meta", grid_types)]
    } else if (length(which(grid_types == "clm")) == 1) {
      # Second priority "clm" file_type
      filename <- grid_files[match("clm", grid_types)]
    } else {
      # Stop if either multiple files per file type or not the right type have
      # been detected
      stop(
        "Cannot detect grid file automatically.\n",
        "$add_grid has to be called supplying parameters as for read_io."
      )
    }
  } else {
    # Stop if no file name matching pattern detected
    stop(
      "Cannot detect grid file automatically.\n",
      "$add_grid has to be called supplying parameters as for read_io."
    )
  }

  filename
}

# Avoid note for "."...
utils::globalVariables(".") # nolint:undesirable_function_linter
