#' @title LPJmL Grid data class
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
LPJmLGridData <- R6::R6Class( # nolint:object_name_linter

  classname = "LPJmLGridData",

  inherit = LPJmLData,

  public = list(

    # modify methods --------------------------------------------------------- #

    #' @description
    #' ! Not allowed to add a grid to an `LPJmLGridData` object.
    #'
    #' @param ... See [`add_grid()`].
    add_grid = function(...) {

      stop("Not allowed for an object of class LPJmLGridData")
    },


    #' @description
    #' ! Not allowed to use dimension names of `LPJmLGridData$data`
    #' array directly to subset each dimension to match the supplied vectors.
    #'
    #' @param ... See [`subset.LPJmLData()`]
    subset = function(...) {

      stop("Not allowed for an object of class LPJmLGridData")
    },


    #' @description
    #' ! Not allowed to transform inner `LPJmLGridData$data` array
    #' into another space or time format.
    #'
    #' @param ... See [`transform()`].
    transform = function(...) {

      stop("Not allowed for an object of class LPJmLGridData")
    },

    # export methods --------------------------------------------------------- #


    #' @description
    #' ! Not allowed to coerce (convert) an `LPJmLGridData` object into a
    #' \link[raster]{raster} or \link[raster]{brick} object.
    #'
    #' @param ... See [`as_raster()`].
    as_raster = function(...) {

      stop("Not allowed for an object of class LPJmLGridData")
    },


    #' @description
    #' ! Not allowed to coerce (convert) an `LPJmLGridData` object into a
    #' \link[terra]{rast} object.
    #'
    #' @param ... See [`as_terra()`].
    as_terra = function(...) {

      stop("Not allowed for an object of class LPJmLGridData")
    },


    #' @description
    #' ! Internal method only to be used for package development.
    #'
    #' @param ... See [`subset()`].
    .__subset_space__ = function(...) {
      private$.subset_space(...)
    },


    #' @description
    #' ! Internal method only to be used for package development.
    #'
    #' @param ... See [`transform()`].
    .__transform_space__ = function(...) {
      private$.transform_space(...)
    },


    # Create a new LPJmLGridData object only to be used internally or explicitly
    #' @description
    #' !Internal method only to be used for package development!
    #'
    #' @param lpjml_data LPJmLData object with variable `"grid"` or `"LPJGRID"`
    initialize = function(lpjml_data) {

      x <- lpjml_data$clone(deep = TRUE)
      x$transform(to = "cell")

      # Clone LPJmLMetaData data into meta attribute
      private$.meta <- x$meta
      # Assign LPJmLData data
      private$.data <- x$data


      if (!is.null(private$.meta$variable)) {

        if (private$.meta$variable == "grid") {
          private$init_grid()

        } else if (private$.meta$variable == "LPJGRID") {
          private$.meta$.__set_attribute__("variable", "grid")
          private$init_grid()
        } else {
          stop(
            paste0(
              "Invalid variable ",
              sQuote(variable),
              ". Supported variables are ",
              sQuote("grid"),
              " and ",
              sQuote("LPJGRID"),
              "."
            )
          )
        }
      }
    },


    #' @description
    #' Method to print the `LPJmLGridData`. \cr
    #' See also \link[base]{print}
    print = function() {

      unset_col <- "\u001b[0m"

      # Print LPJmLData class
      super$print()

      cat(paste0("\u001b[33;3m",
                 ifelse(private$.meta$._space_format_ == "cell",
                        "Note: only min & max printed as equivalent to spatial extent.", # nolint
                        "Note: inverted grid (cell as value)! Only min & max printed for sequence of cells."), # nolint
                 unset_col,
                 "\n"))
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

      # Update grid meta data
      self$.__set_data__(
        drop_omit(self$data, omit = "cell")
      )

      # Update grid meta data
      private$.meta$.__init_grid__()

      return(invisible(self))
    }
  )
)
