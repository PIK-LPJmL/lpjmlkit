' @title LPJmL data class
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

  inherit = LPJmLData,

  public = list(

    # Create a new LPJmLGridData object only to be used internally or explicitly
    #' @description
    #' !Internal method only to be used for package development!
    #'
    #' @param lpjml_data LPJmLData object with variable `"grid"` or `"LPJGRID"`
    initialize = function(lpjml_data) {

      # clone LPJmLMetaData data into meta attribute
      private$.meta <- lpjml_data$meta_data
      # assign LPJmLData data
      private$.data <- lpjml_data$data


      if (!is.null(private$.meta$variable)) {

        if (private$.meta$variable == "grid") {
          private$init_grid()

        } else if (private$.meta$variable == "LPJGRID") {
          private$.meta$.__set_attribute__("variable", "grid")
          private$init_grid()
        }
      }
    },


    #' @description
    #' Method to print the `LPJmLData`. \cr
    #' See also \link[base]{print}
    print = function() {

      # print LPJmLData class
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

    # init grid if variable == "grid"
    init_grid = function() {

      # update grid data
      if (dim(private$.data)[["band"]] == 2) {
        dimnames(private$.data)[["band"]] <- c("lon", "lat")
      } else {
        stop("Unknown number of bands for grid initialization.")
      }

      # update grid meta data
      self$.__set_data__(
        drop_omit(self$data, omit = "cell")
      )

      private$.meta$.__init_grid__()

      return(invisible(self))
    }
  )
)
