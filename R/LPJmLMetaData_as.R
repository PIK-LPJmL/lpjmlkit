#' Coerce LPJmLMetaData to an LPJmL header object
#'
#' Function to coerce (convert) an [`LPJmLMetaData`] object into an
#' LPJmL header object. More information at [`create_header()`].
#'
#' @param x An [LPJmLMetaData] object
#'
#' @param silent Logical. Whether to suppress notifications from header
#' conversion/initialization.
#'
#' @return An LPJmL header object. More information at [`create_header()`].
#'
#' @examples
#' \dontrun{
#'
#' vegc_meta <- read_meta(filename = "./vegc.bin.json")
#'
#' # Returns a list object with the structure of an LPJmL header
#' as_header(vegc_meta)
#' # $name
#' # [1] "LPJDUMMY"
#' #
#' # $header
#' #      version        order    firstyear        nyear    firstcell
#' #          4.0          4.0       1901.0        200.0          0.0
#' #        ncell       nbands cellsize_lon       scalar cellsize_lat
#' #      67420.0          1.0          0.5          1.0          0.5
#' #     datatype        nstep     timestep
#' #          3.0          1.0          1.0
#' #
#' # $endian
#' # [1] "little"
#'
#' }
#'
#' @md
#' @export
as_header <- function(x,
                      silent = FALSE) {
  y <- x$as_header(silent)
  return(y)
}

LPJmLMetaData$set(
  "private",
  ".as_header",
  # as_header method roxygen documentation in LPJmlMetaData.R
  function(silent = FALSE) {

    header <- create_header(
      name = ifelse(is.null(private$.name), "LPJDUMMY", private$.name),
      version = ifelse(is.null(private$.version), 4, private$.version),
      order = ifelse(is.null(self$order), 1, self$order),
      firstyear = ifelse(is.null(self$firstyear), 1901, self$firstyear),
      firstcell = self$firstcell,
      nyear = ifelse(is.null(self$nyear), 1, self$nyear),
      ncell = self$ncell,
      nbands = self$nbands,
      cellsize_lon = self$cellsize_lon,
      cellsize_lat = self$cellsize_lat,
      scalar =  ifelse(is.null(self$scalar), 1.0, self$scalar),
      datatype = self$datatype,
      nstep = self$nstep,
      timestep = ifelse(is.null(self$timestep), 1, self$timestep),
      endian = ifelse(self$bigendian, "big", "little"),
      verbose = !silent
    )

    return(header)
  }
)


#' Coerce LPJmLMetaData to a list
#'
#' Function to coerce (convert) an [`LPJmLMetaData`] object into a
#' \link[base]{list}.
#'
#' @param x An [LPJmLMetaData] object
#'
#' @return A \link[base]{list}
#'
#' @examples
#' \dontrun{
#'
#' vegc_meta <- read_meta(filename = "./vegc.bin.json")
#'
#' # Returns one dimensional array with timeseries for cells `27410:27415`
#' as_list(vegc_meta)
#' # $sim_name
#' # [1] "lu_cf"
#' #
#' # $source
#' # [1] "LPJmL C Version 5.3.001"
#' #
#' # $variable
#' # [1] "vegc"
#' #
#' # $descr
#' # [1] "vegetation carbon"
#' #
#' # $unit
#' # [1] "gC/m2"
#' #
#' # $nbands
#' # [1] 1
#' #
#' # ...
#'
#' }
#'
#' @md
#' @export
as_list <- function(x) {
  y <- x$as_list()
  return(y)
}

LPJmLMetaData$set(
  "private",
  ".as_list",
  # as_header method roxygen documentation in LPJmlMetaData.R
  function() {
    self$._fields_set_ %>%

      sapply(function(x) do.call("$", list(self, x)), simplify = FALSE) %>% # nolint:undesirable_function_linter.
      return()
  }
)
