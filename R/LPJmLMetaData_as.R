#' Coerce LPJmLMetaData to a LPJmL header object
#'
#' Function to coerce (convert) a `LPJmLMetaData` object into a
#' LPJmL header object, more information at [`create_header`].
#'
#' @param x [LPJmLMetaData] object
#'
#' @param silent Logical. Whether to suppress notifications from header
#' conversion/initialization.
#'
#' @return a LPJmL header object, more information at [`create_header`]
#'
#' @examples
#' \dontrun{
#'
#' vegc_meta <- read_meta(filename = "./vegc.bin.json")
#'
#' # returns one dimensional array with timeseries for cells `27410:27415`
#' as_header(vegc_meta)
#' # $name
#' # [1] "LPJDUMMY"
#' #
#' # $header
#' #      version        order    firstyear        nyear    firstcell
#' #          4.0          4.0       1901.0        200.0          0.0
#' #       nbands cellsize_lon       scalar cellsize_lat     datatype
#' #          1.0          0.5          1.0          0.5          3.0
#' #     timestep
#' #          1.0
#' #
#' # $endian
#' # [1] "little"
#'
#' }
#'
#' @md
#' @export
as_header <- function(x,
                      silent = TRUE) {
  y <- x$as_header(silent)
  return(y)
}

LPJmLMetaData$set(
  "private",
  ".as_header",
  # as_header method roxygen documentation in LPJmlMetaData.R
  function(silent = TRUE) {
    invisible(
      capture.output(
        header <- create_header(
          name = ifelse(is.null(private$.name), "LPJDUMMY", private$.name),
          version = ifelse(is.null(private$.version), 4, private$.version),
          order = ifelse(is.null(self$order), 1, self$order),
          firstyear = ifelse(is.null(self$firstyear), 1901, self$firstyear),
          firstcell = self$firstcell,
          nyear = self$nyear,
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
      )
    )
    return(header)
  }
)


#' Coerce LPJmLMetaData to a list
#'
#' Function to coerce (convert) a `LPJmLMetaData` object into a
#' \link[base]{list}.
#'
#' @param x [LPJmLMetaData] object
#'
#' @return a \link[base]{list}
#'
#' @examples
#' \dontrun{
#'
#' vegc_meta <- read_meta(filename = "./vegc.bin.json")
#'
#' # returns one dimensional array with timeseries for cells `27410:27415`
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
      sapply(function(x) do.call("$", list(self, x)), simplify = FALSE) %>%
      return()
  }
)
