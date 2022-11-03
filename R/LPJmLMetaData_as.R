# convert to header object
# TODO: INSERT ROXYGEN DOC
as_header <- function(x, ...) {
  y <- x$as_header(...)
  return(y)
}

LPJmLMetaData$set(
  "public",
  "as_header",
  # TODO: INSERT ROXYGEN DOC
  function(silent = FALSE) {
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


# convert (useful) set fields as list
# TODO: INSERT ROXYGEN DOC
as_list <- function(x, ...) {
  y <- x$as_list(...)
  return(y)
}

LPJmLMetaData$set(
  "public",
  "as_list",
  # TODO: INSERT ROXYGEN DOC
  function() {
    self$fields_set %>%
      sapply(function(x) do.call("$", list(self, x)), simplify = FALSE) %>%
      return()
  }
)

# overwrite conversion of all set fields as list
# as.list.LPJmLMetaData <- function(obj, ...) obj$as_list(...)