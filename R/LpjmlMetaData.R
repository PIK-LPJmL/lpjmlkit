#' LPJmL meta output class
#'
#' Handles metafile data for output data
#'
#' @param meta_list list (not nested) with meta data
#'
#' @return LpjmlMetaData object
#'
#' @examples
#' \dontrun{
#' }
#' @export
# https://adv-r.hadley.nz/r6.html#r6-classes, also why CamelCase is used ...
LpjmlMetaData <- R6::R6Class(
  classname = "LpjmlMetaData",
  lock_objects = FALSE,
  private = list(
    .__meta_fields_set__ = NULL
  ),
  public = list(
    sim_name = NULL,
    source = NULL,
    history = NULL,
    variable = NULL,
    firstcell = NULL,
    ncell = NULL,
    cellsize_lon = NULL,
    cellsize_lat = NULL,
    nstep = NULL,
    timestep = NULL,
    nbands = NULL,
    band_names = NULL,
    descr = NULL,
    unit = NULL,
    firstyear = NULL,
    lastyear = NULL,
    nyear = NULL,
    datatype = NULL,
    scalar = NULL,
    order = NULL,
    bigendian = FALSE,
    format = NULL,
    filename = NULL,

    # init function
    initialize = function(meta_list) {
      for (idx in seq_along(meta_list)) {
        if (!names(meta_list[idx]) %in% names(LpjmlMetaData$public_fields)) {
          warning(paste0(names(meta_list[idx]),
                         " may not be a valid LpjmlMetaData field."))
        }
        do.call("$<-", list(self,
                            names(meta_list[idx]),
                            meta_list[[idx]]))
      }
      self$.__meta_fields_set__ <- names(meta_list)
    },

    # convert to header object
    header = function() {
      create_header(
        name = "LPJ_OUT",
        version = 4,
        order = self$order,
        firstyear = self$firstyear,
        nyear = self$nyear,
        ncell = self$ncell,
        nbands = self$nbands,
        cellsize_lon = self$cellsize_lon,
        cellsize_lat = self$cellsize_lat,
        scalar =  self$scalar,
        datatype = self$datatype,
        nstep = self$nstep,
        timestep = self$timestep,
        endian = ifelse(self$bigendian, "big", "little"),
        verbose = TRUE
      )
    },
    fields = function() {
      names(LpjmlMetaData$public_fields)
    },
    # return fields set as list
    list = function() {
      all_list <- as.list(self) %>%
        `[`(self$.__meta_fields_set__)
      return(all_list)
    },
    print = function() {
      blue_col <- "\u001b[34m"
      unset_col <- "\u001b[0m"
      meta_fields <- unlist(as.list(self)[self$.__meta_fields_set__])
      cat("Meta Data:", "\n")
      cat(
        paste0("  ",
               blue_col,
               names(meta_fields),
               unset_col,
               " ",
               meta_fields,
               collapse = "\n")
      )
      cat("\n")
    }
  )
)
