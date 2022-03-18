#' Read a LPJmL meta file
#'
#' Reads a meta JSON file.
#'
#' @param filename character string representing path
#' (if differs from current working directory) and filename
#'
#' @return (nested) list object
#'
#' @examples
#' \dontrun{
#'  meta <- read_meta(filename = "mpft_npp.bin.json")
#'
#'  meta[[sim_name]]
#'  # [1] "LPJmL Run"
#'
#'  meta[[firstcell]]
#'  # [1] 27410
#'
#'  meta[[pft]][[1]]
#'  # [1] "tropical broadleaved evergreen tree"
#' }
#' @export

LpjmlMeta <- R6::R6Class(
  classname = "LpjmlMeta",
  lock_objects = FALSE,
  private = list(
    fields_set = c()
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
    initialize = function(meta_list) {
      for (idx in seq_along(meta_list)) {
        if (!names(meta_list[idx]) %in% names(LpjmlMeta$public_fields)) {
          warning(paste0(names(meta_list[idx]),
                         " may not be a valid LpjmlMeta field."))
        }
        do.call("$<-", list(self,
                            names(meta_list[idx]),
                            meta_list[[idx]]))
      }
      self$fields_set <- names(meta_list)
    },
    header = function() {
      create_header(
        name = "LPJ_OUT",
        version = 4,
        order = switch(self$order,
                       cellyear = 1,
                       yearcell = 2,
                       cellindex = 3,
                       cellseq = 4),
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
    }, list = function() {
        all_list <- as.list(self) %>%
          `[`(self$fields_set)
        return(all_list)
    }
  )
)
