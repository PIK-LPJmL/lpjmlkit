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
  public = list(
    # init function
    initialize = function(meta_list) {
      for (idx in seq_along(meta_list)) {
        # if (!names(meta_list[idx]) %in% names(LpjmlMetaData$public_fields)) {
        #   warning(paste0(names(meta_list[idx]),
        #                  " may not be a valid LpjmlMetaData field."))
        # }
        do.call("$<-", list(private,
                            paste0(".", names(meta_list[idx])),
                            meta_list[[idx]]))
      }
      private$.fields_set <- names(meta_list)
    },
    # update supplied subset_list in self.subset
    update_subset = function(subset_list) {
      # evtly TODO: if subset_list integer -> index of previus subset_list
      # TODO: convert from time to years/months/days and vice versa
      private$.subset[
        names(subset_list) %in% names(self$dimension_map)
      ] <- subset_list[
        names(subset_list) %in% names(self$dimension_map)
      ]
    },
    # convert to header object
    as_header = function() {
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
    # return fields set as list
    as_list = function() {
      self$fields_set %>%
        sapply(function(x) do.call("$", list(self, x)), simplify = FALSE) %>%
        return()
    },
    print = function(spaces = "") {
      blue_col <- "\u001b[34m"
      unset_col <- "\u001b[0m"
      meta_fields <- self$fields_set %>%
        sapply(function(x) do.call("$", list(self, x)),
               USE.NAMES = FALSE)
      to_char1 <- self$fields_set %>%
        sapply(function(x) {
          check <- do.call("$", list(self, x))
          if (is.character(check) & length(check) <= 1) {
            return("\"")
          } else {
            return("")
          }
        },
        USE.NAMES = FALSE)
      cat(
        paste0(spaces,
               blue_col,
               "$",
               self$fields_set,
               unset_col,
               " ",
               to_char1,
               meta_fields,
               to_char1,
               collapse = "\n")
      )
      cat("\n")
      if (!is.null(private$.subset)) {
        cat(paste0(spaces, blue_col, "$subset", unset_col, "\n"))
        for (sub in seq_along(private$.subset)) {
          to_char2 <- ifelse(is.character(private$.subset[[sub]]), "\"", "")
          if (length(private$.subset[[sub]]) > 30) {
            abbr_subset <- paste0(c(paste0(to_char2,
                                           private$.subset[[sub]][1:4],
                                           to_char2),
                                    "...",
                                    paste0(to_char2,
                                           tail(private$.subset[[sub]], n = 1),
                                           to_char2)))
          } else {
            abbr_subset <- paste0(to_char2, private$.subset[[sub]], to_char2)
          }
          cat(spaces,
              blue_col,
              paste0("$", names(private$.subset[sub])),
              unset_col,
              abbr_subset)
          cat("\n")
        }
      }
    }
  ),
  active = list(
    sim_name = function() {
      return(private$.sim_name)
    },
    source = function() {
      return(private$.source)
    },
    history = function() {
      return(private$.history)
    },
    variable = function() {
      return(private$.variable)
    },
    firstcell = function() {
      return(private$.firstcell)
    },
    ncell = function() {
      return(private$.ncell)
    },
    cellsize_lon = function() {
      return(private$.cellsize_lon)
    },
    cellsize_lat = function() {
      return(private$.cellsize_lat)
    },
    nstep = function() {
      return(private$.nstep)
    },
    timestep = function() {
      return(private$.timestep)
    },
    nbands = function() {
      return(private$.nbands)
    },
    band_names = function() {
      return(private$.band_names)
    },
    descr = function() {
      return(private$.descr)
    },
    unit = function() {
      return(private$.unit)
    },
    firstyear = function() {
      return(private$.firstyear)
    },
    lastyear = function() {
      return(private$.lastyear)
    },
    nyear = function() {
      return(private$.nyear)
    },
    datatype = function() {
      return(private$.datatype)
    },
    scalar = function() {
      return(private$.scalar)
    },
    order = function() {
      return(private$.order)
    },
    bigendian = function() {
      return(private$.bigendian)
    },
    format = function() {
      return(private$.format)
    },
    filename = function() {
      return(private$.filename)
    },
    subset = function() {
      return(private$.subset)
    },
    fields_set = function() {
      return(private$.fields_set)
    },
    dimension_map = function() {
      return(private$.dimension_map)
    }
  ),
  private = list(
    .sim_name = NULL,
    .source = NULL,
    .history = NULL,
    .variable = NULL,
    .firstcell = NULL,
    .ncell = NULL,
    .cellsize_lon = NULL,
    .cellsize_lat = NULL,
    .nstep = NULL,
    .timestep = NULL,
    .nbands = NULL,
    .band_names = NULL,
    .descr = NULL,
    .unit = NULL,
    .firstyear = NULL,
    .lastyear = NULL,
    .nyear = NULL,
    .datatype = NULL,
    .scalar = NULL,
    .order = NULL,
    .bigendian = FALSE,
    .format = NULL,
    .filename = NULL,
    .subset = NULL,
    .fields_set = NULL,
    .dimension_map = list(cells = "cell",
                         time = c("year", "month", "day"),
                         year = "time",
                         month = "time",
                         day = "time",
                         band = "band")
  )
)

as.list.LpjmlMetaData <- function(obj, ...) obj$as_list(...)
