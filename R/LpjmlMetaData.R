#' LPJmL meta output class
#'
#' Handles metafile data for output data
#'
#' @param x list (not nested) with meta data
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
    initialize = function(x, subset_list=list()) {
      if (all(names(x) %in% c("name", "header", "endian"))) {
        header_to_meta <- as.list(x$header)[
          which(!names(x$header) %in% c("version"))
        ] %>%
          append(list(
            "bigendian" = ifelse(x$endian == "big", TRUE, FALSE),
            # "descr" = tolower(x$name),
            "lastyear" = x$header[["firstyear"]] +
                         x$header[["timestep"]] *
                         (x$timestep[["nyear"]] - 1)
          )) %>%
        `[[<-`("order",
               switch(as.character(order),
                      `1` = "cellyear",
                      `2` = "yearcell",
                      `3` = "cellindex",
                      `4` = "cellseq",
                      stop(paste("Invalid order string", sQuote(order)))))
        private$init_list(header_to_meta)
      } else {
        private$init_list(x)
      }
      if (length(subset_list) > 0) {
        self$update_subset(subset_list)
      }
    },
    # update supplied subset_list in self.subset
    update_subset = function(subset_list) {
      # update cell fields - distinguish between character -> LPJmL C index
      #   starting from 0! and numeric/integer -> R index starting from 1 -> -1
      if (!is.null(subset_list$cell)) {
        if (is.character(subset_list$cell)) {
          private$.firstcell <- min(as.integer(subset_list$cell))
        } else {
          private$.firstcell <- min(as.integer(subset_list$cell - 1))
        }
        private$.ncell <- length(subset_list$cell)
        private$.subset <- TRUE
      }
      # for years using indices is forbidded because they cannot be properly
      #   distinguished from years
      if (!is.null(subset_list$year)) {
        private$.firstyear <- min(as.integer(subset_list$year))
        private$.lastyear <- max(as.integer(subset_list$year))
        private$.nyear <- length(subset_list$year)
        private$.subset <- TRUE
      }
      # band can be subsetted via indices or band_names - the latter is updated
      if (!is.null(subset_list$band)) {
        if (is.character(subset_list$band)) {
          if (subset_list$band %in% private$.band_names) {
            warning(paste0(
              "Not all subset_list bands are represented in the data.",
              "Resulting meta data band_names may be incorrect"
            ))
          }
          private$.band_names <- private$.band_names[
            private$.band_names %in% subset_list$band
          ]
        } else {
          private$.band_names <- private$.band_names[subset_list$band]
        }
        private$.subset <- TRUE
      }
      # if (!is.null(subset_list$day)) {
      # }
      # if (!is.null(subset_list$month)) {
      # }
      # if (!is.null(subset_list$time)) {
      # }
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
      if (!private$.subset) {
        cat(
          paste0(
            spaces, blue_col, "$subset", unset_col, " ", private$.subset, "\n"
          )
        )
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
    init_list = function(x) {
      for (idx in seq_along(x)) {
        # if (!names(x[idx]) %in% names(LpjmlMetaData$public_fields)) {
        #   warning(paste0(names(x[idx]),
        #                  " may not be a valid LpjmlMetaData field."))
        # }
        if (names(x[idx]) == "band_names") {
          x[[idx]] <- as.character(x[[idx]])
        }
        do.call("$<-", list(private,
                            paste0(".", names(x[idx])),
                            x[[idx]]))
        private$.fields_set <- names(x)
      }
    },
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
    .subset = FALSE,
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
