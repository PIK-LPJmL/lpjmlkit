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
    initialize = function(x,
                          subset_list = list(),
                          additional_data = list(),
                          data_dir = NULL) {
      if (all(names(x) %in% c("name", "header", "endian"))) {
        header_to_meta <- as.list(x$header) %>%
          append(list(
            "bigendian" = ifelse(x$endian == "big", TRUE, FALSE),
            # "descr" = tolower(x$name),
            "lastyear" = x$header[["firstyear"]] +
                         x$header[["timestep"]] *
                         (x$header[["nyear"]] - 1),
            "name" = ifelse(is.null(x$name), "LPJDUMMY", x$name)
          )) %>%
        `[[<-`("order",
               switch(as.character(.$order),
                      `1` = "cellyear",
                      `2` = "yearcell",
                      `3` = "cellindex",
                      `4` = "cellseq",
                      stop(
                        paste(
                          "Invalid order value", sQuote(.$order), "in header"
                        )
                      )
                     )
              )
        
        private$init_list(header_to_meta, additional_data)
      } else {
        private$init_list(x, additional_data)
      }
      if (length(subset_list) > 0) {
        self$._update_subset(subset_list)
      }
      # add data_dir for lazy loading of (e.g.) grid later
      if (!is.null(data_dir)) {
        private$.data_dir <- data_dir
      }
    },
    # update supplied subset_list in self.subset
    ._update_subset = function(subset_list, time_dimnames = NULL) {
      is_sequential <- function(x) all(diff(as.integer(x)) == 1)
      # update cell fields - distinguish between character -> LPJmL C index
      #   starting from 0! and numeric/integer -> R index starting from 1 -> -1
      if (!is.null(subset_list$cell)) {
        if (is_sequential(subset_list$cell)) {
          if (is.character(subset_list$cell)) {
            private$.firstcell <- min(as.integer(subset_list$cell))
          } else {
            private$.firstcell <- (private$.firstcell +
                                   min(subset_list$cell - 1))
          }
        } else {
          private$.firstcell <- NULL
        }
        private$.ncell <- length(subset_list$cell)
        private$.subset <- TRUE
        private$.subset_spatial <- TRUE
      }
      # for years using indices is forbidded because they cannot be properly
      #   distinguished from years
      if (!is.null(subset_list[["time"]]) && !is.null(time_dimnames)) {
        subset_list[["year"]] <- split_time_names(time_dimnames)[["year"]]
      }
      if (!is.null(subset_list$year)) {
        private$.firstyear <- min(as.integer(subset_list$year))
        private$.lastyear <- max(as.integer(subset_list$year))
        private$.nyear <- length(subset_list$year)
        private$.subset <- TRUE
      }
      # band can be subsetted via indices or band_names - the latter is updated
      if (!is.null(subset_list$band)) {
        if (is.character(subset_list$band)) {
          if (!all(subset_list$band %in% private$.band_names)) {
            warning(paste0(
              "Not all subset_list bands are represented in the original data:",
              "\n- band_names provided to subset_list may be incorrect, or",
              "\n- new names have been provided by the user to band_names."
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

    },
    # convert to header object
    as_header = function(silent = FALSE) {
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
    },
    ._init_grid = function() {
      if (private$.variable != "grid") {
        stop(paste("Only valid for variable", sQuote("grid"), "."))
      }
      # set all time fields to NULL
      private$.nyear <- NULL
      private$.firstyear <- NULL
      private$.lastyear <- NULL
      private$.nstep <- NULL
      private$.timestep <- NULL
      # update fields_set
      private$.fields_set <- private$.fields_set[
        -na.omit(match(c("nyear",
                         "firstyear",
                         "lastyear",
                         "nstep",
                         "timestep",
                         "scalar"),
                       private$.fields_set))
      ]
    },
    # return fields set as list
    as_list = function() {
      self$fields_set %>%
        sapply(function(x) do.call("$", list(self, x)), simplify = FALSE) %>%
        return()
    },
    check = function() {
      print_fields <- self$fields_set
      meta_fields <- print_fields %>%
        sapply(function(x) do.call("$", list(self, x)),
               USE.NAMES = FALSE) %>%
      return(meta_fields)
    },
    ._convert_dimtime_format = function(dimtime_format) {
      private$.dimtime_format <- dimtime_format
    },
    ._convert_dimspatial_format = function(dimspatial_format) {
      private$.dimspatial_format <- dimspatial_format
    },
    print = function(all = TRUE, spaces = "") {
      quotes_option <- options(useFancyQuotes = FALSE)
      on.exit(options(quotes_option))
      if (!all) {
        print_fields <- self$fields_set %>%
          `[`(-stats::na.omit(match(private$exclude_print(), .)))
      } else {
        print_fields <- self$fields_set
      }
      # colorize self print
      blue_col <- "\u001b[34m"
      unset_col <- "\u001b[0m"
      meta_fields <- print_fields %>%
        sapply(function(x) do.call("$", list(self, x)),
               USE.NAMES = FALSE)
      to_char1 <- print_fields %>%
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
               print_fields,
               unset_col,
               " ",
               to_char1,
               lapply(meta_fields, function(x) {
                 if (length(x) > 1) {
                   # print vectors as output not as c(...)
                   if (length(x) > 6 && is.character(x)) {
                     # shorten character vectors
                     x <- c(x[1:4], "...", tail(x, n = 1))
                   }
                   if (is.character(x)) {
                     # quotes only around each element not around vector
                     return(noquote(paste(dQuote(x), collapse = " ")))
                   } else {
                     # no quotes for numeric vectors
                     return(noquote(paste(x, collapse = " ")))
                   }
                 } else {
                   return(x)
                 }
               }),
               to_char1,
               collapse = "\n")
      )
      cat("\n")
      cat(
        paste0(
          spaces,
          # color red if subset
          blue_col,
          "$subset",
          unset_col,
          " ",
          # color red if subset
          ifelse(self$subset, "\u001b[31m", ""),
          self$subset,
          ifelse(self$subset, unset_col, ""),
          "\n"
        )
      )
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
    descr = function() {
      return(private$.descr)
    },
    unit = function() {
      return(private$.unit)
    },
    nbands = function() {
      return(private$.nbands)
    },
    band_names = function() {
      return(private$.band_names)
    },
    nyear = function() {
      return(private$.nyear)
    },
    firstyear = function() {
      return(private$.firstyear)
    },
    lastyear = function() {
      return(private$.lastyear)
    },
    nstep = function() {
      return(private$.nstep)
    },
    timestep = function() {
      return(private$.timestep)
    },
    ncell = function() {
      return(private$.ncell)
    },
    firstcell = function() {
      return(private$.firstcell)
    },
    cellsize_lon = function() {
      return(private$.cellsize_lon)
    },
    cellsize_lat = function() {
      return(private$.cellsize_lat)
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
    offset = function() {
      return(private$.offset)
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
    map = function() {
      return(private$.map)
    },
    version = function() {
      return(private$.version)
    },
    subset = function() {
      if (!is.null(self$variable) && self$variable == "grid") {
        return(private$.subset_spatial)
      } else {
        return(private$.subset)
      }
    },
    subset_spatial = function() {
      return(private$.subset_spatial)
    },
    fields_set = function() {
      return(private$.fields_set)
    },
    data_dir = function() {
      return(private$.data_dir)
    },
    dimtime_format = function() {
      return(private$.dimtime_format)
    },
    dimspatial_format = function() {
      return(private$.dimspatial_format)
    },
    dimension_map = function() {
      return(private$.dimension_map)
    }
  ),
  private = list(
    init_list = function(x, additional_data = list()) {
      for (name_id in private$.name_order) {
        # if (!names(x[idx]) %in% names(LpjmlMetaData$public_fields)) {
        #   warning(paste0(names(x[idx]),
        #                  " may not be a valid LpjmlMetaData field."))
        # }
        if (is.null(x[[name_id]])) {
          if (name_id %in% names(additional_data)) {
            x[[name_id]] <- additional_data[[name_id]]
          } else {
            next
          }
        }
        if (!name_id %in% private$.fields_set) {
          if (name_id == "band_names") {
            x[[name_id]] <- as.character(x[[name_id]])
          }
          do.call("$<-", list(private,
                              paste0(".", names(x[name_id])),
                              x[[name_id]]))
          # Do not add "name" attribute to field_set as it is only saved
          # internally for conversion back to header.
          if (name_id != "name")
            private$.fields_set <- append(private$.fields_set, name_id)
        }
      }
    },
    exclude_print = function() {
      # exclude entries from self print (for LpjmlData class)
      to_exclude <- c(
        "band_names",
        "firstyear",
        "lastyear",
        "firstcell",
        "datatype",
        "format",
        "bigendian",
        "order",
        "history",
        "source",
        "filename"
      ) %>%
      # only append scalar if != 1
      append(
        ifelse(
          !is.null(private$.scalar),
          ifelse(private$.scalar == 1, "scalar", NA),
          NA
        )
      ) %>%
      # workaround to deal with NAs (NULL not possible in ifelse)
      stats::na.omit() %>%
      as.vector() %>%
      return()
    },
    .sim_name = NULL,
    .source = NULL,
    .history = NULL,
    .variable = NULL,
    .descr = NULL,
    .unit = NULL,
    .nbands = NULL,
    .band_names = NULL,
    .nyear = NULL,
    .firstyear = NULL,
    .lastyear = NULL,
    .nstep = NULL,
    .timestep = NULL,
    .ncell = NULL,
    .firstcell = NULL,
    .cellsize_lon = NULL,
    .cellsize_lat = NULL,
    .datatype = NULL,
    .scalar = NULL,
    .order = NULL,
    .bigendian = FALSE,
    .format = NULL,
    .filename = NULL,
    .version = NULL,
    .offset = NULL,
    .name = NULL,
    .map = NULL,
    .subset = FALSE,
    .subset_spatial = FALSE,
    .fields_set = NULL,
    .data_dir = NULL,
    .dimtime_format = "time",
    .dimspatial_format = "cell",
    .name_order = c("sim_name",
                    "source",
                    "history",
                    "variable",
                    "descr",
                    "unit",
                    "nbands",
                    "band_names",
                    "nyear",
                    "firstyear",
                    "lastyear",
                    "nstep",
                    "timestep",
                    "ncell",
                    "firstcell",
                    "cellsize_lon",
                    "cellsize_lat",
                    "datatype",
                    "scalar",
                    "order",
                    "bigendian",
                    "format",
                    "filename",
                    "name",
                    "map",
                    "version",
                    "offset"
                   ),
    .dimension_map = list(cells = "cell",
                         time = c("year", "month", "day"),
                         year = "time",
                         month = "time",
                         day = "time",
                         band = "band")
  )
)

as.list.LpjmlMetaData <- function(obj, ...) obj$as_list(...)
