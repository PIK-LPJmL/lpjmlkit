#' @title LPJmL meta output class
#'
#' @description Handles metafile data for output data
#'
LPJmLMetaData <- R6::R6Class(
  classname = "LPJmLMetaData",
  lock_objects = TRUE,
  public = list(
    # export methods --------------------------------------------------------- #

    #' @description
    #' Method to coerce (convert) a `LPJmLMetaData` object into a
    #' \link[base]{list}. \cr
    #' See also [`as_list`]
    as_list = function() {
      private$.as_list()
    },

    #' @description
    #' Method to coerce (convert) a `LPJmLMetaData` object into a LPJmL input
    #' header (more info at [`create_header`]). \cr
    #'
    #' @param ... See [`as_header`]
    as_header = function(...) {
      private$.as_header(...)
    },

    #' @description
    #' Method to print a `LPJmLMetaData` object.
    #' See also \link[base]{print}
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
    },

    # initialize grid meta data
    .__init_grid__ = function() {
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
                         "timestep"),
                       private$.fields_set))
      ]
    },

    # update supplied subset in self.subset
    .__update_subset__ = function(subset,
                               time_dimnames = NULL,
                               cell_dimnames = NULL) {
      is_sequential <- function(x) all(diff(as.integer(x)) == 1)
      # update cell fields - distinguish between character -> LPJmL C index
      #   starting from 0! and numeric/integer -> R index starting from 1 -> -1
      if (!is.null(subset$cell) ||
          !is.null(subset$lon) || !is.null(subset$lat)) {
        # recalculate firstcell and ncell if subsetted by lat and/or lon
        if (!is.null(cell_dimnames)) {
          if (is_sequential(cell_dimnames)) {
            private$.firstcell <- min(as.numeric(cell_dimnames))
          } else {
            firstcell <- NULL
          }
          private$.ncell <- length(cell_dimnames)
        } else {
          private$.firstcell <- NA
          private$.ncell <- NA
        }
        private$.subset <- TRUE
        private$.subset_space <- TRUE
      }
      # for years using indices is forbidded because they cannot be properly
      #   distinguished from years
      if (!is.null(subset[["time"]]) && !is.null(time_dimnames)) {
        subset[["year"]] <- split_time_names(time_dimnames)[["year"]]
      }
      if (!is.null(subset$year)) {
        private$.firstyear <- min(as.integer(subset$year))
        private$.lastyear <- max(as.integer(subset$year))
        private$.nyear <- length(subset$year)
        private$.subset <- TRUE
      }
      # band can be subsetted via indices or band_names - the latter is updated
      if (!is.null(subset$band)) {
        if (is.character(subset$band)) {
          if (!all(subset$band %in% private$.band_names)) {
            warning(paste0(
              "Not all subset bands are represented in the original data:",
              "\n- band_names provided to subset may be incorrect, or",
              "\n- new names have been provided by the user to band_names."
            ))
          }
          private$.band_names <- private$.band_names[
            private$.band_names %in% subset$band
          ]
        } else {
          private$.band_names <- private$.band_names[subset$band]
        }
        private$.nbands <- length(private$.band_names)
        private$.subset <- TRUE
      }
    },

    .__transform_time_format__ = function(time_format) {
      private$.time_format <- time_format
    },

    .__transform_space_format__ = function(space_format) {
      private$.space_format <- space_format
    },

    # Create a new LPJmLMetaData object
    #   x `list` (not nested) with meta data
    #   subset `list` of array dimension(s) as name/key and
    #     corresponding subset vector as value, e.g.
    #     `list(cell = c(27411:27415)`, more information at
    #     \link[lpjmlkit]{subset}.
    #   additional_data `list` of additional attributes to be set that
    #     are not included in file header. These are
    #     `c"(band_names", "variable", "descr", "unit")`
    #   data_dir Character string for data directory to "lazy load" grid
    initialize = function(x,
                          subset = list(),
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
      if (length(subset) > 0) {
        self$.__update_subset__(subset)
      }
      # add data_dir for lazy loading of (e.g.) grid later
      if (!is.null(data_dir)) {
        private$.data_dir <- data_dir
      }
    }
  ),
  active = list(
    #' @field sim_name simulation name (workds as identifier in LPJmL Runner)
    sim_name = function() {
      return(private$.sim_name)
    },
    #' @field source LPJmL version (character string).
    source = function() {
      return(private$.source)
    },
    #' @field history Character string of path to LPJmL executable and path to
    #' config file for simulation.
    history = function() {
      return(private$.history)
    },
    #' @field variable Variable of output like `"npp"` or `"runoff"`
    variable = function() {
      return(private$.variable)
    },
    #' @field descr Description of the output/variable.
    descr = function() {
      return(private$.descr)
    },
    #' @field unit Unit of the output/variable.
    unit = function() {
      return(private$.unit)
    },
    #' @field nbands Number (numeric) of bands (categoric dimension). Please
    #' note that nband has somehow become accepted instead of nband as opposed
    #' to nyear or ncell (!)
    nbands = function() {
      return(private$.nbands)
    },
    #' @field band_names Name of bands (categoric dimension), if `nbands > 1`,
    #' else it is not included (!)
    band_names = function() {
      return(private$.band_names)
    },
    #' @field nyear Number (numeric) of simulation years in the output.
    nyear = function() {
      return(private$.nyear)
    },
    #' @field firstyear First year (numeric) of output of the simulation.
    firstyear = function() {
      return(private$.firstyear)
    },
    #' @field lastyear First year (numeric) of output of the simulation.
    lastyear = function() {
      return(private$.lastyear)
    },
    #' @field nstep Intra annual time steps (numeric) `1 == "annual"`,
    #' `12 == "monthly"` and `365 == "daily"`.
    nstep = function() {
      return(private$.nstep)
    },
    #' @field timestep Inter annual time steps (numeric). `timestep = 5` means
    #' that output is written every 5 years.
    timestep = function() {
      return(private$.timestep)
    },
    #' @field ncell Number (numeric) of cells used in the simulation.
    ncell = function() {
      return(private$.ncell)
    },
    #' @field firstcell First cell (numeric) beeing simulated.
    firstcell = function() {
      return(private$.firstcell)
    },
    #' @field cellsize_lon Longitude cellsize in degree (numeric).
    cellsize_lon = function() {
      return(private$.cellsize_lon)
    },
    #' @field cellsize_lat Latitude cellsize in degree (numeric).
    cellsize_lat = function() {
      return(private$.cellsize_lat)
    },
    #' @field datatype File data type (character string), e.g. `"float"`.
    datatype = function() {
      return(private$.datatype)
    },
    #' @field scalar Conversion factor (numeric).
    scalar = function() {
      return(private$.scalar)
    },
    #' @field order Order of data items , either `1 == "cellyear"`,
    #' `2 == "yearcell"` or `3 == "cellindex"`
    order = function() {
      return(private$.order)
    },
    #' @field offset Offset in binary file (numeric).
    offset = function() {
      return(private$.offset)
    },
    #' @field bigendian (logical) Endianness refers to the order in which bytes
    #' are stored in a multi-byte value, with big-endian storing the most
    #' significant byte at the lowest address and little-endian storing the
    #' least significant byte at the lowest address.
    bigendian = function() {
      return(private$.bigendian)
    },
    #' @field format Output format (character string). Either "raw" or "clm"
    #' (raw with header), or "cdf" for netCDF format.
    format = function() {
      return(private$.format)
    },
    #' @field filename Name of the file.
    filename = function() {
      return(private$.filename)
    },
    map = function() {
      return(private$.map)
    },
    version = function() {
      return(private$.version)
    },
    #' @field subset Logical. Whether is subsetted or not.
    subset = function() {
      if (!is.null(self$variable) && self$variable == "grid") {
        return(private$.subset_space)
      } else {
        return(private$.subset)
      }
    },
    subset_space = function() {
      return(private$.subset_space)
    },
    fields_set = function() {
      return(private$.fields_set)
    },
    data_dir = function() {
      return(private$.data_dir)
    },
    time_format = function() {
      return(private$.time_format)
    },
    space_format = function() {
      return(private$.space_format)
    },
    dimension_map = function() {
      return(private$.dimension_map)
    }
  ),
  private = list(
    init_list = function(x, additional_data = list()) {
      for (name_id in private$.name_order) {
        # if (!names(x[idx]) %in% names(LPJmLMetaData$public_fields)) {
        #   warning(paste0(names(x[idx]),
        #                  " may not be a valid LPJmLMetaData field."))
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
      # exclude entries from self print (for LPJmLData class)
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
    .subset_space = FALSE,
    .fields_set = NULL,
    .data_dir = NULL,
    .time_format = "time",
    .space_format = "cell",
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
    .dimension_map = list(space_format = c("cell", "lon_lat"),
                          time_format = c("time", "year_month_day"),
                          time = "time",
                          year_month_day = c("year",
                                             "year_month", "month_year",
                                             "year_month_day", "day_month_year"), # nolint
                          cell = "cell",
                          lon_lat = c("lon_lat", "lat_lon"))
  )
)
