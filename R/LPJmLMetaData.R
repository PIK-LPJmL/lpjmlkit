#' @title LPJmL meta data class
#'
#' @description A meta data container for LPJmL input and output meta data.
#' Container - because an [`LPJmLMetaData`] object is an environment in which
#' the meta data is stored after [`read_meta`] (or [`read_io`]).
#' Each attribute can be accessed via `$<attribute>`. To get an overview over
#' available attributes, [`print`] the object or export it as a list [`as_list`].
#' The enclosing environment is locked and cannot be altered.
#'
LPJmLMetaData <- R6::R6Class( # nolint

  classname = "LPJmLMetaData",

  lock_objects = TRUE,

  public = list(


    # export methods --------------------------------------------------------- #

    #' @description
    #' Method to coerce (convert) an `LPJmLMetaData` object into a
    #' \link[base]{list}. \cr
    #' See also [`as_list`]
    as_list = function() {
      private$.as_list()
    },


    #' @description
    #' Method to coerce (convert) an `LPJmLMetaData` object into an LPJmL (input)
    #' header (more info at [`create_header`]). \cr
    #'
    #' @param ... See [`as_header`]
    as_header = function(...) {
      private$.as_header(...)
    },


    #' @description
    #' Method to print an `LPJmLMetaData` object.
    #' See also \link[base]{print}
    #'
    #' @param all Logical. Should all attributes be printed or only the most
    #' relevant (`all = FALSE`)
    #'
    #' @param spaces *internal* Spaces to be printed in the beginning
    print = function(all = TRUE, spaces = "") {

      if (!all) {
        print_fields <- self$._fields_set_ %>%
          `[`(-stats::na.omit(match(private$exclude_print(), .)))
      } else {
        print_fields <- self$._fields_set_
      }

      # Colorize self print.
      blue_col <- "\u001b[34m"
      unset_col <- "\u001b[0m"

      meta_fields <- print_fields %>%
        sapply(function(x) do.call("$", list(self, x)), # nolint:undesirable_function_linter.
               USE.NAMES = FALSE)

      to_char1 <- print_fields %>%
        sapply(function(x) { # nolint:undesirable_function_linter
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

                   # Print vectors as output not as c(...).
                   if (length(x) > 6 && is.character(x)) {
                     # Shorten character vectors.
                     x <- c(x[1:4], "...", tail(x, n = 1))
                   }

                   if (is.character(x)) {
                     # Quotes only around each element not around vector.
                     return(noquote(paste(dQuote(x), collapse = " ")))
                   } else {
                     # No quotes for numeric vectors.
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
          # Color red if subset.
          blue_col,
          "$subset",
          unset_col,
          " ",
          # Color red if subset.
          ifelse(self$subset, "\u001b[31m", ""),
          self$subset,
          ifelse(self$subset, unset_col, ""),
          "\n"
        )
      )
    },


    # Method to initialize meta data for LPJmLMetaData object with variable
    #   == "grid"
    #' @description
    #' !Internal method only to be used for package development!
    .__init_grid__ = function() {

      if (private$.variable != "grid") {
        stop(paste("Only valid for variable", sQuote("grid"), "."))
      }

      # Set all time fields to NULL.
      private$.nyear <- NULL
      private$.firstyear <- NULL
      private$.lastyear <- NULL
      private$.nstep <- NULL
      private$.timestep <- NULL

      # Update fields_set.
      private$.fields_set <- private$.fields_set[
        -na.omit(match(c("nyear",
                         "firstyear",
                         "lastyear",
                         "nstep",
                         "timestep"),
                       private$.fields_set))
      ]
    },

    # Update supplied subset in self.subset
    #   (!only in conjunction with LPJmLData!)
    #' @description
    #' !Internal method only to be used for package development!
    #' @param subset List of subset arguments, see also [`subset`].
    #'
    #' @param cell_dimnames Optional - list of new cell_dimnames of subset data
    #' to update meta data, required if spatial dimensions are subsetted !
    #'
    #' @param time_dimnames Optional - list of new time_dimnames of subset data
    #' to update meta data, required if time dimension is subsetted !
    #'
    #' @param year_dimnames Optional - list of new year_dimnames of subset data
    #' to update meta data, required if year dimension is subsetted !
    .__update_subset__ = function(subset,
                                  cell_dimnames = NULL,
                                  time_dimnames = NULL,
                                  year_dimnames = NULL) {

      # Update cell fields - distinguish between character -> LPJmL C index
      #   starting from 0! and numeric/integer -> R index starting from 1 -> -1.
      if (!is.null(subset$cell) ||
          !is.null(subset$lon) || !is.null(subset$lat)) {

        # Subset of subset$cell, subset$lon or subset$lat always have to be
        #   accompanied by cell_dimnames.
        if (!is.null(cell_dimnames)) {
          private$.firstcell <- min(as.numeric(cell_dimnames))
          private$.ncell <- length(cell_dimnames)
        }

        private$.subset <- TRUE
        private$.subset_space <- TRUE
      }

      if (!is.null(subset$time) && !is.null(time_dimnames)) {
        year_dimnames <- split_time_names(time_dimnames)$year
      } else if (!is.null(subset$year) && is.character(subset$year)) {
        year_dimnames <- subset$year
      }

      if (!is.null(year_dimnames)) {
        private$.firstyear <- min(as.integer(year_dimnames))
        private$.lastyear <- max(as.integer(year_dimnames))
        private$.nyear <- length(year_dimnames)
        private$.subset <- TRUE
      }

      # band can be subsetted via indices or band_names - the latter is updated
      if (!is.null(subset$band)) {

        if (is.character(subset$band) && !is.null(private$.band_names)) {
          private$.band_names <- private$.band_names[
            private$.band_names %in% subset$band
          ]

        } else {
          private$.band_names <- private$.band_names[subset$band]
        }

        if (!is.null(private$.band_names)) {
          private$.nbands <- length(private$.band_names)
        } else {
          private$.nbands <- length(seq_len(private$.nbands)[subset$band])
        }

        private$.subset <- TRUE
      }
    },


    # Set new time format
    #' @description
    #' !Internal method only to be used for package development!
    #'
    #' @param time_format Character. Choose between `"year_month_day"` and
    #' `"time"`
    .__transform_time_format__ = function(time_format) {
      private$.time_format <- time_format
    },


    # Set new space format
    #' @description
    #' !Internal method only to be used for package development!
    #'
    #' @param space_format Character. Choose between `"lon_lat"` and `"cell"`
    .__transform_space_format__ = function(space_format) {
      private$.space_format <- space_format
    },


    # Set attribute
    #' @description
    #' !Internal method only to be used for package development!
    #' @param key Character. Name of the attribute, e.g. `"variable"`
    #'
    #' @param value Value of the attribute, e.g. `"grid"`
    .__set_attribute__ = function(key, value) {
      private[[paste0(".", key)]] <- value
    },


    #' @description
    #' Create a new LPJmLMetaData object
    #'
    #' @param x A `list` (not nested) with meta data
    #'
    #' @param additional_attributes A `list` of additional attributes to be set
    #' that are not included in file header. These are
    #' `c"(band_names", "variable", "descr", "unit")`
    #'
    #' @param data_dir A Character string for data directory to "lazy load" grid
    initialize = function(x,
                          additional_attributes = list(),
                          data_dir = NULL) {

      if (all(names(x) %in% c("name", "header", "endian"))) {
        header_to_meta <- as.list(x$header) %>%
          append(list(
            "bigendian" = ifelse(x$endian == "big", TRUE, FALSE),
            # "descr" = tolower(x$name), # nolint
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
        private$init_list(header_to_meta, additional_attributes)

      } else {
        private$init_list(x, additional_attributes)
      }

      # Add data_dir for lazy loading of (e.g.) grid later.
      if (!is.null(data_dir)) {
        private$.data_dir <- data_dir
      }
    }
  ),


  # Active bindings
  active = list(

    #' @field sim_name Simulation name (workds as identifier in LPJmL Runner)
    sim_name = function() {
      return(private$.sim_name)
    },

    #' @field source LPJmL version (character string)
    source = function() {
      return(private$.source)
    },

    #' @field history Character string of path to LPJmL executable and path to
    #' config file for simulation
    history = function() {
      return(private$.history)
    },

    #' @field variable Variable of output like `"npp"` or `"runoff"`
    variable = function() {
      return(private$.variable)
    },

    #' @field descr Description of the output/variable
    descr = function() {
      return(private$.descr)
    },

    #' @field unit Unit of the output/variable
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

    #' @field nyear Number (numeric) of simulation years in the output
    nyear = function() {
      return(private$.nyear)
    },

    #' @field firstyear First year (numeric) of output of the simulation
    firstyear = function() {
      return(private$.firstyear)
    },

    #' @field lastyear First year (numeric) of output of the simulation
    lastyear = function() {
      return(private$.lastyear)
    },

    #' @field nstep Intra annual time steps (numeric) `1 == "annual"`,
    #' `12 == "monthly"` and `365 == "daily"`
    nstep = function() {
      return(private$.nstep)
    },

    #' @field timestep Inter annual time steps (numeric). `timestep = 5` means
    #' that output is written every 5 years.
    timestep = function() {
      return(private$.timestep)
    },

    #' @field ncell Number (numeric) of cells used in the simulation
    ncell = function() {
      return(private$.ncell)
    },

    #' @field firstcell First cell (numeric) beeing simulated
    firstcell = function() {
      return(private$.firstcell)
    },

    #' @field cellsize_lon Longitude cellsize in degree (numeric)
    cellsize_lon = function() {
      return(private$.cellsize_lon)
    },

    #' @field cellsize_lat Latitude cellsize in degree (numeric)
    cellsize_lat = function() {
      return(private$.cellsize_lat)
    },

    #' @field datatype File data type (character string), e.g. `"float"`.
    datatype = function() {
      return(private$.datatype)
    },

    #' @field scalar Conversion factor (numeric)
    scalar = function() {
      return(private$.scalar)
    },

    #' @field order Order of data items , either `1 == "cellyear"`,
    #' `2 == "yearcell"` or `3 == "cellindex"`
    order = function() {
      return(private$.order)
    },

    #' @field offset Offset in binary file (numeric)
    offset = function() {
      return(private$.offset)
    },

    #' @field bigendian (Logical) endianness refers to the order in which bytes
    #' are stored in a multi-byte value, with big-endian storing the most
    #' significant byte at the lowest address and little-endian storing the
    #' least significant byte at the lowest address.
    bigendian = function() {
      return(private$.bigendian)
    },

    #' @field format Output format (character string). Either "raw" or "clm"
    #' (raw with header), or "cdf" for netCDF format
    format = function() {
      return(private$.format)
    },

    #' @field filename Name of the file
    filename = function() {
      return(private$.filename)
    },

    #' @field subset Logical. Whether is subsetted or not.
    subset = function() {
      if (!is.null(self$variable) && self$variable == "grid") {
        return(private$.subset_space)
      } else {
        return(private$.subset)
      }
    },

    #' @field map For inputs
    map = function() {
      return(private$.map)
    },

    #' @field version Version of file
    version = function() {
      return(private$.version)
    },

    #' @field ._data_dir_ *internal* Character string LPJmL simulation output
    #' directory.
    ._data_dir_ = function() {
      return(private$.data_dir)
    },

    #' @field ._subset_space_ *internal* Logical. Whether space dimensions are
    #' subsetted.
    ._subset_space_ = function() {
      return(private$.subset_space)
    },

    #' @field ._fields_set_ *internal* Character vector of names of attributes
    #' set by meta file
    ._fields_set_ = function() {
      return(private$.fields_set)
    },

    #' @field ._time_format_ *internal* Character sting. Time dimension format,
    #' either `"time"` or `"year_month_day"`
    ._time_format_ = function() {
      return(private$.time_format)
    },

    #' @field ._space_format_ *internal* Character string. Space dimension
    #' format, either `"cell"` or `"lon_lat"`
    ._space_format_ = function() {
      return(private$.space_format)
    },

    #' @field ._dimension_map_ *internal* Dictionary/List of space and time
    #' dimension formats with categories and namings
    ._dimension_map_ = function() {
      return(private$.dimension_map)
    }
  ),


  private = list(
    init_list = function(x, additional_attributes = list()) {

      for (name_id in private$.name_order) {

        if (is.null(x[[name_id]])) {
          if (name_id %in% names(additional_attributes)) {
            x[[name_id]] <- additional_attributes[[name_id]]
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

      # Exclude entries from self print (for LPJmLData class).
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

      # Only append scalar if != 1
      append(
        ifelse(
          !is.null(private$.scalar),
          ifelse(private$.scalar == 1, "scalar", NA),
          NA
        )
      ) %>%

      # Workaround to deal with NAs (NULL not possible in ifelse)
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
