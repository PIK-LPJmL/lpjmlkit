#' @title LPJmL meta data class
#'
#' @description
#' A meta data container for LPJmL input and output meta data.
#' Container - because an [`LPJmLMetaData`] object is an environment in which
#' the meta data are stored after [`read_meta()`] (or [`read_io()`]).
#' Each attribute can be accessed via `$<attribute>`. To get an overview over
#' available attributes, [`print`] the object or export it as a list
#' [`as_list()`].
#' The enclosing environment is locked and cannot be altered.
#'
#' @md
#' @export
LPJmLMetaData <- R6::R6Class( # nolint

  classname = "LPJmLMetaData",

  lock_objects = TRUE,

  public = list(


    # Export methods --------------------------------------------------------- #

    #' @description
    #' Method to coerce (convert) an `LPJmLMetaData` object into a
    #' \link[base]{list}. \cr
    #' See also [`as_list()`].
    as_list = function() {
      private$.as_list()
    },


    #' @description
    #' Method to coerce (convert) an `LPJmLMetaData` object into an LPJmL
    #' binary file header. More information about file headers at
    #' [`create_header()`]). \cr
    #'
    #' @param ... See [`as_header()`].
    as_header = function(...) {
      private$.as_header(...)
    },


    #' @description
    #' Method to print an `LPJmLMetaData` object.
    #' See also \link[base]{print}.
    #'
    #' @param all Logical. Should all attributes be printed or only the most
    #'   relevant (`all = FALSE`)?
    #'
    #' @param spaces *Internal parameter* Spaces to be printed at the start.
    print = function(all = TRUE, spaces = "") {

      if (!all) {
        print_fields <- self$._fields_set_ %>%
          `[`(-stats::na.omit(match(private$exclude_print(), .)))
      } else {
        print_fields <- self$._fields_set_
      }

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
               col_var(
                 paste0("$", print_fields)
               ),
               " ",
               to_char1,
               lapply(meta_fields, function(x) {

                 if (length(x) > 1) {

                   # Print vectors as output not as c(...)
                   if (length(x) > 6 && is.character(x)) {
                     # Shorten character vectors.
                     x <- c(x[1:4], "...", tail(x, n = 1))
                   }

                   if (is.character(x)) {
                     # Quotes only around each element, not around vector
                     return(noquote(paste(dQuote(x), collapse = " ")))
                   } else if (is.list(x) && !is.null(x[["filename"]])) {
                     return(noquote(paste(dQuote(x[["filename"]]), collapse = " ")))
                   } else {
                     # No quotes for numeric vectors
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

      # Print information about subset if subsetted
      cat(
        paste0(
          spaces,
          col_var("$subset"),
          " ",
          # Color red if subset.
          ifelse(self$subset, col_warn(self$subset), self$subset),
          "\n"
        )
      )
    },


    # Method to initialize meta data for LPJmLMetaData object with variable
    #   == "grid"
    #' @description
    #' !Internal method only to be used for package development!
    .__init_grid__ = function() {

      if (!private$.variable %in% c("grid", "cellid")) {
        stop(
          "Only valid for variable ", sQuote("grid"),
          " or ", sQuote("cellid"), "."
        )
      }

      # Set all time fields to NULL
      private$.nyear <- NULL
      private$.firstyear <- NULL
      private$.lastyear <- NULL
      private$.nstep <- NULL
      private$.timestep <- NULL

      # Update fields_set
      private$.fields_set <- private$.fields_set[
        -stats::na.omit(
          match(
            c(
              "nyear",
              "firstyear",
              "lastyear",
              "nstep",
              "timestep"
            ),
            private$.fields_set
          )
        )
      ]
    },

    # Update supplied subset in self.subset
    #   (!only in conjunction with LPJmLData!)
    #' @description
    #' !Internal method only to be used for package development!
    #'
    #' @param subset List of subset arguments, see also [`subset.LPJmLData()`].
    #'
    #' @param cell_dimnames Optional list of new cell_dimnames of subset data
    #'   to update meta data. Required if spatial dimensions are subsetted.
    #'
    #' @param time_dimnames Optional list of new time_dimnames of subset data
    #'  to update meta data. Required if time dimension is subsetted.
    #'
    #' @param year_dimnames Optional list of new year_dimnames of subset data
    #'   to update meta data. Required if year dimension is subsetted.
    .__update_subset__ = function(subset,
                                  cell_dimnames = NULL,
                                  time_dimnames = NULL,
                                  year_dimnames = NULL) {

      # Update cell fields - distinguish between character -> LPJmL C index
      #   starting from 0 and numeric/integer -> R index starting from 1 -> -1.
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

      # "band" can be subsetted via indices or band_names. Update band_names
      # (if set) and nbands.
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
    #'   `"time"`.
    .__transform_time_format__ = function(time_format) {
      private$.time_format <- time_format
    },


    # Set new space format
    #' @description
    #' !Internal method only to be used for package development!
    #'
    #' @param space_format Character. Choose between `"lon_lat"` and `"cell"`.
    .__transform_space_format__ = function(space_format) {
      private$.space_format <- space_format
    },


    # Set meta data attribute
    #' @description
    #' !Internal method only to be used for package development!
    #'
    #' @param key Name of the attribute, e.g. `"variable"`
    #'
    #' @param value Value of the attribute, e.g. `"grid"`
    .__set_attribute__ = function(key, value) {
      private[[paste0(".", key)]] <- value
    },


    #' @description
    #' Create a new LPJmLMetaData object.
    #'
    #' @param x A list (not nested) with meta data.
    #'
    #' @param additional_attributes A list of additional attributes to be set
    #'   that are not included in file header or JSON meta file. These are
    #' `c"(band_names", "variable", "descr", "unit")`
    #'
    #' @param data_dir Directory containing the file this LPJmLMetaData object
    #'   refers to. Used to "lazy load" grid.
    #'
    initialize = function(x,
                          additional_attributes = list(),
                          data_dir = NULL) {

      if (all(names(x) %in% c("name", "header", "endian"))) {
        is_valid_header(x)
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
          ) %>%
          `[[<-`("datatype",
            switch(as.character(.$datatype),
              `0` = "byte",
              `1` = "short",
              `2` = "int",
              `3` = "float",
              `4` = "double",
              stop(
                paste(
                  "Invalid datatype value", sQuote(.$datatype),
                  "in header"
                )
              )
            )
          )
        private$init_list(header_to_meta, additional_attributes)

      } else {
        private$init_list(x, additional_attributes)
      }

      # Add data_dir for lazy loading of (e.g.) grid later
      if (!is.null(data_dir)) {
        private$.data_dir <- data_dir
      }

      # NetCDF files come directly in the format "lon_lat"
      if (private$.format == "cdf") {
        private$.space_format <- "lon_lat"
      }
    }
  ),


  # Active bindings
  active = list(

    #' @field sim_name Simulation name (works as identifier in LPJmL Runner).
    sim_name = function(...) {
      check_change(self, "sim_name", ...)
      return(private$.sim_name)
    },

    #' @field source LPJmL version (character string).
    source = function(...) {
      check_change(self, "source", ...)
      return(private$.source)
    },

    #' @field history Character string of the call used to run LPJmL. This
    #'   normally includes the path to the LPJmL executable and the path to the
    #'   configuration file for the simulation.
    history = function(...) {
      check_change(self, "history", ...)
      return(private$.history)
    },

    #' @field variable Name of the input/output variable, e.g. `"npp"` or
    #'   `"runoff"`.
    variable = function(...) {
      check_change(self, "variable", ...)
      return(private$.variable)
    },

    #' @field descr Description of the input/output variable.
    descr = function(...) {
      check_change(self, "descr", ...)
      return(private$.descr)
    },

    #' @field unit Unit of the input/output variable.
    unit = function(...) {
      check_change(self, "unit", ...)
      return(private$.unit)
    },

    #' @field nbands Number (numeric) of bands (categoric dimension). Please
    #'   note that `nbands` follows the convention in LPJmL, which uses the
    #'   plural form for bands as opposed to `nyear` or `ncell`.
    nbands = function(...) {
      check_change(self, "nbands", ...)
      return(private$.nbands)
    },

    #' @field band_names Name of the bands (categoric dimension). Not included
    #'   if `nbands = 1`.
    band_names = function(...) {
      check_change(self, "band_names", ...)
      return(private$.band_names)
    },

    #' @field nyear Number (numeric) of data years in the parent `LPJmLData`
    #'   object.
    nyear = function(...) {
      check_change(self, "nyear", ...)
      return(private$.nyear)
    },

    #' @field firstyear First calendar year (numeric) in the parent `LPJmLData`
    #'   object.
    firstyear = function(...) {
      check_change(self, "firstyear", ...)
      return(private$.firstyear)
    },

    #' @field lastyear Last calendar year (numeric) in the parent `LPJmLData`
    #'   object.
    lastyear = function(...) {
      check_change(self, "lastyear", ...)
      return(private$.lastyear)
    },

    #' @field nstep Number (numeric) of intra-annual time steps. `1` for annual,
    #' `12` for monthly, and `365` for daily data.
    nstep = function(...) {
      check_change(self, "nstep", ...)
      return(private$.nstep)
    },

    #' @field timestep Number (numeric) of years between time steps.
    #'   `timestep = 5` means that output is written every 5 years.
    timestep = function(...) {
      check_change(self, "timestep", ...)
      return(private$.timestep)
    },

    #' @field ncell Number (numeric) of cells in the parent `LPJmLData` object.
    ncell = function(...) {
      check_change(self, "ncell", ...)
      return(private$.ncell)
    },

    #' @field firstcell First cell (numeric) in the parent `LPJmLData` object.
    firstcell = function(...) {
      check_change(self, "firstcell", ...)
      return(private$.firstcell)
    },

    #' @field cellsize_lon Longitude cellsize in degrees (numeric).
    cellsize_lon = function(...) {
      check_change(self, "cellsize_lon", ...)
      return(private$.cellsize_lon)
    },

    #' @field cellsize_lat Latitude cellsize in degrees (numeric).
    cellsize_lat = function(...) {
      check_change(self, "cellsize_lat", ...)
      return(private$.cellsize_lat)
    },

    #' @field datatype File data type (character string), e.g. `"float"`. Note
    #'   that data are converted into R-internal data type by [`read_io()`].
    datatype = function(...) {
      check_change(self, "datatype", ...)
      return(private$.datatype)
    },

    #' @field scalar Conversion factor (numeric) applied when reading raw data
    #'   from file. The parent `LPJmLData` object contains the values after
    #'   the application of the conversion factor.
    scalar = function(...) {
      check_change(self, "scalar", ...)
      return(private$.scalar)
    },

    #' @field order Order of the data items in the file, either `"cellyear"`,
    #'   `"yearcell"`, `"cellindex"`, or `"cellseq"`. The structure of the data
    #'   array in the parent `LPJmLData` object may differ from the original
    #'   order in the file depending on the `dim_order` parameter used in
    #'   [`read_io()`].
    order = function(...) {
      check_change(self, "order", ...)
      return(private$.order)
    },

    #' @field offset Offset (numeric) at the start of the binary file before the
    #'   actual data start.
    offset = function(...) {
      check_change(self, "offset", ...)
      return(private$.offset)
    },

    #' @field bigendian (Logical) Endianness refers to the order in which bytes
    #' are stored in a multi-byte value, with big-endian storing the most
    #' significant byte at the lowest address and little-endian storing the
    #' least significant byte at the lowest address.
    bigendian = function(...) {
      check_change(self, "bigendian", ...)
      return(private$.bigendian)
    },

    #' @field format Binary format (character string) of the file containing the
    #'   actual data. Either `"raw"`, `"clm"` (raw with header), or `"cdf"` for
    #'   NetCDF format.
    format = function(...) {
      check_change(self, "format", ...)
      return(private$.format)
    },

    #' @field filename Name of the file containing the actual data.
    filename = function(...) {
      check_change(self, "filename", ...)
      return(private$.filename)
    },

    #' @field subset Logical. Whether parent `LPJmLData` object is subsetted.
    subset = function(...) {
      check_change(self, "subset", ...)
      if (!is.null(self$variable) && self$variable == "grid") {
        return(private$.subset_space)
      } else {
        return(private$.subset)
      }
    },

    #' @field map Character vector describing how to map the bands in an input
    #'   file to the bands used inside LPJmL. May be used by [`read_io()`] to
    #'   construct a `band_names` attribute.
    map = function(...) {
      check_change(self, "map", ...)
      return(private$.map)
    },

    #' @field version Version of data file.
    version = function(...) {
      check_change(self, "version", ...)
      return(private$.version)
    },

    #' @field grid List including information on the filename and type of the
    #' corresponding grid file.
    grid = function(...) {
      check_change(self, "grid", ...)
      return(private$.grid)
    },

    #' @field ref_area List including information on the reference_area filename and type
    #' of the data.
    ref_area = function(...) {
      check_change(self, "ref_area", ...)
      return(private$.ref_area)
    },

    #' @field ._data_dir_ *Internal* character string containing the directory
    #'   from which the file was loaded.
    ._data_dir_ = function(...) {
      check_change(self, "._data_dir_", ...)
      return(private$.data_dir)
    },

    #' @field ._subset_space_ *Internal* logical. Whether space dimensions are
    #' subsetted in the parent `LPJmLData` object.
    ._subset_space_ = function(...) {
      check_change(self, "._subset_space_", ...)
      return(private$.subset_space)
    },

    #' @field ._fields_set_ *Internal* character vector of names of attributes
    #' set by the meta file.
    ._fields_set_ = function(...) {
      check_change(self, "._fields_set_", ...)
      return(private$.fields_set)
    },

    #' @field ._time_format_ *Internal* character string describing the time
    #'   dimension format, either `"time"` or `"year_month_day"`.
    ._time_format_ = function(...) {
      check_change(self, "._time_format_", ...)
      return(private$.time_format)
    },

    #' @field ._space_format_ *Internal* character string describing the space
    #'   dimension format, either `"cell"` or `"lon_lat"`.
    ._space_format_ = function(...) {
      check_change(self, "._space_format_", ...)
      return(private$.space_format)
    },

    #' @field ._dimension_map_ *Internal* dictionary/list of space and time
    #' dimension formats with categories and namings.
    ._dimension_map_ = function(...) {
      check_change(self, "._dimension_map_", ...)
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

          # Do not add "name" attribute to .fields_set because it is only saved
          # internally for conversion back to a header
          if (name_id != "name")
            private$.fields_set <- append(private$.fields_set, name_id)
        }
      }
    },

    exclude_print = function() {

      # Exclude entries from self print (for LPJmLData class)
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
        "filename",
        "grid",
        "ref_area"
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

    .grid = NULL,

    .ref_area = NULL,

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
                    "grid",
                    "ref_area",
                    "name",
                    "map",
                    "version",
                    "offset"),

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
