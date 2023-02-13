#' @title Read LPJmL input and output files
#'
#' @description Generic function to read LPJmL input & output files in different
#'   formats. Depending on the format, arguments can be automatically detected
#'   or have to be passed as individual arguments.
#'
#' @param filename Mandatory character string giving the file name to read,
#'   including its path and extension.
#' @param subset Optional list allowing to subset data read from the file along
#'   one or several of its dimensions. See details for more information.
#' @param band_names Optional vector of character strings providing the band
#'   names or `NULL`. Normally determined automatically from the meta file in
#'   case of output files using `file_type = "meta"`.
#' @param dim_order Order of dimensions in returned LPJmLData object. Must be
#'   a character vector containing all of the following in any order:
#'   `c("cell", "time", "band")`. Users may select the order most useful to
#'   their further data processing.
#' @param file_type Optional character string giving the file type. This is
#'   normally detected automatically but can be prescribed if automatic
#'   detection is incorrect. Valid options:
#'   * `"raw"`, a binary file without header.
#'   * `"clm"`, a binary file with header.
#'   * `"meta"`, a meta information JSON file complementing a raw or clm file.
#' @param version Integer indicating the clm file header version, currently
#'   supports one of `c(1, 2, 3, 4)`.
#' @param order Integer value or character string describing the order of data
#'   items in the file (default in input file: 1; in output file: 4). Valid
#'   values for LPJmL input/output files are `"cellyear"`/ `1`, `"yearcell"` /
#'   `2`, `"cellindex"`/ `3`, and `"cellseq"` / `4`, although only options `1`
#'   and `4` are supported by this function.
#' @param firstyear Integer providing the first year of data in the file.
#' @param nyear Integer providing the number of years of data included in the
#'   file. These are not consecutive in case of `timestep > 1`.
#' @param firstcell Integer providing the cell index of the first data item.
#'   `0` by default.
#' @param ncell Integer providing the number of data items per band.
#' @param nbands Integer providing the number of bands per time step of data.
#' @param cellsize_lon Numeric value providing the longitude cell size in
#'   degrees.
#' @param scalar Numeric value providing a conversion factor that needs to be
#'   applied to raw data when reading it from file to derive final values.
#' @param cellsize_lat Numeric value providing the latitude cell size in
#'   degrees.
#' @param datatype Integer value or character string describing the LPJmL data
#'   type stored in the file. Supported options: `"byte"` / `0`, `"short"` /
#'   `1`, `"int"` / `2`, `"float"` / `3`, or `"double"` / `4`.
#' @param nstep Integer value defining the number of within-year time steps of
#'   the file. Valid values are `1` (yearly), `12` (monthly), `365` (daily).
#'   Defaults to `1` if not read from file ("clm" or "meta" file) or provided by
#'   the user.
#' @param timestep Integer value providing the interval in years between years
#'   represented in the file data. Normally `1`, but LPJmL also allows averaging
#'   annual outputs over several years. Defaults to `1` if not read from file
#'   ("clm" or "meta" file) or provided by user.
#' @param endian Endianness to use for file (either `"big"` or `"little"`). By
#'   default uses endianness determined from file header or set in meta
#'   information or the platform-specific endianness `.Platform$endian` if not
#'   set.
#' @param variable Optional character string providing the name of the variable
#'   contained in the file. Included in some JSON meta files. **Important:** If
#'   `file_type == "raw"`, prescribe `variable = "grid"` to ensure that data
#'   are recognized as a grid.
#' @param descr Optional character string providing a more detailed description
#'   of the variable contained in the file. Included in some JSON meta files.
#' @param unit Optional character string providing the unit of the data in the
#'   file. Included in some JSON meta files.
#' @param name Optional character string specifying the header name. This is
#'   usually read from clm headers for `file_type = "clm"` but can be specified
#'   for the other `file_type` options.
#' @param silent If set to `TRUE`, suppresses most warnings or messages. Use
#'   only after testing that `read_io()` works as expected with the files it is
#'   being used on. Default: `FALSE`.
#' @return An [LPJmLData] object.
#' @examples
#' \dontrun{
#' # First case: meta file. Reads meta information from "my_file.json" and
#' # data from binary file linked in "my_file.json". Normally does not require
#' # any additional arguments.
#' my_data <- read_io("my_file.json")
#'
#' # Suppose that file data has two bands named "wheat" and "rice". `band_names`
#' # are included in the JSON meta file. Select only the "wheat" band during
#' # reading and discard the "rice" band. Also, read only data for years
#' # 1910-1920.
#' my_data_wheat <- read_io(
#'   "my_file.json",
#'   subset = list(band = "wheat", year = as.character(seq(1910, 1920)))
#' )
#'
#' # Read data from clm file. This includes a header describing the file
#' # structure.
#' my_data_clm <- read_io("my_file.clm")
#'
#' # Suppose that "my_file.clm" has two bands containing data for "wheat" and
#' # "rice". Assign names to them manually since the header does not include a
#' # `band_names` attribute.
#' my_data_clm <- read_io("my_file.clm", band_names = c("wheat", "rice"))
#'
#' # Once `band_names` are set, subsetting by name is possible also for
#' # file_type = "clm"
#' my_data_wheat <- read_io(
#'   "my_file.clm",
#'   band_names = c("wheat", "rice"),
#'   subset = list(band = "wheat", year = as.character(seq(1910, 1920)))
#' )
#'
#' # Read data from raw binary file. All information about file structure needs
#' # to be supplied. Use default values except for nyear (1 by default), and
#' # nbands (also 1 by default).
#' my_data <- read_io("my_file.bin", nyear = 100, nbands = 2)
#'
#' # Supply band_names to be able to subset by name
#' my_data_wheat <- read_io(
#'   "my_file.bin",
#'   band_names = c("wheat", "rice"), # length needs to correspond to `nbands`
#'   subset = list(band = "wheat", year = as.character(seq(1910, 1920)))
#'   nyear = 100,
#'   nbands = 2,
#' )
#' }
#' @details
#' The `file_type` determines which arguments are mandatory or optional.
#' `filename` must always be provided. `file_type` is usually detected
#' automatically. Supply only if detected `file_type` is incorrect.
#'
#' In case of `file_type = "meta"`, if any of the function arguments not listed
#' as "mandatory" are provided and are already set in the JSON file, a warning
#' is given, but they are still overwritten. Normally, you would only set meta
#' attributes not set in the JSON file.
#'
#' In case of `file_type = "clm"`, function arguments not listed as "optional"
#' are usually determined automatically from the file header included in the
#' clm file. Users may still provide any of these arguments to overwrite values
#' read from the file header, e.g. when they know that the values in the file
#' header are wrong. Also, clm headers with versions < 4 do not contain all
#' header attributes, with missing attributes filled with default values that
#' may not be correct for all files.
#'
#' In case of `file_type = "raw"`, files do not contain any information about
#' their structure. Users should provide all arguments not listed as "optional".
#' Otherwise, default values valid for LPJmL standard outputs are used for
#' arguments not supplied by the user. For example, the default `firstyear` is
#' 1901, the default for `nyear`, `nbands`, `nstep`, and `timestep` is 1.
#'
#' `subset` can be a list containing one or several named elements. Allowed
#' names are "band", "cell", and "year".
#' * "year" can be used to return data for a subset of one or several years
#' included in the file. Integer indices can be between 1 and `nyear`. If
#' subsetting by actual calendar years (starting at `firstyear`) a
#' character vector has to be supplied.
#' * "band" can be used to return data for a subset of one or several bands
#' included in the file. These can be specified either as integer indices or as
#' a character vector if bands are named.
#' * "cell" can be used to return data for a subset of cells. Note that integer
#' indices start counting at 1, whereas character indices start counting at the
#' value of `firstcell` (usually `0`).
#' @aliases read_input read_output
#' @export
read_io <- function( # nolint:cyclocomp_linter.
  filename,
  subset  = list(),
  band_names   = NULL,
  dim_order   = c("cell", "time", "band"),
  file_type    = NULL,
  version      = NULL,
  order        = NULL,
  firstyear    = NULL,
  nyear        = NULL,
  firstcell    = NULL,
  ncell        = NULL,
  nbands       = NULL,
  cellsize_lon = NULL,
  scalar       = NULL,
  cellsize_lat = NULL,
  datatype     = NULL,
  nstep        = NULL,
  timestep     = NULL,
  endian       = NULL,
  variable     = NULL,
  descr        = NULL,
  unit         = NULL,
  name         = NULL,
  silent       = FALSE
) {
  # Switch off fancy quotes but revert setting when leaving the function
  quotes_option <- options(useFancyQuotes = FALSE) # nolint:undesirable_function_linter.
  on.exit(options(quotes_option)) # nolint:undesirable_function_linter.
  # Detect file_type if not provided by user
  if (is.null(file_type)) {
    file_type <- detect_type(filename)
  }
  # Check valid file_type
  if (!file_type %in% supported_types) {
    stop(
      "file_type ", dQuote(file_type), " is not supported.\n",
      "This function can read files of type ", toString(dQuote(supported_types))
    )
  }
  # Check valid dim_order
  valid_dim_names <- c("cell", "time", "band")
  if (!all(dim_order %in% valid_dim_names)) {
    stop(
      "Invalid dim_order prodided: c(",
      toString(sQuote(dim_order)), ")\n",
      "dim_order can be in any order but must include all of the following ",
      "band names: c(", toString(sQuote(sort(valid_dim_names))), ")"
    )
  }
  if (!all(valid_dim_names %in% dim_order)) {
    stop(
      "Invalid dim_order prodided: c(",
      toString(sQuote(dim_order)), ")\n",
      "dim_order can be in any order but must include all of the following ",
      "band names: c(", toString(sQuote(sort(valid_dim_names))), ")"
    )
  }

  # Construct meta data from JSON, clm header, and/or provided function
  # arguments
  meta_data <- match.arg(file_type, supported_types) %>%
    paste("read_io_metadata", ., sep = "_") %>%
    do.call(args = list(filename = filename,
                        file_type = file_type,
                        band_names = band_names,
                        version = version,
                        order = order,
                        firstyear = firstyear,
                        nyear = nyear,
                        firstcell = firstcell,
                        ncell = ncell,
                        nbands = nbands,
                        cellsize_lon = cellsize_lon,
                        scalar = scalar,
                        cellsize_lat = cellsize_lat,
                        datatype = datatype,
                        nstep = nstep,
                        timestep = timestep,
                        endian = endian,
                        variable = variable,
                        descr = descr,
                        unit = unit,
                        name = name,
                        silent = silent))
  # Offset at beginning of binary data file
  start_offset <- default(meta_data$offset, 0)

  # Filter for NAs in subset
  if (length(subset) > 0 && any(sapply(subset, anyNA))) { # nolint:undesirable_function_linter.
    before <- names(subset)
    if (!silent) {
      warning(
        "Removing NA values from ",
        paste(
          "subset[[", dQuote(names(which(sapply(subset, anyNA)))), "]]", # nolint:undesirable_function_linter.
          sep = "", collapse = ", "
        )
      )
    }
    subset <- lapply(subset, stats::na.omit)
    if ("year" %in% names(subset) && length(subset$year) == 0) {
      stop("subset[[\"year\"]] is empty after removal of NAs")
    }
    subset <- subset[which(sapply(subset, length) > 0)] # nolint:undesirable_function_linter.
    if (length(subset) < length(before) && !silent) {
      warning(
        paste(
          "subset[[", dQuote(setdiff(before, names(subset))), "]]",
          sep = "", collapse = ", "
        ),
        " empty after removal of NAs. ",
        ifelse(length(before) - length(subset) > 1, "Dimensions", "Dimension"),
        " not subsetted."
      )
    }
  }

  # Check if subset contains valid year subset
  check_year_subset(subset, meta_data, silent)

  if (file_type == "meta") {
    # Check format of the file linked in meta file
    if (is.null(meta_data$format)) {
      stop("Missing 'format' in meta file ", sQuote(filename))
    } else if (! meta_data$format %in% setdiff(supported_types, "meta")) {
      # Capture fringe case where the meta file links to a file in an
      # unsupported format
      stop("Format ", dQuote(meta_data$format), " specified in meta file ",
           sQuote(filename), " not supported.")
    }
    # Get filename from meta file
    if (basename(meta_data$filename) == meta_data$filename) {
      # meta_data$filename is in same directory as filename. Can use path from
      # filename.
      filename <- file.path(dirname(filename), meta_data$filename)
    } else {
      # meta_data$filename is in a different directory than filename. Need to
      # parse path.
      # Save current working directory.
      wd <- getwd()
      # Reset working directory if function exits (breaks, fails, etc.)
      on.exit(setwd(wd)) # nolint:undesirable_function_linter.
      # Set working directory to path of filename
      setwd(dirname(filename)) # nolint:undesirable_function_linter.
      # Relative path can be parsed now
      filename <- normalizePath(meta_data$filename)
      # Reset working directory
      setwd(wd) # nolint:undesirable_function_linter.
    }
  }
  # Derive file_header from meta_data. Set silent = TRUE here because any
  # warnings should have been triggered in read_io_metadata already.
  file_header <- meta_data$as_header(silent = TRUE)

  # Check file size
  expected_filesize <- unname(
    get_header_item(file_header, "ncell") *
    get_header_item(file_header, "nbands") *
    get_header_item(file_header, "nstep") *
    get_header_item(file_header, "nyear") *
    get_datatype(file_header)$size + start_offset
  )
  if (file.size(filename) != expected_filesize) {
    stop(
      "Unexpected file size (", file.size(filename), " bytes) of ", filename,
      "\nExpected size: ", expected_filesize, " bytes",
      "\nPlease check ",
      switch(file_type, meta = "meta file", clm = "header", "supplied"),
      " attributes."
    )
  }

  # Check whether nbands may actually be nstep
  if (!silent && get_header_item(file_header, "version") < 4 &&
    get_header_item(file_header, "nstep") == 1 &&
    get_header_item(file_header, "nbands") %in% c(12, 365)
  ) {
    message(
      "read_io: Detected \"nbands = ", get_header_item(file_header, "nbands"),
      "\" and \"nstep = 1\". If this is a ",
      ifelse(get_header_item(file_header, "nbands") == 12, "monthly", "daily"),
      " file consider setting \"nbands = 1\" and \"nstep = ",
      get_header_item(file_header, "nbands"),
      "\" to allow correct setting of the time axis."
    )
  }

  # Check if file is an LPJDAMS input file, which has a different format that is
  # not supported by this function. TODO: Implement drop-in function for LPJDAMS
  # input.
  if (get_header_item(file_header, "name") == "LPJDAMS") {
    stop(
      "This function currently does not support reading LPJDAMS input files."
    )
  }

  # Read data from binary file
  file_data <- read_io_data(filename, meta_data, subset, silent)

  # Update meta_data based on subset
  if (!is.null(subset$year) && is.numeric(subset$year)) {
    year_dimnames <- split_time_names(dimnames(file_data)[["time"]])$year
  } else {
    year_dimnames <- NULL
  }
  if (!is.null(subset$cell)) {
    cell_dimnames <- dimnames(file_data)[["cell"]]
  } else {
    cell_dimnames <- NULL
  }
  if (length(subset) > 0) {
    meta_data$.__update_subset__(subset,
                                 cell_dimnames = cell_dimnames,
                                 year_dimnames = year_dimnames)
  }
  # Adjust dimension order to dim_order
  if (!identical(dim_order, names(dim(file_data))))
    file_data <- aperm(file_data, perm = dim_order)

  # Create LPJmLData object and combine data and meta_data
  lpjml_data <- LPJmLData$new(data = file_data,
                              meta_data = meta_data)
  rm(file_data, meta_data)
  lpjml_data
}

# Read & assign metadata for binary file without a header
read_io_metadata_raw <- function(filename, file_type, band_names,
                                 version, order, firstyear, nyear, firstcell,
                                 ncell, nbands, cellsize_lon, scalar,
                                 cellsize_lat, datatype, nstep, timestep,
                                 endian, variable, descr, unit, name, silent) {
  # Create a dummy header with the information passed as arguments
  verbose <- (!is.null(version) && version < 4)
  verbose <- verbose && !silent
  file_header <- create_header(
    name = as.character(default(name, "LPJDUMMY")[1]),
    version = default(version, 4), # Default: use newest version
    order = default(order, 4), # Default: order used in most output files
    firstyear = default(firstyear, 1901),
    nyear = default(nyear, 1),
    firstcell = default(firstcell, 0),
    ncell = default(ncell, 67420), # Default: number of cells in global CRU
                                   # grid
    nbands = default(nbands, 1),
    cellsize_lon = default(cellsize_lon, 0.5),
    # Default: resolution of global CRU grid
    scalar = default(scalar, 1),
    cellsize_lat = default(cellsize_lat, default(cellsize_lon, 0.5)),
    # If not provided, default to same as cellsize_lon
    datatype = default(datatype, 3), # Default: float used in most output files
    nstep = default(nstep, 1),
    timestep = default(timestep, 1),
    endian = default(endian, .Platform$endian),
    # Default: endian used by operating system
    verbose = verbose
  )

  # Check validity of band_names
  check_band_names(get_header_item(file_header, "nbands"),
                   band_names)

  # Prepare additional attributes to be added to meta information
  additional_attributes <- list(band_names = band_names, variable = variable,
                                descr = descr, unit = unit)
  additional_attributes <-
    additional_attributes[which(!sapply(additional_attributes, is.null))] # nolint
  # Use header name is a substitute for variable if variable is not set
  if (is.null(additional_attributes[["variable"]])) {
    additional_attributes[["variable"]] <- get_header_item(file_header, "name")
  }
  # Generate meta_data
  meta_data <- LPJmLMetaData$new(x = file_header,
                                 additional_attributes = additional_attributes,
                                 data_dir = dirname(filename))
  meta_data
}

# Read & assign metadata for binary file with a header
read_io_metadata_clm <- function(filename, file_type, band_names,
                                 version, order, firstyear, nyear, firstcell,
                                 ncell, nbands, cellsize_lon, scalar,
                                 cellsize_lat, datatype, nstep, timestep,
                                 endian, variable, descr, unit, name, silent) {
  # Read file_header
  file_header <- read_header(filename, version, !silent)

  # Update header with the information passed as arguments. Especially for
  # version 1 and 2 headers default values may need to be overwritten.
  if (get_header_item(file_header, "version") > 3 && is.null(version)) {
    verbose <- FALSE
  } else if (!is.null(version) && version > 3) {
    verbose <- FALSE
  } else {
    verbose <- TRUE
  }
  verbose <- verbose && !silent

  # Some existing LPJmL input files use order = 0, which is not a valid order
  # value (1, 2, 3, 4 or corresponding string options). Reset order = 0 to
  # order = 1. # nolint:commented_code_linter.
  if (get_header_item(file_header, "order") == 0 && is.null(order)) {
    if (!silent)
      warning(
        "Header in file ", sQuote(filename),
        " has invalid order = 0. Setting to 1.\n",
        "Provide order as function argument if default is incorrect."
      )
    file_header <- set_header_item(file_header, order = 1)
  }

  # Do not allow overwriting name attribute in header because it may change the
  # header length, which needs to be skipped when reading data from file.
  if (!silent && !is.null(name)) {
    warning("You cannot overwrite the header name in clm files.")
  }
  file_header <- create_header(
    name = get_header_item(file_header, "name"),
    version = default(version, get_header_item(file_header, "version")),
    order = default(order, get_header_item(file_header, "order")),
    firstyear = default(firstyear, get_header_item(file_header, "firstyear")),
    nyear = default(nyear, get_header_item(file_header, "nyear")),
    firstcell = default(firstcell, get_header_item(file_header, "firstcell")),
    ncell = default(ncell, get_header_item(file_header, "ncell")),
    nbands = default(nbands, get_header_item(file_header, "nbands")),
    cellsize_lon = default(cellsize_lon,
                           get_header_item(file_header, "cellsize_lon")),
    scalar = default(scalar, get_header_item(file_header, "scalar")),
    cellsize_lat = default(cellsize_lat,
                           get_header_item(file_header, "cellsize_lat")),
    datatype = default(datatype, get_header_item(file_header, "datatype")),
    nstep = default(nstep, get_header_item(file_header, "nstep")),
    timestep = default(timestep, get_header_item(file_header, "timestep")),
    endian = default(endian, get_header_item(file_header, "endian")),
    verbose = verbose
  )

  # Check validity of band_names
  check_band_names(get_header_item(file_header, "nbands"),
                   band_names)

  # Prepare additional attributes to be added to meta information
  additional_attributes <- list(band_names = band_names, variable = variable,
                                descr = descr, unit = unit)
  additional_attributes <-
    additional_attributes[which(!sapply(additional_attributes, is.null))] # nolint
  # Use header name as a substitute for variable if variable is not set. Here,
  # use name argument if supplied by user.
  if (is.null(additional_attributes[["variable"]])) {
    additional_attributes[["variable"]] <- as.character(
      default(name, get_header_item(file_header, "name"))[1]
    )
  }

  # Offset at the start of the file before values begin
  additional_attributes[["offset"]] <- unname(get_headersize(file_header))

  # Generate meta_data
  meta_data <- LPJmLMetaData$new(x = file_header,
                                 additional_attributes = additional_attributes,
                                 data_dir = dirname(filename))
  meta_data
}

# Read & assign metadata for meta file type (binary file with associated
# meta-data json file)
read_io_metadata_meta <- function(filename, file_type, band_names,
                                  version, order, firstyear, nyear, firstcell,
                                  ncell, nbands, cellsize_lon, scalar,
                                  cellsize_lat, datatype, nstep, timestep,
                                  endian, variable, descr, unit, name, silent) {
  # Read meta data
  meta_data <- read_meta(filename)

  # Check if user has tried to overwrite any meta attributes which are set
  # already in the JSON. If so, give a warning but still allow for meta files.
  set_args <- setdiff(
    names(formals()),
    c("filename", "file_type", "silent")
  )
  # Filter arguments that are NULL
  set_args <- set_args[which(!sapply(set_args, function(x) is.null(get(x))))] # nolint:undesirable_function_linter.

  # Only warn about arguments that are currently set in metadata
  overwrite_set_args <- intersect(
    set_args,
    names(which(!sapply(meta_data, is.null))) # nolint:undesirable_function_linter.
  )
  if (length(overwrite_set_args) > 0 && !silent) {
    warning(
      "You are trying to overwrite the following parameters, which are already",
      " set for this file: ",
      toString(sQuote(overwrite_set_args)),
      call. = FALSE
    )
  }
  # Override attributes
  for (att in overwrite_set_args) {
    meta_data$.__set_attribute__(att, get(att))
  }
  # Remove arguments that are set/updated already
  set_args <- setdiff(set_args, overwrite_set_args)

  # If user wants band_names check consistency with nbands
  if (!"nbands" %in% set_args) {
    nbands <- default(meta_data$nbands, 1)
  }
  if ("band_names" %in% set_args) {
    if (length(band_names) != nbands) {
      stop(
        "Provided band_names ",
        toString(
          dQuote(
            if (length(band_names) > 6) {
                c(utils::head(band_names, n = 4), "...",
                  utils::tail(band_names, n = 1))
            } else {
              band_names
            }
          )
        ),
        " do not match number of bands in file: ",
        length(band_names), "!=", nbands
      )
    }
  }

  if (!"band_names" %in% set_args && is.null(meta_data$band_names) &&
    !is.null(meta_data$map) && !is.null(nbands)
  ) {
    if (length(meta_data$map) == nbands / 2) {
      # Create band_names from the map attribute that is included in meta data.
      # This assumes that the map contains the band names without "rainfed/
      # irrigated" qualifier.
      band_names <- paste(
        rep(c("rainfed", "irrigated"), each = length(meta_data$map)),
        meta_data$map
      )
      set_args <- c(set_args, "band_names")
      if (!silent)
        message(
          "Setting automatically generated band_names based an $map attribute"
        )
    }
    if (length(meta_data$map) == nbands / 4) {
      # Create band_names from the map attribute that is included in meta data.
      # This assumes that the map contains the band names without "rainfed/
      # irrigation system" qualifier.
      band_names <- paste(
        rep(
          c("rainfed", "surface-irrigated", "sprinkler-irrigated",
            "drip-irrigated"),
          each = length(meta_data$map)
        ),
        meta_data$map
      )
      set_args <- c(set_args, "band_names")
      if (!silent)
        message(
          "Setting automatically generated band_names based an $map attribute"
        )
    }
  }
  # Prepare additional attributes to be added to metadata
  additional_attributes <- sapply(set_args, function(x) get(x), simplify = FALSE) # nolint

  # Update meta_data
  meta_data$initialize(x = meta_data$as_list(),
                       additional_attributes = additional_attributes)

  # Convert meta data into header
  file_header <- meta_data$as_header(silent)

  # Check validity of band_names
  check_band_names(get_header_item(file_header, "nbands"),
                   meta_data$band_names)

  meta_data
}

# Read data (instead of meta data) from file
read_io_data <- function(
  filename,
  meta_data,
  subset,
  silent
) {
  # Determine all years in the file
  years <- seq(
    from       = default(meta_data$firstyear, 1901),
    by         = default(meta_data$timestep, 1),
    length.out = default(meta_data$nyear, 1)
  )
  # Years to read
  if ("year" %in% names(subset)) {
    if (is.numeric(subset[["year"]])) {
      years <- years[subset[["year"]]]
    } else {
      years <- as.integer(subset[["year"]])
    }
  }

  # Open binary file connection
  file_connection <- file(filename, "rb")
  # Ensure that file connection is closed even if function is terminated with an
  # error
  on.exit(if (exists("file_connection")) close(file_connection)) # nolint:undesirable_function_linter.

  # Dimension order during reading. Note: Must be 3 dimensions in total, with
  # "time" being last dimension for code below to work.
  read_band_order <- c("cell", "band", "time")
  # Loop over subset years
  for (yy in years) {
    # Compute offset
    data_offset <- (yy - default(meta_data$firstyear, 1901)) /
      default(meta_data$timestep, 1) * meta_data$ncell *
      default(meta_data$nbands, 1) * default(meta_data$nstep, 1) *
      get_datatype(meta_data$datatype)$size +
      default(meta_data$offset, 0)

    # Number of values to read for one year
    n_values <- meta_data$ncell * default(meta_data$nbands, 1) *
      default(meta_data$nstep, 1)

    # Read data for one year from binary file
    year_data <- read_raw(
      file_connection,
      data_offset = data_offset,
      n_values = n_values,
      datatype = get_datatype(meta_data$datatype),
      endian = ifelse(meta_data$bigendian, "big", "little")
    ) * default(meta_data$scalar, 1)

    # Convert to array.
    # Note: order of nbands and nstep for "cellyear" (order = 1) is currently
    # not defined in LPJmL.
    dim(year_data) <- switch(
      default(meta_data$order, "cellyear"),
      cellyear = c(
        band = unname(default(meta_data$nbands, 1)),
        time = unname(default(meta_data$nstep, 1)),
        cell = unname(meta_data$ncell)
      ),
      yearcell = stop("Order yearcell not supported"),
      cellindex = stop("Order cellindex not supported"),
      cellseq = c(
        cell = unname(meta_data$ncell),
        band = unname(default(meta_data$nbands, 1)),
        time = unname(default(meta_data$nstep, 1))
      )
    )

    # Assign dimension names to array.
    # Ensure cell or band indices are not written in scientific notation.
    band_names <- default(
      meta_data$band_names, seq_len(default(meta_data$nbands, 1))
    ) %>%
      format(trim = TRUE, scientific = FALSE, justify = "none")

    cell_dimnames <- format(
      seq(default(meta_data$firstcell, 0), length.out = meta_data$ncell),
      trim = TRUE, scientific = FALSE, justify = "none"
    )

    dimnames(year_data) <- switch(
      default(meta_data$order, "cellyear"),
      cellyear  = list(                                                # order 1
        band = band_names,
        time = NULL, # Assign dates later
        cell = cell_dimnames
      ),
      yearcell  = stop("Order yearcell not supported"),                # order 2
      cellindex = stop("Order cellindex not supported"),               # order 3
      cellseq   = list(                                                # order 4
        cell = cell_dimnames,
        band = band_names,
        time = NULL # Assign dates later
      )
    )

    # Convert to read_band_order and apply subsetting along bands or cells
    index <- which(!names(subset) %in%
      c("day", "month", "year", "time")
    )

    year_data <- aperm(year_data, perm = read_band_order) %>%
      # Apply any subsetting along bands or cells
      subset_array(
        subset[index],
        drop = FALSE,
        silent = silent
      )

    # Concatenate years together
    if (yy == years[1]) {
      # Allocate full array for all years
      file_data <- array(
        dim = dim(year_data) * ifelse(
          names(dimnames(year_data)) == "time",
          length(years),
          1
        )
      )
      names(dim(file_data)) <- names(dim(year_data))
      # Setting dimnames like this will only work if the dimnames of time
      # dimension are set to NULL above. Otherwise, length will be wrong for
      # time dimension.
      dimnames(file_data) <- dimnames(year_data)
      # Assign year_data to time indices of first year in full array
      time_index <- seq_len(dim(year_data)["time"])
      file_data[, , time_index] <- year_data
    } else {
      # Increment time index
      time_index <- time_index + dim(year_data)["time"]
      # Assign year_data to time indices of current year in full array
      file_data[, , time_index] <- year_data
    }
  }
  # Close binary file connection
  close(file_connection)
  # Delete file_connection to prevent triggering on.exit expression
  rm(file_connection)

  # Create and assign time dimension names
  time_dimnames <- create_time_names(
    nstep = default(meta_data$nstep, 1),
    years = years
  )
  dimnames(file_data)$time <- time_dimnames

  file_data
}


# Function to read values from LPJmL binary files
read_raw <- function(file_connection, data_offset, n_values, datatype, endian) {
  seek(con = file_connection, where = data_offset)
  file_data <- readBin(
    file_connection,
    what = datatype$type,
    n = n_values,
    size = datatype$size,
    signed = datatype$signed,
    endian = endian
  )
  file_data
}


# Simple validity check for band_names
check_band_names <- function(nbands, band_names) {
  if (!is.null(band_names) &&
    length(band_names) != nbands
  ) {
    stop(
      "Provided band_names ",
      toString(
        dQuote(
          if (length(band_names) > 6) {
              c(utils::head(band_names, n = 4), "...",
                utils::tail(band_names, n = 1))
          } else {
            band_names
          }
        )
      ),
      " do not match number of bands in file: ",
      length(band_names), "!=", nbands,
      call. = FALSE
    )
  }
}

# Simple validity check for year subset
check_year_subset <- function(subset, meta_data, silent = FALSE) {
  if ("year" %in% names(subset)) {
    if (is.numeric(subset[["year"]])) {
      outside_range <- which(
        as.integer(subset[["year"]]) < 1 |
        as.integer(subset[["year"]]) > default(meta_data$nyear, 1)
      )
      if (length(outside_range) > 0) {
        stop(
          ifelse(length(outside_range) > 1, "Years ", "Year "),
          toString(subset[["year"]][outside_range]),
          " provided as subset[[", dQuote("year"), "]] outside of file range"
        )
      }
    } else if (is.character(subset[["year"]])) {
      file_years <- seq(
        from = default(meta_data$firstyear, 1901),
        by = default(meta_data$timestep, 1),
        length.out = default(meta_data$nyear, 1)
      )
      outside_range <- setdiff(subset[["year"]], file_years)
      if (length(outside_range) > 0) {
        stop(
          ifelse(length(outside_range) > 1, "Years ", "Year "),
          toString(dQuote(outside_range)),
          " provided as subset[[", dQuote("year"), "]] outside of file range"
        )
      }
    } else {
      stop(
        "Unsupported type ", sQuote(typeof(subset[["year"]])),
        " provided as subset[[", dQuote("year"), "]]"
      )
    }
    if (length(unique(subset[["year"]])) != length(subset[["year"]]) && !silent) {
      warning(
        "Removing ",
        length(subset[["year"]]) - length(unique(subset[["year"]])),
        " duplicate ",
        ifelse(length(subset[["year"]]) - length(unique(subset[["year"]])) > 1,
               "entries", "entry"),
        " from provided subset[[", dQuote("year"), "]]"
      )
    }
  }
  invisible(subset)
}

# Utility function to replace missing attribute with default value
default <- function(value, default) {
  if (is.null(value)) {
    return(default)
  } else {
    return(value)
  }
}
# file_type options supported by read_io
supported_types <- c("raw", "clm", "meta")
