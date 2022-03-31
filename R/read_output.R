#' @title Read LPJmL output file
#'
#' @description Generic function to read LPJmL output files in different
#' formats. Depending on the format, arguments can be automatically detected,
#' or have to be passed on as individual arguments.
#'
#' @param file_name A string value giving the output-file name to read, including
#' its path and extension. If file_type == "meta", file_name should be a json file.
#' @param file_type A string value giving the output-file type. Valid options:
#' `raw`, a binary file without header;
#' `clm`, a binary file with header;
#' `meta` (default), a raw file complemented by a meta-information json file.
#' @param band_names A vector of strings providing the band names or
#' NULL to determine automatically from the metafile.
#' @param nstep A integer value defining the time step of the output file.
#' Valid values are 1 (yearly), 12 (monthly), 365 (daily).
#' @param subset_list A list defining the dimensions and their values
#' to be subset
#' @param version Integer indicating CLM-file header version,
#' between 1, 2, 3 or 4.
#' @param order Order of data items (see LPJmL code for supported values;
#' default: 1).
#' @param firstyear First year of data in the file.
#' @param nyear Number of years of data included in the file.
#' @param firstcell Index of first data item.
#' @param ncell Number of data items per band.
#' @param nbands Number of bands per year of data.
#' @param cellsize_lon Longitude cellsize in deg.
#' @param scalar Conversion factor applied to data when it is read by LPJmL.
#' @param cellsize_lat Latitude cellsize in deg.
#' @param datatype LPJmL data type in file (see LPJmL code for valid data type
#' codes)
#' @param endian Endianness to use for file (either "big" or "little", by
#'   default uses platform-specific endianness `.Platform$endian`).
#' @return
#' @examples
#' @details
#' @seealso
#' @export

read_output <- function(
  file_name        = "file_name.bin",
  file_type    = "meta",
  band_names   = NULL,
  nstep        = NULL,
  timestep     = NULL,
  subset_list  = list(),
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
  endian       = NULL
) {

  cat(paste("\nReading:", "\n----------------------------\n", file_name))

  file_type <- match.arg(file_type, c("raw", "clm", "meta"))

  if (file_type == "raw") {
    # Read raw file type (binary file without a header)

    # Create a dummy header with the info passed as arguments
    if (is.null(version)) {
      verbose <- FALSE
    } else if (version < 4) {
      verbose <- TRUE
    } else {
      verbose <- FALSE
    }

    file_header <- create_header(
      name         = "LPJDUMMY",
      version      = ifelse(is.null(version), 4, version),
      order        = ifelse(is.null(order), 4, order),
      firstyear    = ifelse(is.null(firstyear), 1901, firstyear),
      nyear        = ifelse(is.null(nyear), 1, nyear),
      firstcell    = ifelse(is.null(firstcell), 0, firstcell),
      ncell        = ifelse(is.null(ncell), 67420, ncell),
      nbands       = ifelse(is.null(nbands), 1, nbands),
      cellsize_lon = ifelse(is.null(cellsize_lon), 0.5, cellsize_lon),
      scalar       = ifelse(is.null(scalar), 1, scalar),
      cellsize_lat = ifelse(is.null(cellsize_lat), 0.5, cellsize_lat),
      datatype     = ifelse(is.null(datatype), 3, datatype),
      nstep        = ifelse(is.null(nstep), 1, nstep),
      timestep     = ifelse(is.null(timestep), 1, timestep),
      endian       = ifelse(is.null(endian), .Platform$endian, endian),
      verbose      = verbose
    )

    # Offset at the start of the file before values begin
    start_offset <- 0

  } else if (file_type == "clm") {
    # Read clm file type (binary file with a LPJmL header)

    # Read file_header
    file_header <- read_header(file_name)

    # Update header with the info passed as arguments (especially for version 1
    # and 2 header values may need to be overwritten)
    if (get_header_item(file_header, "version") > 3 && is.null(version)) {
      verbose <- FALSE
    } else if (!is.null(version) && version > 3) {
      verbose <- FALSE
    } else {
      verbose <- TRUE
    }

    file_header <- create_header(
      name         = get_header_item(file_header, "name"),
      version      = ifelse(
        is.null(version),
        get_header_item(file_header, "version"),
        version
      ),
      order        = ifelse(
        is.null(order),
        get_header_item(file_header, "order"),
        order
      ),
      firstyear    = ifelse(
        is.null(firstyear),
        get_header_item(file_header, "firstyear"),
        firstyear
      ),
      nyear        = ifelse(
        is.null(nyear),
        get_header_item(file_header, "nyear"),
        nyear
      ),
      firstcell    = ifelse(
        is.null(firstcell),
        get_header_item(file_header, "firstcell"),
        firstcell
      ),
      ncell        = ifelse(
        is.null(ncell),
        get_header_item(file_header, "ncell"),
        ncell
      ),
      nbands       = ifelse(
        is.null(nbands),
        get_header_item(file_header, "nbands"),
        nbands
      ),
      cellsize_lon = ifelse(
        is.null(cellsize_lon),
        get_header_item(file_header, "cellsize_lon"),
        cellsize_lon
      ),
      scalar       = ifelse(
        is.null(scalar),
        get_header_item(file_header, "scalar"),
        scalar
      ),
      cellsize_lat = ifelse(
        is.null(cellsize_lat),
        get_header_item(file_header, "cellsize_lat"),
        cellsize_lat
      ),
      datatype     = ifelse(
        is.null(datatype),
        get_header_item(file_header, "datatype"),
        datatype
      ),
      nstep        = ifelse(
        is.null(nstep),
        get_header_item(file_header, "nstep"),
        nstep
      ),
      timestep     = ifelse(
        is.null(timestep),
        get_header_item(file_header, "timestep"),
        timestep
      ),
      endian       = ifelse(
        is.null(endian),
        get_header_item(file_header, "endian"),
        endian
      ),
      verbose      = verbose
    )

    # Offset at the start of the file before values begin
    start_offset <- get_headersize(file_header)


  } else if (file_type == "meta") {
    # Read meta file type (binary file with associated meta-data json file)

    # Read meta data
    meta_data <- read_meta(file_name)

    # Derive header from meta data
    file_header <- meta_data$as_header()

    # Update header with the info passed as arguments (especially for version 1
    # and 2 header values may need to be overwritten)
    if (get_header_item(file_header, "version") > 3 && is.null(version)) {
      verbose <- FALSE
    } else if (!is.null(version) && version > 3) {
      verbose <- FALSE
    } else {
      verbose <- TRUE
    }

    # To Do: catch to check if any unnecessary arguments are passed and return a warning

    # Offset at the start of the file before values begin
    # Confirm if JSON reflects if it is actually for a CLM file. Otherwise, need
    # to devise a test.
    if (is.null(meta_data$offset)) {
      start_offset <- 0
    } else {
      start_offset <- meta_data$offset
    }

    # Get file_name from meta file
    file_name <- meta_data$filename
  }

  # Check file size
  expected_filesize <- get_header_item(file_header, "ncell") *
    get_header_item(file_header, "nbands") *
    get_header_item(file_header, "nstep") *
    get_header_item(file_header, "nyear") *
    get_datatype(file_header)$size + start_offset

  if (file.size(file_name) != expected_filesize) {
    stop(
      paste0(
        "Unexpected file size (", file.size(file_name), ") of ", file_name,
        "\nExpected size: ", expected_filesize,
        "\nPlease check ",
        ifelse(file_type == "meta", "meta file", "header"),
        " attributes."
      )
    )
  }

  check_subset(subset_list, file_header, band_names)

  # # Years subset
  # if (! "year" %in% names(subset_list)) {
  #   # if not provided, read all years
  #   read_years <- years
  # } else {
  #   if (!all(subset_list[["year"]] %in% years)) {
  #     stop("Not all selected years are in file!")
  #   }
  #   read_years <- subset_list[["year"]]
  # }

  # # Check band names
  # if (!is.null(band_names)) {
  #   if (length(band_names) != get_header_item(file_header, "nbands")) {
  #     stop("Provided band_names attribute has wrong length")
  #   }
  # }

  if ("year" %in% names(subset_list)) {
    years <- subset_list[["year"]]
  } else {
    # All years in the file
    years <- seq(
      from       = get_header_item(file_header, "firstyear"),
      by         = get_header_item(file_header, "timestep"),
      length.out = get_header_item(file_header, "nyear")
    )
  }


  # Open binary file connection
  file_connection <- file(file_name, "rb")

  # Loop over subset years
  for (yy in years) {

    # Compute offset
    data_offset <- (yy - get_header_item(file_header, "firstyear")) /
      get_header_item(file_header, "timestep") *
      get_header_item(file_header, "ncell") *
      get_header_item(file_header, "nbands") *
      get_header_item(file_header, "nstep") *
      get_datatype(file_header)$size + start_offset

    # Number of values to read for one year
    n_values <- get_header_item(file_header, "ncell") *
      get_header_item(file_header, "nbands") *
      get_header_item(file_header, "nstep")


    # Read data from binary file
    file_data <- read_raw(
      file_connection,
      data_offset = data_offset,
      n_values = n_values,
      datatype = get_datatype(file_header),
      endian = get_header_item(file_header, "endian")
    )

    # Convert to array
    # Confirm order of nbands and nstep for "cellyear"
    dim(file_data) <- switch(
      get_header_item(file_header, "order"),
      c(band = get_header_item(file_header, "nbands"),
        time = get_header_item(file_header, "nstep"),
        cell = get_header_item(file_header, "ncell")
      ),
      stop("Order yearcell not supported"),
      stop("Order cellindex not supported"),
      c(cell = get_header_item(file_header, "ncell"),
        band = get_header_item(file_header, "nbands"),
        time = get_header_item(file_header, "nstep")
      )
    )
  }



  # Assign dimnames [cellnr, time, nbands]


  return(file_data)
}


# Function to read LPJmL raw files
read_raw <- function(
  file_connection,
  data_offset,
  n_values,
  datatype,
  endian
) {
  seek(con = file_connection, where = data_offset)
  file_data <- readBin(
    file_connection,
    n = n_values,
    what = datatype$type,
    size = datatype$size,
    signed = datatype$signed,
    endian = endian
  )
  return(file_data)
}


check_subset <- function(subset_list, header, band_names) {
    if (!is.null(subset_list[["year"]])) {
      years <- seq(
        from       = get_header_item(header, "firstyear"),
        by         = get_header_item(header, "timestep"),
        length.out = get_header_item(header, "nyear")
      )
      if (!all(subset_list[["year"]] %in% as.character(years))) {
        stop("subset_list[[\"year\"]] does not match years in file.")
      }
    }
    if (!is.null(subset_list[["month"]])) {
      warning(paste0(
        "Using \"month\" as subset is currently not supported in this context ",
        "and thus will be ignored."
      ))
    }
    if (!is.null(subset_list[["day"]])) {
      warning(paste0(
        "Using \"day\" as subset is currently not supported in this context ",
        "and thus will be ignored."
      ))
    }
    if (!is.null(subset_list[["cell"]])) {
    }
    if (!is.null(subset_list[["band"]])) {
    }
    if (
      any(!names(subset_list) %in% c("cell", "year", "month", "day", "band"))
    ) {
      warning(paste0("\"", names(subset_list)[
        which(
          !names(subset_list) %in% c("cell", "year", "month", "day", "band")
        )
      ], "\" is not a valid subset name and will be ignored."))
    }
}