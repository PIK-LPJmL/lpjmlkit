#' @title Read LPJmL output file
#'
#' @description Generic function to read LPJmL output files in different
#' formats. Depending on the format, arguments can be automatically detected,
#' or have to be passed on as individual arguments.
#'
#' @param fname A string value giving the output-file name to read, including
#' its path and extension.
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
  fname        = "file_name.bin",
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
  endian       = NULL,
  verbose      = FALSE
) {

  cat(paste("\nReading:", "\n----------------------------\n", fname))

  file_type <- match.arg(file_type, c("raw", "clm", "meta"))

  # Read raw file type (binary file without a header)
  if (file_type == "raw") {

    if (is.null(version)) {
      verbose <- FALSE
    } else if (version < 4) {
      verbose <- TRUE
    } else {
      verbose <- FALSE
    }

    # Create a dummy header with the info passed as arguments
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

    # Check file size
    expected_filesize <- get_header_item(file_header, "ncell") *
      get_header_item(file_header, "nbands") *
      get_header_item(file_header, "nstep") *
      get_header_item(file_header, "nyear") *
      get_datatype(file_header)$size

  # Read clm file type (binary file with a LPJmL header)
  } else if (file_type == "clm") {

  # read file_header

  # Read meta file type (binary file with associated meta-data json file)
  } else if (file_type == "meta") {

  # Get fname from meta file

  # crate file_header using info from meta file

  }

    # All years in the file
    years <- seq(from       = get_header_item(file_header, "firstyear"),
                 by         = get_header_item(file_header, "timestep"),
                 length.out = get_header_item(file_header, "nyear"))

    # Years subset
    if (! "years" %in% names(subset_list)) {
      subset_list[["years"]] <- years       # if not provided, read all years
    } else {
      if (!all(subset_list[["years"]] %in% years)) {
        stop("Not all selected years are in file!")
      }
    }

    # Open binary file connection
    file_connection <- file(fname, "rb")

    # Loop over subset years
    for (yy in subset_list[["years"]]) {

      # Compute offset
      # ...

      # Read data from binary file
      file_data <- read_raw(file_connection,
        data_offset = data_offset,
        nvalue,
        datatype = get_datatype(file_header),
        endian = get_header_item(file_header, "endian")
      )
    }

  # Convert to array
  if ((end_year - start_year + 1) == 1) {
    dim(file_data) <- c(ncell, nbands)
  } else {
    dim(file_data) <- c(ncell, nbands, end_year - start_year + 1)
  }

  # Assign dimnames [cellnr, time, nbands]
  if (!is.null(band_names)) {

  }

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
  file_data <- readBin(file_connection, n = n_values, what = datatype$type,
                        size = datatype$size, signed = datatype$signed,
                        endian = endian)
  return(file_data)
}