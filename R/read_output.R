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
#' @param subset_list A list defining the dimensions to be subset
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
  subset_list  = NULL,
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

  if (file_type == "raw") {

    # Create a dummy header with info passed as arguments
    dummy_header <- create_header(
      name = "LPJDUMMY", version, order, firstyear, nyear, firstcell, ncell,
      nbands, cellsize_lon, scalar, cellsize_lat, datatype, endian, verbose
      )

    # Read raw file
    file_data <- read_raw(fname, header = dummy_header)

  } else if (file_type == "clm") {

  # read_clm()

  } else if (file_type == "meta") {

  # read_meta()

  }

  if (!is.null(band_names)) {
    # Assign dimnames [cellnr, time, nbands]
  }

  return(file_data)
}


# Function to read LPJmL raw files
read_raw <- function(
  fname      = "",
  header,           # a header object in the format return by `write_header()`
  start_year = NULL,
  end_year   = NULL
) {

  # Get information from header
  datatype  <- get_datatype(header)
  ncell     <- get_header_item(header, "ncell")
  nbands    <- get_header_item(header, "nbands")
  firstyear <- get_header_item(header, "firstyear")
  nyear     <- get_header_item(header, "nyear")
  scalar    <- get_header_item(header, "scalar")

  start_year <- ifelse(is.null(start_year), firstyear, start_year)
  end_year   <- ifelse(is.null(end_year),   firstyear + nyear - 1, end_year)

  vector_offset <- (start_year - firstyear) * ncell * nbands * datatype$size

  if ("nstep" %in% names(header$header)) {
    nstep <- get_header_item(header, "nstep")
  } else {
    nstep <- 1
  }

  # Check file size
  cat(paste("\nFile size (", file.size(fname), ") as expected = ",
            file.size(fname) / ncell / nbands / nstep / nyear == datatype$size,
            "\n"))

  # Calculate nr. of values to read
  nvalue <- ncell * nbands * nstep * (end_year - start_year + 1)

  # Read binary file
  file_connection <-  file(fname, "rb")
  seek(con = file_connection, where = vector_offset)
  file_data <- readBin(file_connection, n = nvalue, what = datatype$type,
                       size = datatype$size, signed = datatype$signed) * scalar
  close.connection(file_connection)

  return(file_data)
}