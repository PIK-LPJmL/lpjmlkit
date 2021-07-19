#' @title newHeader
#'
#' @description Create a header in the format required by writeHeader().
#'
#' @param name Header name attribute (default: "LPJGRID")
#' @param version CLM version to use (default: 3)
#' @param order Order of data items (see LPJmL code for supported values; default: 1)
#' @param firstyear Start year of data in file (default: 1901)
#' @param nyear Number of years of data included in file (default: 1)
#' @param firstcell Index of first data item (default: 0)
#' @param ncell Number of data items per band
#' @param nbands Number of bands per year of data (default: 2)
#' @param cellsize_lon Longitude cellsize in deg (default: 0.5)
#' @param scalar Conversion factor applied to data when it is read by LPJmL (default: 0.01)
#' @param cellsize_lat Latitude cellsize in deg (default: cellsize_lon)
#' @param datatype LPJmL data type in file (see LPJmL code for valid data type codes; default: 1)
#' @param endian: Endianness to use for file (either "big" or "little", by default uses
#'     platform-specific endianness .Platform$endian)
#'
#' @return The function returns a list with 3 components:
#'     - name
#'     - header: vector of header values ('version', 'order', 'firstyear',
#'       'nyear', 'firstcell', 'ncell', 'nbands', 'cellsize_lon', 'scalar',
#'       'cellsize_lat', 'datatype')
#'     - endian
#'
#' @examples
#' newHeader(name="LPJGRID", version=3, order=1, firstyear=1901, nyear=1, firstcell=0,
#'           ncell=67420, nbands=12, cellsize_lon=0.5, scalar=1, cellsize_lat=0.5, datatype=1,
#'           endian=.Platform$endian)
#'
#' @details File headers in input files are used by LPJmL to determine the structure
#'    of the file and how to read it. Header names usually start with "LPJ" followed by
#'    a word or abbreviation describing the type of input data. See LPJmL code for valid header names.
#'    The version number determines the amount of header information included in the file. All versions
#'    save the header name and header attributes 'version', 'order', 'firstyear', 'nyear', 'firstcell',
#'    'ncell', and 'nbands'. Header versions 2 and 3 add header attributes 'cellsize_lon' and 'scalar'.
#'    Header version 3 adds header attributes 'cellsize_lat' and 'datatype'.
#'    Valid values for 'order' are 1, 2, 3, and 4. The default for LPJmL input files is 1. The default
#'    for LPJmL output files is 4, except for grid output files which also use 1.
#'    By default, input files contain data for all cells, indicated by setting the 'firstcell' index to 0.
#'    If 'firstcell' > 0, LPJmL assumes the first 'firstcell' cells to be missing in data.
#'    Valid codes for the 'datatype' attribute and the corresponding LPJmL data types are: 0 (LPJ_BYTE),
#'    1 (LPJ_SHORT), 2 (LPJ_INT), 3 (LPJ_FLOAT), 4 (LPJ_DOUBLE).
#'    Default parameters of the function are valid for grid input files.
#'
#' @seealso [readHeader()] for reading headers from LPJmL files and [writeHeader()] for writing headers to files.
#'
#' @export
newHeader <- function(name="LPJGRID", version=3, order=1, firstyear=1901, nyear=1, firstcell=0, ncell, nbands=2, cellsize_lon=0.5, scalar=0.01, cellsize_lat=cellsize_lon, datatype=1, endian=.Platform$endian) {
  header <- list()
  if(is.character(name) && length(name) == 1) {
    header[["name"]] <- name
  } else {
    stop("name must be a character vector of length 1")
  }
  if(substr(header[["name"]], 1, 3) != "LPJ")
    warning(paste("Header name", sQuote(name), "is probably invalid for use in LPJmL"))
  header[["header"]] <- numeric(0)
  # check that valid values have been provided for all parameters included header version 1
  for(check in c("version", "order", "firstyear", "nyear", "firstcell", "ncell", "nbands")) {
    if(is.numeric(get(check)) && length(get(check)) == 1 && get(check)==as.integer(get(check))) {
      header[["header"]] <- c(header[["header"]], get(check))
      names(header[["header"]])[length(header[["header"]])] <- check
    } else {
      stop(paste(sQuote(check), "must be an integer of length 1"))
    }
  }
  if(version >= 2) {
    # check that valid values have been provided for additional parameters in header version 2
    for(check in c("cellsize_lon", "scalar")) {
      if(is.numeric(get(check)) && length(get(check)) == 1) {
        header[["header"]] <- c(header[["header"]], get(check))
        names(header[["header"]])[length(header[["header"]])] <- check
      } else {
        stop(paste(sQuote(check), "must be a float of length 1"))
      }
    }
    # check that valid values have been provided for additional parameters in header version 3
    if(version >= 3) {
      if(is.numeric(cellsize_lat) && length(cellsize_lat)==1) {
        header[["header"]] <- c(header[["header"]], cellsize_lat=as.double(cellsize_lat))
      } else {
        stop("cellsize_lat must be a float of length 1")
      }
      if(length(datatype)==1) {
        if(!is.null(lpjDatatype(c(header[["header"]], datatype=datatype)))) {
          header[["header"]] <- c(header[["header"]], datatype=as.integer(datatype))
          print(paste0("Setting datatype to ", ifelse(datatype < 1, "unsigned ", ""), typeof(lpjDatatype(header)$type), " with size ", lpjDatatype(header)$size))
        } else {
          stop(paste("Unknown datatype", datatype))
        }
      } else {
        stop("datatype must be integer of length 1")
      }
    } else {
      # add defaults
      header[["header"]] <- c(header[["header"]], cellsize_lat = as.double(header[["header"]]["cellsize_lon"]), datatype = 1)
      warning("Type 2 header. Adding default value for datatype which may not be correct in all cases")
    }
  } else {
    header[["header"]] <- c(header[["header"]], cellsize_lon = 0.5, scalar = 1, cellsize_lat = 0.5, datatype = 1)
    warning("Type 1 header. Adding default values for resolution, scalar and datatype which may not be correct in all cases")
  }
  header[["endian"]] <- endian
  return(header)
}
