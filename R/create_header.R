#' @title Create new LPJmL file header from scratch.
#'
#' @description Create a header in the format required by `write_header()`.
#'
#' @param name Header name attribute (default: "LPJGRID").
#' @param version CLM version to use (default: 3).
#' @param order Order of data items (see LPJmL code for supported values;
#'   default: 1).
#' @param firstyear Start year of data in file (default: 1901).
#' @param nyear Number of years of data included in file (default: 1).
#' @param firstcell Index of first data item (default: 0).
#' @param ncell Number of data items per band.
#' @param nbands Number of bands per year of data (default: 2).
#' @param cellsize_lon Longitude cellsize in deg (default: 0.5).
#' @param scalar Conversion factor applied to data when it is read by LPJmL
#'   (default: 1.0).
#' @param cellsize_lat Latitude cellsize in deg (default: same as cellsize_lon).
#' @param datatype LPJmL data type in file (see LPJmL code for valid data type
#'   codes; default: 3).
#' @param nstep Number of time steps per year, added in header version 4 to
#'   separate time bands from content bands (default: 1).
#' @param endian Endianness to use for file (either "big" or "little", by
#'   default uses platform-specific endianness `.Platform$endian`).
#' @param verbose If TRUE (the default), function provides some feedback on
#'   datatype and when using default values for missing parameters. If FALSE,
#'   only errors are reported.
#'
#' @return The function returns a list with 3 components:
#' * name: The header name, e.g. "LPJGRID".
#' * header: Vector of header values ('version', 'order', 'firstyear',
#'     'nyear', 'firstcell', 'ncell', 'nbands', 'cellsize_lon', 'scalar',
#'     'cellsize_lat', 'datatype', 'nstep').
#' * endian: Endian used to write binary data, either "little" or "big".
#'
#' @examples
#' header <- create_header(
#'   name = "LPJGRID",
#'   version = 3,
#'   order = 1,
#'   firstyear = 1901,
#'   nyear = 1,
#'   firstcell = 0,
#'   ncell = 67420,
#'   nbands = 2,
#'   cellsize_lon = 0.5,
#'   scalar = 0.01,
#'   cellsize_lat = 0.5,
#'   datatype = 1,
#'   nstep = 1,
#'   endian = .Platform$endian,
#'   verbose = TRUE
#' )
#'
#' @details
#' File headers in input files are used by LPJmL to determine the
#' structure of the file and how to read it.
#'
#' Header names usually start with "LPJ" followed by a word or abbreviation
#' describing the type of input data. See LPJmL code for valid header names.
#'
#' The version number determines the amount of header information included in
#' the file. All versions save the header name and header attributes 'version',
#' 'order', 'firstyear', 'nyear', 'firstcell', 'ncell', and 'nbands'. Header
#' versions 2 and 3 add header attributes 'cellsize_lon' and 'scalar'. Header
#' version 3 adds header attributes 'cellsize_lat' and 'datatype'.
#'
#' Valid values for 'order' are 1, 2, 3, and 4. The default for LPJmL input
#' files is 1. The default for LPJmL output files is 4, except for grid output
#' files which also use 1.
#'
#' By default, input files contain data for all cells, indicated by setting
#' the 'firstcell' index to 0. If 'firstcell' > 0, LPJmL assumes the first
#' 'firstcell' cells to be missing in data.
#'
#' Valid codes for the 'datatype' attribute and the corresponding LPJmL data
#' types are: 0 (LPJ_BYTE), 1 (LPJ_SHORT), 2 (LPJ_INT), 3 (LPJ_FLOAT),
#' 4 (LPJ_DOUBLE).
#'
#' Default parameters of the function are valid for grid input files.
#'
#' @seealso
#' * [read_header()] for reading headers from LPJmL files.
#' * [write_header()] for writing headers to files.
#'
#' @export
create_header <- function(name = "LPJGRID",
                          version = 3,
                          order = 1,
                          firstyear = 1901,
                          nyear = 1,
                          firstcell = 0,
                          ncell,
                          nbands = 2,
                          cellsize_lon = 0.5,
                          scalar = 1,
                          cellsize_lat = cellsize_lon,
                          datatype = 3,
                          nstep = 1,
                          endian = .Platform$endian,
                          verbose = TRUE
                         ) {
  header <- list()
  if (is.character(name) && length(name) == 1) {
    header[["name"]] <- name
  } else {
    stop("name must be a character vector of length 1")
  }
  if (substr(header[["name"]], 1, 3) != "LPJ" && verbose)
    warning(
      paste(
        "Header name",
        sQuote(name),
        "is probably invalid for use in LPJmL."
      )
    )
  header[["header"]] <- numeric(0)
  # Check that valid values have been provided for all parameters included
  # in header version 1
  header_elements <- c(
    "version",
    "order",
    "firstyear",
    "nyear",
    "firstcell",
    "ncell",
    "nbands"
  )
  for (check in header_elements) {
    if (is.numeric(get(check)) &&
        length(get(check)) == 1 && get(check) == as.integer(get(check))) {
      header[["header"]] <- c(header[["header"]], as.integer(get(check)))
      names(header[["header"]])[length(header[["header"]])] <- check
    } else {
      stop(paste(sQuote(check), "must be an integer of length 1"))
    }
  }
  if (version >= 2) {
    # Check that valid values have been provided for additional parameters in
    # header version 2
    for (check in c("cellsize_lon", "scalar")) {
      if (is.numeric(get(check)) && length(get(check)) == 1) {
        header[["header"]] <- c(header[["header"]], as.double(get(check)))
        names(header[["header"]])[length(header[["header"]])] <- check
      } else {
        stop(paste(sQuote(check), "must be a float of length 1"))
      }
    }
    # Check that valid values have been provided for additional parameters in
    # header version 3
    if (version >= 3) {
      if (is.numeric(cellsize_lat) && length(cellsize_lat) == 1) {
        header[["header"]] <- c(
          header[["header"]],
          cellsize_lat = as.double(cellsize_lat)
        )
      } else {
        stop("cellsize_lat must be a float of length 1")
      }
      if (length(datatype) == 1) {
        if (!is.null(
          get_datatype(c(header[["header"]], datatype = datatype))
        )) {
          header[["header"]] <- c(
            header[["header"]],
            datatype = as.integer(datatype)
          )
          if (verbose) {
            print(
              paste0(
                "Setting datatype to ",
                ifelse(get_datatype(header)$signed, "", "unsigned "),
                typeof(get_datatype(header)$type),
                " with size ",
                get_datatype(header)$size, "."
              )
            )
          }
        } else {
          stop(paste0("Unknown datatype", datatype, "."))
        }
      } else {
        stop("datatype must be integer of length 1.")
      }
      # Check that valid values have been provided for additional parameters in
      # header version 4
      if (version >= 4) {
        if (is.numeric(nstep) && length(nstep) == 1 &&
          nstep == as.integer(nstep)
        ) {
          header[["header"]] <- c(
            header[["header"]],
            nstep = as.integer(nstep)
          )
        } else {
          stop("nstep must be an integer of length 1")
        }
      } else {
        # Add defaults
        warntext <- "Type 3 header:"
        if (missing(nstep) || !is.numeric(nstep) || length(nstep) != 1 ||
          nstep != as.integer(nstep)
        ) {
          header[["header"]] <- c(
            header[["header"]],
            nstep = 1
          )
          warntext <- paste(
            warntext,
            "Setting default value 1 for nstep."
          )
        } else {
          header[["header"]] <- c(
            header[["header"]],
            nstep = as.integer(nstep)
          )
          if (nstep != 1) {
            warntext <- paste0(
              warntext,
              " Setting non-default nstep", nstep,
              ". This information is not kept when saving header to file."
            )
          }
        }
      }
    } else {
      # Add defaults
      warntext <- "Type 2 header:"
      if (missing(cellsize_lat) || !is.numeric(cellsize_lat) ||
        length(cellsize_lat) != 1
      ) {
        header[["header"]] <- c(
          header[["header"]],
          cellsize_lat = as.double(header[["header"]]["cellsize_lon"])
        )
        warntext <- paste(
          warntext,
          "Setting default value",
          as.double(header[["header"]]["cellsize_lon"]),
          "for cellsize_lat."
        )
      } else {
        header[["header"]] <- c(
          header[["header"]],
          cellsize_lat = as.double(cellsize_lat)
        )
        if (cellsize_lat != header[["header"]]["cellsize_lon"]) {
          warntext <- paste0(
            warntext,
            " Setting non-default cellsize_lat ", cellsize_lat,
            ". This information is not kept when saving header to file."
          )
        } else {
          warntext <- paste(
            warntext,
            "Setting default value",
            as.double(header[["header"]]["cellsize_lat"]),
            "for cellsize_lat."
          )
        }
      }
      if (missing(datatype) || length(datatype) != 1 || is.null(
        get_datatype(c(header[["header"]], datatype = datatype))
      )) {
        header[["header"]] <- c(
          header[["header"]],
          datatype = 1
        )
        warntext <- paste(warntext, "Setting default value 1 for datatype.")
      } else {
        header[["header"]] <- c(
          header[["header"]],
          datatype = as.integer(datatype)
        )
        if (datatype != 1) {
          warntext <- paste0(
            warntext,
            " Setting datatype to non-default ", as.integer(datatype), " (",
            ifelse(get_datatype(header)$signed, "", "unsigned "),
            typeof(get_datatype(header)$type),
            " with size ",
            get_datatype(header)$size,
            "). This information is not kept when saving header to file."
          )
        } else {
          warntext <- paste(warntext, "Setting default value 1 for datatype.")
        }
      }
      if (missing(nstep) || !is.numeric(nstep) || length(nstep) != 1 ||
        nstep != as.integer(nstep)
      ) {
        header[["header"]] <- c(
          header[["header"]],
          nstep = 1
        )
        warntext <- paste(
          warntext,
          "Setting default value 1 for nstep."
        )
      } else {
        header[["header"]] <- c(
          header[["header"]],
          nstep = as.integer(nstep)
        )
        if (nstep != 1) {
          warntext <- paste0(
            warntext,
            " Setting non-default nstep", nstep,
            ". This information is not kept when saving header to file."
          )
        }
      }
      if (warntext != "Type 2 header:" && verbose)
        warning(warntext)
    }
  } else {
    # Add defaults
    warntext <- "Type 1 header:"
    if (missing(cellsize_lon) || !is.numeric(cellsize_lon) ||
      length(cellsize_lon) != 1
    ) {
      header[["header"]] <- c(
        header[["header"]],
        cellsize_lon = 0.5
      )
      warntext <- paste(
        warntext,
        "Setting default value 0.5 for cellsize_lon."
      )
    } else {
      header[["header"]] <- c(
        header[["header"]],
        cellsize_lon = as.double(cellsize_lon)
      )
      if (cellsize_lon != 0.5) {
        warntext <- paste0(
          warntext,
          " Setting non-default cellsize_lon ", cellsize_lon,
          ". This information is not kept when saving header to file."
        )
      }
      if (missing(cellsize_lat)) {
        cellsize_lat <- cellsize_lon
      }
    }
    if (missing(scalar) || !is.numeric(scalar) || length(scalar) != 1) {
      header[["header"]] <- c(
        header[["header"]],
        scalar = 1
      )
      warntext <- paste(warntext, "Setting default value 1 for scalar.")
    } else {
      header[["header"]] <- c(
        header[["header"]],
        scalar = as.double(scalar)
      )
      if (scalar != 1) {
        warntext <- paste0(
          warntext,
          " Setting non-default scalar ", scalar,
          ". This information is not kept when saving header to file."
        )
      }
    }
    if (missing(cellsize_lat) || !is.numeric(cellsize_lat) ||
      length(cellsize_lat) != 1
    ) {
      header[["header"]] <- c(
        header[["header"]],
        cellsize_lat = 0.5
      )
      warntext <- paste(
        warntext,
        "Setting default value 0.5 for cellsize_lat."
      )
    } else {
      header[["header"]] <- c(
        header[["header"]],
        cellsize_lat = as.double(cellsize_lat)
      )
      if (any(cellsize_lat != c(header[["header"]]["cellsize_lon"], 0.5))) {
        warntext <- paste0(
          warntext,
          " Setting non-default cellsize_lat ", cellsize_lat,
          ". This information is not kept when saving header to file."
        )
      } else {
        warntext <- paste(
          warntext,
          "Setting default value 0.5 for cellsize_lat."
        )
      }
    }
    if (missing(datatype) || length(datatype) != 1 || is.null(
      get_datatype(c(header[["header"]], datatype = datatype))
    )) {
      header[["header"]] <- c(
        header[["header"]],
        datatype = 1
      )
      warntext <- paste(warntext, "Setting default value 1 for datatype.")
    } else {
      header[["header"]] <- c(
        header[["header"]],
        datatype = as.integer(datatype)
      )
      if (datatype != 1) {
        warntext <- paste0(
          warntext,
          " Setting datatype to non-default ", as.integer(datatype), " (",
          ifelse(get_datatype(header)$signed, "", "unsigned "),
          typeof(get_datatype(header)$type),
          " with size ",
          get_datatype(header)$size,
          "). This information is not kept when saving header to file."
        )
      } else {
        warntext <- paste(warntext, "Setting default value 1 for datatype.")
      }
    }
    if (missing(nstep) || !is.numeric(nstep) || length(nstep) != 1 ||
      nstep != as.integer(nstep)
    ) {
      header[["header"]] <- c(
        header[["header"]],
        nstep = 1
      )
      warntext <- paste(
        warntext,
        "Setting default value 1 for nstep."
      )
    } else {
      header[["header"]] <- c(
        header[["header"]],
        nstep = as.integer(nstep)
      )
      if (nstep != 1) {
        warntext <- paste0(
          warntext,
          " Setting non-default nstep", nstep,
          ". This information is not kept when saving header to file."
        )
      }
    }
    if (warntext != "Type 1 header:" && verbose)
      warning(warntext)
  }
  if (endian %in% c("big", "little")) {
    header[["endian"]] <- endian
  } else {
    stop(
      paste0(
        "Endian must be either 'big' or 'little'. Provided value: ",
        sQuote(endian), "."
      )
    )
  }
  header
}
