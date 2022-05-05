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
#' @param order Order of data items in file (default in input file: 1;
#' in output file: 4). In other words, this refers to how the data are sorted
#' along the vector in the output file. See LPJmL code for supported values,
#' i.e. header.h: CELLYEAR 1, YEARCELL 2, CELLINDEX 3, CELLSEQ 4.
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
  file_name    = "file_name.bin",
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

  # Default band order in returned data object
  default_band_order <- c("cell", "time", "band")

  # ------------------------------------ #
  # Get infos about the type and structure of the file
  if (file_type == "raw") {
    # raw file type (binary file without a header)

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
    # clm file type (binary file with a LPJmL header)

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
    # meta file type (binary file with associated meta-data json file)

    # Read meta data
    meta_data <- read_meta(file_name)

    # Derive header from meta data
    file_header <- meta_data$as_header()

    # Check if user has tried overwriting any header attributes which we do not
    # allow for meta files.
    header_args <- as.list(match.call()) %>%
      # get all arguments except file_name, ...
      .[!names(.) %in% c("file_name", "file_type", "subset_list")] %>%
      # exclude the function call itself (workaround)
      `[`(names(.) > 0) %>%
      # return the names of the arguments (not the values)
      names()

    if (length(header_args) > 0) {
      warning(
        paste0(
          "You cannot overwrite any of the following parameters for file_type ",
          sQuote(file_type),
          ": ",
          toString(sQuote(not_allowed))
        )
      )
    }

    # Offset at the start of the file before values begin
    # Confirm if JSON reflects if it is actually for a CLM file. Otherwise, need
    # to devise a test.
    if (is.null(meta_data$offset)) {
      start_offset <- 0
    } else {
      start_offset <- meta_data$offset
    }

    # Use band_names from meta file
    band_names <- meta_data$band_names

    # Get file_name from meta file.
    if (basename(meta_data$filename) == meta_data$filename) {
      # meta_data$filename is in same directory as file_name, can use path from
      # file_name.
      file_name <- file.path(dirname(file_name), meta_data$filename)
    } else {
      # meta_data$filename is in a different directory than file_name. Need to
      # parse path.
      # Save current working directory
      wd <- getwd()
      # Set working directory to path of file_name
      setwd(dirname(file_name))
      # Relative path can be parsed now.
      file_name <- normalizePath(meta_data$filename)
      # Reset working directory
      setwd(wd)
    }
  }

  # ------------------------------------ #
  # Check file size
  expected_filesize <- unname(
    get_header_item(file_header, "ncell") *
    get_header_item(file_header, "nbands") *
    get_header_item(file_header, "nstep") *
    get_header_item(file_header, "nyear") *
    get_datatype(file_header)$size + start_offset
  )

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

  # ------------------------------------ #
  # Check validity of subset_list and band_names
  check_subset(subset_list, file_header, band_names)

  # Years to read
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

  # ------------------------------------ #
  # Read data from binary file

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
    n_values <- unname(
      get_header_item(file_header, "ncell") *
      get_header_item(file_header, "nbands") *
      get_header_item(file_header, "nstep")
    )


    # Read data for one year from binary file
    year_data <- read_raw(
      file_connection,
      data_offset = data_offset,
      n_values = n_values,
      datatype = get_datatype(file_header),
      endian = get_header_item(file_header, "endian")
    )

    # Convert to array
    # Note: order of nbands and nstep for "cellyear" (order = 1) is currently
    # not defined in LPJmL.
    dim(year_data) <- switch(
      get_header_item(file_header, "order"),
      c(band = get_header_item(file_header, "nbands"), # order 1
        time = get_header_item(file_header, "nstep"),
        cell = get_header_item(file_header, "ncell")
      ),
      stop("Order yearcell not supported"),            # order 2
      stop("Order cellindex not supported"),           # order 3
      c(cell = get_header_item(file_header, "ncell"),  # order 4
        band = get_header_item(file_header, "nbands"),
        time = get_header_item(file_header, "nstep")
      )
    )

    # Assign dimension names to array
    if (is.null(band_names)) {  # use band index
      band_names <- seq_len(get_header_item(file_header, "nbands"))
    }

    dimnames(year_data) <- switch(
      get_header_item(file_header, "order"),
      list(                                                 # order 1
        band = band_names,
        time = NULL, # Assign dates later
        cell = seq(
          get_header_item(file_header, "firstcell"),
          length.out = get_header_item(file_header, "ncell")
        )
      ),
      stop("Order yearcell not supported"),                 # order 2
      stop("Order cellindex not supported"),                # order 3
      list(                                                 # order 4
        cell = seq(
          get_header_item(file_header, "firstcell"),
          length.out = get_header_item(file_header, "ncell")
        ),
        band = band_names,
        time = NULL # Assign dates later
      )
    )

    # Convert to default dimension order
    year_data <- aperm(year_data, perm = default_band_order) %>%
      # Apply any subsetting along bands or cells
      subset_array(
        subset_list[!names(subset_list) %in% c("day", "month", "year", "time")],
        drop = FALSE
      )

    # Concatenate years together
    if (yy == years[1]) {
      file_data <- year_data
    } else {
      file_data <- abind::abind(
        file_data,
        year_data,
        along = which(default_band_order == "time"),
        use.dnns = TRUE
      )
    }
  }
  # Close binary file connection
  close(file_connection)

  # ------------------------------------ #
  # Create time dimension names:
  time_dimnames <- create_time_names(
    nstep = get_header_item(file_header, "nstep"),
    years = years
  )

  # ------------------------------------ #
  # Assign final dimnames [cellnr, time, bands]
  dimnames(file_data)$time <- time_dimnames

  return(file_data)
  # lpjml_data <- LpjmlData$new(file_data, meta_data, subset_list)

  # return(lpjml_data)
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
      if (!all(subset_list[["year"]] %in% years)) {
        stop(
          paste(
            "Requested year(s)", setdiff(subset_list[["year"]], years),
            "not covered by file.",
            "\nCheck subset_list[[\"year\"]]."
          )
        )
      }
      rm(years)
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
      if (is.character(subset_list[["cell"]])) {
        cells <- seq(
          from = get_header_item(header, "firstcell"),
          length.out = get_header_item(header, "ncell")
        )
      } else if (is.numeric(subset_list[["cell"]])) {
        cells <- seq_len(get_header_item(header, "ncell"))
      } else {
        stop(
          paste(
            "subset_list[[\"cell\"]] must be numerical index vector or",
            "vector of cell names."
          )
        )
      }
      if (!all(subset_list[["cell"]] %in% cells)) {
        stop(
          paste(
            "Requested cell(s)", setdiff(subset_list[["cell"]], cells),
            "not covered by file.",
            "\nCheck subset_list[[\"cell\"]]."
          )
        )
      }
      rm(cells)
    }
    if (!is.null(band_names) &&
      length(band_names) != get_header_item(header, "nbands")
    ) {
      stop(
        paste(
          "Provided", sQuote(band_names), "do not match number of bands in",
          "file:", length(band_names), "!=", get_header_item(header, "nbands")
        )
      )
    }
    if (!is.null(subset_list[["band"]])) {
      if (is.character(subset_list[["band"]]) && is.null(band_names)) {
        stop(
          paste(
            "File has no associated band_names. Cannot do subset by name.",
            "\nProvide band indices instead of band names in",
            "subset_list[[\"cell\"]] or set band_names."
          )
        )
        if (!all(subset_list[["band"]] %in% band_names)) {
          stop(
            paste(
              "Requested band(s)", setdiff(subset_list[["band"]], band_names),
              "not covered by file.",
              "\nCheck subset_list[[\"cell\"]]."
            )
          )
        }
      } else if (is.numeric(subset_list[["band"]])) {
        bands <- seq(get_header_item(header, "nbands"))
        if (!all(subset_list[["band"]] %in% bands)) {
          stop(
            paste(
              "Requested band(s)", setdiff(subset_list[["band"]], bands),
              "not covered by file.",
              "\nCheck subset_list[[\"band\"]]."
            )
          )
        }
        rm(bands)
      }
    }
    if (
      any(!names(subset_list) %in% c("cell", "year", "month", "day", "band"))
    ) {
      warning(
        paste0(
          "Invalid 'subset_list' name(s)",
          toString(
            dQuote(
              setdiff(
                names(subset_list),
                c("cell", "year", "month", "day", "band")
              )
            )
          ),
          "will be ignored."
        )
      )
    }
}
