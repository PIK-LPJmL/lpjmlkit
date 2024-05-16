# Internal function to read reservoir input file. Called by read_io().
read_io_reservoir <- function(filename, meta_data, subset, silent) {
  if (is.null(meta_data$nbands) || meta_data$nbands != length(band_names_reservoir)) {
    stop(
      "Invalid number of bands ", default(meta_data$nbands, 1),
      " in reservoir file ", filename, ". Expected ",
      length(band_names_reservoir), "."
    )
  }
  if (!is.null(meta_data$nyear) && meta_data$nyear != 1) {
    stop(
      "Invalid number of years ", meta_data$nyear,
      " in reservoir file ", filename, ". Expected 1."
    )
  }
  if (!is.null(meta_data$nstep) && meta_data$nstep != 1) {
    stop(
      "Invalid number of time steps per year ", meta_data$nstep,
      " in reservoir file ", filename, ". Expected 1."
    )
  }
  if (!silent) {
    message(
      "Note: Reservoir file detected, which uses a special file structure. ",
      "Time axis in returned LPJmLData object is not meaningful."
    )
  }
  if (!silent && !is.null(meta_data$band_names) &&
     !all(tolower(meta_data$band_names) == tolower(band_names_reservoir))
  ) {
    warning(
      "Supplied band_names attribute c(",
      toString(dQuote(meta_data$band_names, q = FALSE)),
      ") does not seem to match implemented reservoir file structure. ",
      "Implemented band order: c(",
      toString(dQuote(band_names_reservoir, q = FALSE)), ").",
      immediate. = TRUE, call. = FALSE
    )
  }
  # Determine all years in the file
  years <- seq(
    from       = default(meta_data$firstyear, 1901),
    by         = default(meta_data$timestep, 1),
    length.out = default(meta_data$nyear, 1)
  )
  # Subsetting by year does not work for restart files
  if ("year" %in% names(subset)) {
    warning(
      "Subsetting by year not possible for reservoir files",
      immediate. = TRUE, call. = FALSE
    )
  }

  # Open binary file connection
  file_connection <- file(filename, "rb")
  # Ensure that file connection is closed even if function is terminated with an
  # error
  on.exit(if (exists("file_connection")) close(file_connection)) # nolint:undesirable_function_linter.

  # Dimension order during reading. Note: Must be 3 dimensions in total, with
  # "time" being last dimension for code below to work.
  read_band_order <- c("cell", "band", "time")

  # Seek to start position of data
  seek(file_connection, default(meta_data$offset, 0))
  
  year_data <- array(
    dim = switch(
      default(meta_data$order, "cellyear"),
      cellyear = c(
        band = unname(default(meta_data$nbands, 1)),
        time = unname(default(meta_data$nstep, 1)),
        cell = unname(meta_data$ncell)
      ),
      yearcell = stop("Order yearcell not supported"),
      cellindex = stop("Order cellindex not supported"),
      cellseq = stop("Order cellindex not supported")
    )
  )
  
  # Assign dimension names to array.
  band_names <- default(
    meta_data$band_names,
    band_names_reservoir
  )
  cell_dimnames <- seq(
    default(meta_data$firstcell, 0),
    length.out = meta_data$ncell
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
    cellseq   = stop("Order cellindex not supported")                # order 4
  )
  # Read data
  for (cell in seq_len(meta_data$ncell)) {
    year_data[1, 1, cell] <- readBin(
      file_connection,
      n = 1,
      what = integer(),
      size = 4,
      endian = ifelse(meta_data$bigendian, "big", "little")
    )
    year_data[c(2, 3), 1, cell] <- readBin(
      file_connection,
      n = 2,
      what = double(),
      size = 4,
      endian = ifelse(meta_data$bigendian, "big", "little")
    )
    year_data[seq(4, 10), 1, cell] <- readBin(
      file_connection,
      n = 7,
      what = integer(),
      size = 4,
      endian = ifelse(meta_data$bigendian, "big", "little")
    )
  }
  # Close binary file connection
  close(file_connection)
  # Delete file_connection to prevent triggering on.exit expression
  rm(file_connection)

  # Convert to read_band_order and apply subsetting along bands or cells
  year_data <- aperm(year_data, perm = read_band_order)

  # Apply any subsetting along bands or cells
  index <- which(!names(subset) %in% c("day", "month", "year", "time"))
  year_data <- subset_array(
    year_data,
    subset[index],
    drop = FALSE,
    silent = silent
  )

  # Create and assign time dimension names
  time_dimnames <- create_time_names(
    nstep = default(meta_data$nstep, 1),
    years = years
  )
  dimnames(year_data)$time <- time_dimnames
  
  year_data
}
