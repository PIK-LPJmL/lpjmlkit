# utility function to determine time resolution of a netcdf file
get_timestep <- function(file_nc) {
  time_dim <- get_time_variable(file_nc)
  tunit <- file_nc$dim[[time_dim]]$units
  if (is.null(tunit)) {
    return(NULL)
  }
  time_values <- file_nc$dim[[time_dim]]$vals
  # default
  timestep <- 1

  if (grepl("[[:digit:]]", tunit)) {
    time_cf <- CFtime::CFtime(definition = tunit,
      calendar = file_nc$dim[[time_dim]]$calendar,
      offsets = time_values
    )
    rows <- length(time_cf)
    # split timestamps into matrix (taking care of negative years)
    timestamps <- CFtime::as_timestamp(time_cf)

    dates <- matrix(
      as.integer(
        unlist(
          regmatches(timestamps,
            regexec("([-]?[[:digit:]]+)-([[:digit:]]+)-([[:digit:]]+)",
                    timestamps)
          ) |>
            lapply(function(x) as.integer(x[-1]))
        )
      ), ncol = 3, byrow = TRUE,
      dimnames = list(seq(1, rows), c("year", "month", "day"))
    )

    ybands <- table(dates[, "year"])
    if (all(unique(ybands) %in% c(365, 366))) {
      time_res <- "daily"
      nyear <- length(unique(dates[, "year"]))
      nstep <- 365
    } else if (all(unique(ybands) %in% c(12))) {
      time_res <- "monthly"
      nyear <- length(unique(dates[, "year"]))
      nstep <- 12
    } else if (all(unique(ybands) %in% c(1))) {
      time_res <- "annual"
      nyear <- length(unique(dates[, "year"]))
      nstep <- 1
    } else {
      stop("Automatic detection of time resolution failed.")
    }
    timesteps <- unique(dates[, "year"])
    if (length(timesteps) == 1) {
      timestep <- 1
    } else if (length(unique(diff(timesteps))) == 1) {
      timestep <- unique(diff(timesteps))
    } else {
      stop("Timesteps between years are not regular.")
    }
    first_year <- dates[1, "year"]

  } else {
    if (grepl("year", tunit, ignore.case = TRUE)) {
      time_res <- "annual"
      first_year <- time_values[1]
      nyear <- length(time_values)
      timestep <- time_values[2] - time_values[1]
      nstep <- 1
    } else {
      stop("Automatic detection of firstyear and time resolution failed.")
    }
  }

  return(
    list(
      time_res = time_res,
      nyear = nyear,
      nstep = nstep,
      first_year = as.numeric(first_year),
      timestep = timestep
    )
  )
}

# utility function to guess the main variable of a netcdf file
get_main_variable <- function(file_nc) {
  for (variable_name in names(file_nc$var)) {

    dim_names <- vapply(file_nc$var[[variable_name]]$dim,
                        function(x) x$name, FUN.VALUE = character(1))

    if (grepl("lon", paste(dim_names, collapse = " "), ignore.case = TRUE) &&
        grepl("lat", paste(dim_names, collapse = " "), ignore.case = TRUE)
    ) {
      return(variable_name)
    }
  }
  stop("None of the variables could certainly be identified as main variable.")
}

# utility function to guess the time variable of a netcdf file
get_time_variable <- function(file_nc) {
  variable_name <- get_main_variable(file_nc)

  dim_names <- vapply(file_nc$var[[variable_name]]$dim,
                      function(x) x$name, FUN.VALUE = character(1))

  pot_time_var <- grep("time", dim_names, ignore.case = TRUE, value = TRUE)
  if (length(pot_time_var) > 0) {
    return(pot_time_var)
  } else {
    return(NULL)
  }
}

#' Reads netcdf and returns it header info
#'
#' Reads an arbitrary netcdf + returns the header values to read as lpjml object
#'
#' @param filename netcdf file name
#' @param variable_name optional variable to be read, in case automatic
#' detection does not work as intended or several variables are stored within
#' the file
#' @param silent suppress warnings. Default: TRUE
#'
#' @return header data
#'
read_cdf_meta <- function( # nolint:cyclocomp_linter
  filename,
  variable_name,
  silent
) {
  file_type <- detect_io_type(filename = filename)

  file_nc <- ncdf4::nc_open(filename = filename)
  var_names <- names(file_nc$var)
  if (is.null(variable_name)) variable_name <- get_main_variable(
    file_nc = file_nc
  )

  ## spatial dimensions
  # get lon/lat information
  total_dim_names <- names(file_nc$dim)
  latdim <- grep("lat", total_dim_names, ignore.case = TRUE)
  londim <- grep("lon", total_dim_names, ignore.case = TRUE)
  lon <- file_nc$dim[[londim]]$vals
  lat <- file_nc$dim[[latdim]]$vals
  nlon <- length(lon)
  nlat <- length(lat)

  # now only the main variable dimensions
  dim_names <- vapply(file_nc$var[[variable_name]]$dim,
                      function(x) x$name, FUN.VALUE = character(1))

  if (length(grep("lat_bnds|lon_bnds", names(file_nc$var))) == 2) {
    lon_bnds <- ncdf4::ncvar_get(file_nc, "lon_bnds")
    lat_bnds <- ncdf4::ncvar_get(file_nc, "lat_bnds")
    if (length(dim(lon_bnds) == 1)) {
      resolution_lon <- abs(lon_bnds[2] - lon_bnds[1])
    } else if (length(dim(lon_bnds) == 2)) {
      resolution_lon <- abs(min(lon_bnds[2, ] - lon_bnds[1, ]))
    } else {
      stop("Unsupported lon bnds for variable: ",
           variable_name)
    }

    if (length(dim(lat_bnds) == 1)) {
      resolution_lat <- abs(lat_bnds[2] - lat_bnds[1])
    } else if (length(dim(lat_bnds) == 2)) {
      resolution_lat <- abs(min(lat_bnds[2, ] - lat_bnds[1, ]))
    } else {
      stop("Unsupported lat bnds for variable: ",
           variable_name)
    }
  } else {
    # take the median difference between the cells as the resolution
    if (nlon > 1) {
      resolution_lon <- abs(stats::median(diff(lon)))
    }
    if (nlat > 1) {
      resolution_lat <- abs(stats::median(diff(lat)))
    }
    # if we have only a subset, we need to make some assumptions for the resolution
    if (nlat == 1 && nlon == 1) {
      resolution_lat <- 0.5
      resolution_lon <- 0.5
      if (!silent) warning(
        "Only one longitude and latitude value found. ",
        "Assuming resolution of 0.5 degrees."
      )
    } else if (nlat == 1 && nlon > 1) {
      resolution_lat <- resolution_lon
      if (!silent) warning(
        "Only one latitude value found. ",
        "Taking longitude resolution as latitude resolution."
      )
    } else if (nlat > 1 && nlon == 1) {
      resolution_lon <- resolution_lat
      if (!silent) warning(
        "Only one longitude value found. ",
        "Taking latitude resolution as longitude resolution."
      )
    }
  }
  suppressWarnings(global_attributes <- ncdf4::ncatt_get(file_nc, 0))
  suppressWarnings(var_attributes <- ncdf4::ncatt_get(file_nc, variable_name))

  meta_list <- list()

  ## time
  time_var <- get_time_variable(file_nc = file_nc)
  if (is.null(time_var)) {
    if (!variable_name %in% c("cellid"))
      if (!silent)
        warning(
          "None of the variables could certainly be identified as time variable."
        )
  } else if (!(time_var == "time")) {
    if (!silent)
      warning(
        "Non-standard time dimension in netcdf file identified as: ",
        time_var
      )
  }

  if (!is.null(time_var)) {
    time_info <- get_timestep(file_nc = file_nc)
    meta_list$firstyear <- time_info$first_year
    meta_list$timestep <- time_info$timestep
    meta_list$nyear <- time_info$nyear
    meta_list$nstep <- time_info$nstep
    meta_list$lastyear <- meta_list$firstyear +
      (meta_list$nyear - 1) * meta_list$timestep

  } else {

    if (!variable_name %in% c("cellid", "grid", "LPJGRID")) {
      if (!silent)
        warning(
          "Time information could not be extracted from the netcdf file. ",
          "Please define them as parameters to read_io."
        )
    }
  }

  # get the first of the variable names that has not been identified as the
  # main variable or one of time, lat or lon band names
  lat_name <- grep("lat", dim_names, value = TRUE, ignore.case = TRUE)
  lon_name <- grep("lon", dim_names, value = TRUE, ignore.case = TRUE)
  time_name <- grep("time", dim_names, value = TRUE, ignore.case = TRUE)

  # potential bounds variables, these are not containing the main var
  bnds_names <- var_names[
    grepl("lat_bnds|lon_bnds|time_bnds",
          var_names, ignore.case = TRUE)
  ]

  dimnames_reduced <- setdiff(dim_names, c(lat_name, lon_name, time_name))
  varnames_reduced <- setdiff(var_names, c(variable_name, bnds_names))

  # If there are bands in the variable besides lat lon and time, get them now.
  # In LPJmL the band names are also written into the varnames list,
  # and can be retrieved from there, however for external data (e.g. ERA5) this
  # is not always the case, thus here we need to discriminate
  if (length(dimnames_reduced) > 0) {
    bands_dim_name <- dimnames_reduced[1]
    if (!silent)
      warning(
        "Automatic band dimensions detection. Picking: ",
        bands_dim_name
      )
    if (length(dimnames_reduced) > 1) {
      if (!silent)
        warning(
          "Several potential band dimensions detected. Picking: ",
          bands_dim_name
        )
    }
    if (length(varnames_reduced) > 0) {
      bands_var_name <- varnames_reduced[1]
      if (!silent)
        warning(
          "Automatic band name dimensions detection. Picking: ",
          bands_var_name
        )
      if (length(varnames_reduced) > 1) {
        if (!silent)
          warning(
            "Several potential band dimensions detected. Picking: ",
            bands_var_name
          )
      }
      bands <- ncdf4::ncvar_get(nc = file_nc, varid = bands_var_name)
    } else { # no varname available - taking indices instead
      ind <- which(dim_names == bands_dim_name)
      bands <- seq(1, file_nc$var[variable_name]$varsize[ind], 1)
      warning("Banded variable, but names of bands not supplied as bands.")
    }

    if (length(dim(bands)) == 2) {
      bands <- colMeans(bands)
      if (!silent)
        warning(
          "Band ", bands_var_name,
          " has 2 dimensions, assuming these are bounds, averaging."
        )
    }
    if (length(dim(bands)) > 2)
      if (!silent)
        warning("Band dimensions larger than 2. Exiting.")
    nbands <- length(bands)
  } else {
    bands_var_name <- ""
    bands <- NULL
    nbands <- 1
  }

  meta_list$sim_name <- global_attributes$title
  meta_list$source <- global_attributes$source
  meta_list$history <- global_attributes$history
  meta_list$name <- tolower(variable_name)
  meta_list$variable <- variable_name
  meta_list$descr <- var_attributes$long_name
  meta_list$unit <- var_attributes$units
  meta_list$nbands <- nbands
  meta_list$band_names <- bands

  # Changed according to team decision
  meta_list$ncell <- NA
  meta_list$firstcell <- 0

  meta_list$cellsize_lon <- resolution_lon
  meta_list$cellsize_lat <- resolution_lat

  meta_list$format <- file_type
  meta_list$filename <- basename(filename)
  meta_list$subset <- FALSE
  # get precision of the variable
  meta_list$datatype <- file_nc$var[[variable_name]]$prec
  if (!meta_list$datatype %in% c("byte", "short", "int", "float", "double"))
    stop(paste0("Unknown datatype: ", meta_list$datatype))

  meta_list$scalar <- 1
  meta_list$order <- "cellseq"
  meta_list$bigendian <- FALSE

  ncdf4::nc_close(file_nc)

  meta_list
}

#' Reads netcdf and returns it as array
#'
#' Reads an arbitrary netcdf and returns the values at the lon/lat locations..
#'
#' @param filename netcdf file name
#' @param nc_header header data, read in from either meta file or data in
#'        netcdf file
#' @param subset list object defining which subset of the data to be read
#' @param silent suppress warnings. Default: TRUE
#'
#' @return array with netcdf's data, dim=c(nlon,nlat,bands,steps (months/days),years)
#'
read_cdf <- function( # nolint:cyclocomp_linter
  filename,
  nc_header,
  subset = list(),
  silent = TRUE
) {
  variable_name <- nc_header$variable
  file_nc <- ncdf4::nc_open(filename = filename)

  # Determine all years in the file
  years_only <- seq(
    from       = default(nc_header$firstyear, 1901),
    by = default(nc_header$timestep, 1),
    length.out = default(nc_header$nyear, 1)
  )
  # Determine all sub-yearly steps in the file
  years_nstep <- rep(
    years_only,
    each = default(nc_header$nstep, 1)
  )

  if (!is.null(names(subset))) {
    if (any(c("lon", "lat", "coords", "coordinates") %in% names(subset))) {
      stop("Subsetting by lon/lat/coordinates not supported for reading NetCDF files.")
    }

    if (!all(names(subset) %in% c("year", "band"))) {
      stop(
        "Subset must be a list with elements of the array dimensions ",
        "'year', 'band'."
      )
    }
  }

  # get full list of timestamps including subsetted years and leapdays
  time_dim <- get_time_variable(file_nc)

  if (is.null(time_dim)) {
    relevant_timesteps <- c(1)
    years <- 0
  } else {
    tunit <- file_nc$dim[[time_dim]]$units
    if (tolower(tunit) %in% c("years", "year")) tunit <- "years since 0-01-01"
    calendar <- file_nc$dim[[time_dim]]$calendar
    if (is.null(calendar)) calendar <- "noleap"
    time_cf <- CFtime::CFtime(definition = tunit,
      calendar = calendar,
      offsets = ncdf4::ncvar_get(nc = file_nc,
                                 varid = time_dim)
    )
    timestamps_full <- CFtime::as_timestamp(time_cf)
    leapday <- grepl("-02-29", timestamps_full)
    rows <- length(timestamps_full)

    # warning
    if (any(grepl("-02-29", timestamps_full)))
      if (!silent)
        warning("Netcdf contains leapdays, these will be deleted.")

    # split timestamps into matrix (taking care of negative years)
    dates <- matrix(
      as.integer(
        unlist(
          regmatches(timestamps_full,
            regexec("([-]?[[:digit:]]+)-([[:digit:]]+)-([[:digit:]]+)",
                    timestamps_full)
          ) |>
            lapply(function(x) as.integer(x[-1]))
        )
      ), ncol = 3, byrow = TRUE,
      dimnames = list(seq(1, rows), c("year", "month", "day"))
    )

    # Years to read
    if (!is.null(names(subset)) && "year" %in% names(subset)) {
      if (is.numeric(subset[["year"]])) {
        # subset year indices outside file range are checked in check_year_subset
        subset[["year"]] <- years_only[subset[["year"]]]
      }
      timesteps <- which(years_nstep %in% subset[["year"]])
      years <- years_nstep[timesteps]
      relevant_year <- dates[, "year"] %in% subset[["year"]]
    } else {
      timesteps <- seq_along(years_nstep)
      years <- years_nstep
      relevant_year <- rep(TRUE, length(rows))
    }

    relevant_timesteps <- which(relevant_year & !leapday)
  }

  # check if subset[["band"]] is valid
  if (any(!subset[["band"]] %in% nc_header$band_names)) {
    stop(paste("Specified subset bands are unknown. They need to be one of:",
      paste(nc_header$band_names, collapse = ", ")
    )
    )
  }

  # bands to read
  if (!is.null(names(subset)) && "band" %in% names(subset)) {
    if (nc_header$nbands == 1) stop("Can't extract bands from single band input.")
    if (is.numeric(subset[["band"]])) {
      band_subset_ids <- subset[["band"]]
    } else {
      band_subset_ids <- match(subset[["band"]], nc_header$band_names)
    }
  } else {
    band_subset_ids <- seq_len(nc_header$nbands)
  }
  nbands <- length(band_subset_ids)

  # get lon/lat information
  total_dim_names <- names(file_nc$dim)
  latdim <- grep("lat", total_dim_names, ignore.case = TRUE)
  londim <- grep("lon", total_dim_names, ignore.case = TRUE)
  lon <- as.numeric(file_nc$dim[[londim]]$vals)
  lat <- as.numeric(file_nc$dim[[latdim]]$vals)
  nlon <- length(lon)
  nlat <- length(lat)

  # if longitude is in [0,360] format, convert to [-180,180]
  if (any(lon > 180)) {
    lon[lon > 180] <- lon[lon > 180] - 360
  }

  # check if it is a regular grid
  if (!silent) {
    reg_lat <- seq(-90 + nc_header$cellsize_lat / 2,
                   90 - nc_header$cellsize_lat / 2,
                   nc_header$cellsize_lat)
    reg_lon <- seq(-180 + nc_header$cellsize_lon / 2,
                   180 - nc_header$cellsize_lon / 2,
                   nc_header$cellsize_lon)
    if (any(!(lon %in% reg_lon)))
      warning("The grid contains irregular longitude values. ",
              "Attaching an lpjml grid is probably not possible.")
    if (any(!(lat %in% reg_lat)))
      warning("The grid contains irregular latitude values. ",
              "Attaching an lpjml grid is probably not possible.")
  }

  # now only the main variable dimensions
  dim_names <- vapply(file_nc$var[[variable_name]]$dim,
                      function(x) x$name, FUN.VALUE = character(1))

  # create empty data array to fill in next step
  outdata <- array(
    NA,
    dim = c(
      lon = nlon,
      lat = nlat,
      time = length(relevant_timesteps),
      band = nbands
    ),
    dimnames = list(
      lon = lon,
      lat = lat,
      time = create_time_names(
        nstep = default(nc_header$nstep, 1),
        years = unique(years)
      ),
      band = nc_header$band_names[band_subset_ids]
    )
  )

  time_idx <- 1
  for (i_time in relevant_timesteps) {
    band_idx <- 1
    for (i_band in band_subset_ids) {

      if (length(dim_names) == 4) { # lon, lat, time, band

        outdata[, , time_idx, band_idx] <-
          ncdf4::ncvar_get(
            nc = file_nc,
            varid = variable_name,
            count = c(-1, -1, 1, 1),
            start = c(1, 1, i_band, i_time),
            collapse_degen = FALSE # avoid dropping of the lat dim, if len=1
          )[, , 1, 1] # drop band and time

      } else if (length(dim_names) == 3) { # time, lon, lat or bands, lon, lat

        if ("time" %in% dim_names) { # lon, lat, time
          outdata[, , time_idx, band_idx] <-
            ncdf4::ncvar_get(
              nc = file_nc, varid = variable_name,
              count = c(-1, -1, 1),
              start = c(1, 1, i_time),
              collapse_degen = FALSE
            )[, , 1] # drop time
        } else { # lat, lon, band
          outdata[, , time_idx, band_idx] <-
            ncdf4::ncvar_get(
              nc = file_nc,
              varid = variable_name,
              count = c(-1, -1, 1),
              start = c(1, 1, i_band),
              collapse_degen = FALSE
            )[, , 1] # drop band
        }

      } else if (length(dim_names) == 2) {

        outdata[, , time_idx, band_idx] <-
          ncdf4::ncvar_get(
            nc = file_nc, varid = variable_name,
            count = c(-1, -1),
            start = c(1, 1),
            collapse_degen = FALSE # avoid dropping of the lat dim, if len=1
          ) # don't drop anything

      } else {

        stop("Less than 2 spatial dimensions in data array information found in netcdf file.")
      }
      band_idx <- band_idx + 1
    }
    time_idx <- time_idx + 1
  }

  # check if the data is in correct (LPJmL) lon and lat order: if not, transpose
  outdata <- transpose_lon_lat(outdata)

  ncdf4::nc_close(file_nc)

  return(outdata)
}
