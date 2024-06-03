# utility function to determine time resolution of a netcdf file
get_timestep <- function(file_nc){
  tunit <- file_nc$dim$time$units
  tvals <- file_nc$dim$time$vals
  if (grepl("years since", tunit, ignore.case = TRUE)){
    tres <- "annual"
    firstyr <- unlist(strsplit(unlist(strsplit(tunit, split = ' ', 
                                               fixed = TRUE))[3], split = '-', fixed = TRUE))[1]
  }else if (grepl("year", tunit, ignore.case = TRUE)){
    tres <- "annual"
    firstyr <- tvals[1]
  }else if (grepl("days since", tunit, fixed = TRUE)){
    ddiff <- tvals[2]-tvals[1]
    firstyr <- unlist(strsplit(unlist(strsplit(tunit, split = ' ', 
                                               fixed = TRUE))[3], split = '-', fixed = TRUE))[1]
    if (ddiff > 27 && ddiff < 32){
      tres <- "monthly"
    }else if (ddiff > 364 && ddiff < 367){
      tres <- "annual"
    }else if (ddiff == 1){
      tres <- "daily"
    }else{
      stop("Automatic detection of firstyear and time resolution failed.")
    }
  }else{
    stop("Automatic detection of firstyear and time resolution failed.")
  }
  return(list(tres = tres, firstyr = as.numeric(firstyr)))
}

# utility function to guess the main variable of a netcdf file
get_main_variable <- function(file_nc){
  for (var in names(file_nc$var)){
    ndims <- file_nc$var[[var]]$ndims
    dimNames <- c()
    for (d in 1:ndims){
      dimNames <- append(dimNames,file_nc$var[[var]]$dim[[d]]$name) 
    }
    #print(paste0(var,paste(dimNames,collapse = ",")))
    if (grepl("lon",paste(dimNames,collapse = " "),ignore.case = T) && grepl("lat",paste(dimNames,collapse = " "),ignore.case = T)){
      return(var)
    }
  }
  print(paste0("None of the variables could certainly be identified as main variable, guessing the last one: ",var))
  return(var)
}

#' Reads netcdf and returns it header info
#'
#' Reads an arbitrary netcdf + returns the header values to read as lpjml object
#'
#' @param nc_in_file netcdf file name
#' @param var optional variable to be read, in case automatic detection does 
#'        not work as intended or several variables are stored within the file
#' @param suppress_read_print whether to suppress the info "reading file XYZ" 
#'        (default TRUE)
#'
#' @return header data
#'
#' @examples
#' \dontrun{
#' }
#'
#' @export
read_cdf_header <- function( nc_in_file, 
                             var = NULL,
                             suppress_read_print = TRUE
) {
  file_type <- lpjmlkit::detect_io_type(filename = nc_in_file)
  #read_cdf_meta <- lpjmlkit::read_meta(filename = fpc_file_meta)

  file_nc <- ncdf4::nc_open(filename = nc_in_file)
  varnames <- names(file_nc$var)
  if (is.null(var)) var <- get_main_variable(file_nc = file_nc)
  
  # get the first of the variable names that has not been identified as the 
  # main variable
  if (length(varnames)>1){
    bands_var_name <- varnames[-match(var,varnames)][1]
    bands <- ncdf4::ncvar_get(nc = file_nc, varid = bands_var_name)
  }else{
    bands_var_name <- ""
    bands <- 1
  }

                           
  # get lon/lat information
  dimnames <- names(file_nc$dim)
  latdim <- which(grepl("latitude", dimnames, ignore.case = TRUE))
  londim <- which(grepl("longitude", dimnames, ignore.case = TRUE))
  lon <- file_nc$dim[[londim]]$vals
  lat <- file_nc$dim[[latdim]]$vals
  nlonin <- length(lon)
  nlatin <- length(lat)
  
  # take the median difference between the cells as the resolution
  spatial_difference_lon <- lon[2:length(lon)] - lon[1:(length(lon)-1)]
  resolution_lon <- median(spatial_difference_lon)
  spatial_difference_lat <- lat[2:length(lat)] - lat[1:(length(lat)-1)]
  resolution_lat <- median(spatial_difference_lat)
  
  ndims <- file_nc$var[[var]]$ndims
  tunit <- file_nc$dim$time$units
  tvals <- file_nc$dim$time$vals
  timing <- get_timestep(file_nc = file_nc)
  tres <- timing$tres
  firstyr <- timing$firstyr
  if (tres == "annual"){
    nyears <- length(tvals)
    nsteps <- 1
  }else if (tres == "monthly"){
    nyears <- length(tvals)/12
    nsteps <- 12
  }else {
    stop("time resolution unknown. (e.g. daily not implemented yet)")
  }
  
  lastyr <- firstyr + nyears - 1
  
  global_attributes <- ncdf4::ncatt_get(file_nc, 0)
  var_attributes <- ncdf4::ncatt_get(file_nc, var)
  
  empty_meta <- list()
  empty_meta$sim_name <- global_attributes$title
  empty_meta$source <- global_attributes$source
  empty_meta$history <- global_attributes$history
  empty_meta$name <- tolower(var)
  empty_meta$variable <- var
  empty_meta$descr <- var_attributes$longname
  empty_meta$unit <- var_attributes$units
  empty_meta$nbands <- length(bands)
  empty_meta$band_names <- bands
  empty_meta$nyear <- nyears
  empty_meta$firstyear <- firstyr
  empty_meta$lastyear <- lastyr
  empty_meta$ncell <- nlonin*nlatin
  empty_meta$firstcell <- 0
  empty_meta$cellsize_lon <- resolution_lon
  empty_meta$cellsize_lat <- resolution_lat
  empty_meta$scalar <- 1
  empty_meta$format <- file_type
  empty_meta$filename <- basename(nc_in_file)
  empty_meta$nstep <- nsteps
  
  empty_meta$timestep <- 1
  empty_meta$datatype <- "float"
  empty_meta$order <- "cellseq"
  empty_meta$bigendian <- FALSE
  empty_meta$subset <- FALSE
  ncdf4::nc_close(file_nc)
  
  header_data <- lpjmlkit::LPJmLMetaData$new(x = empty_meta)
  return(header_data)
}

#' Reads netcdf and returns it as array
#'
#' Reads an arbitrary netcdf and returns the values at the lon/lat locations..
#' Todos:
#' - annual data which is written out only every X years (what about monthly?):
#'   (/p/tmp/sibylls/Methane/output_holocene/soilc.nc)
#' - daily timesteps
#' - check calendar
#' - check lpj code for nc output creation
#' - how to distribute nlat and nlon from nc header to read function? 
#'   or read in again?
#'
#' @param nc_in_file netcdf file name
#' @param nc_header header data, read in from either meta file or data in 
#'        netcdf file
#' @param get_year_start first year to be read 
#'        (if not specified will default to first record year)
#' @param get_year_stop final year to be read 
#'        (if not specified will default to last record year)
#' @param suppress_read_print whether to suppress the info "reading file XYZ" 
#'        (default TRUE)
#'
#' @return array with netcdf's data, dim=c(nlon,nlat,[bands],[months],years)
#'
#' @examples
#' \dontrun{
#' }
#'
#' @export
read_cdf <- function( nc_in_file, 
                      nc_header,
                      get_year_start = NULL,
                      get_year_stop = NULL,
                      suppress_read_print = TRUE
) {
  var <- nc_header$variable
  file_nc <- ncdf4::nc_open(filename = nc_in_file)
  if (!suppress_read_print) {
    print(paste0("Reading in: ", nc_in_file))
    print(paste0("Attempting to read variable: ",var,
                 ". If this is not correct,",
                 " please specify manually via argument var."))
  }

  if (is.null(get_year_start)) get_year_start <- nc_header$firstyear
  if (is.null(get_year_stop)) get_year_stop <- nc_header$lastyear
  ngetyears <- get_year_stop - get_year_start + 1
  # get lon/lat information - not part of header yet
  dimnames <- names(file_nc$dim)
  latdim <- which(grepl("latitude", dimnames, ignore.case = TRUE))
  londim <- which(grepl("longitude", dimnames, ignore.case = TRUE))
  lon <- file_nc$dim[[londim]]$vals
  lat <- file_nc$dim[[latdim]]$vals
  nlonin <- length(lon)
  nlatin <- length(lat)
  
  print(paste(nlonin, nlatin, nc_header$nbands, nc_header$nstep, ngetyears,sep = ","))
  outdata <- array(0, dim = c(nlonin, nlatin, nc_header$nbands, nc_header$nstep, ngetyears))

  # get spatial extent and resolution
  # this will give a warning if the NetCDF has more than one data field, 
  # e.g. crop bands or time axis
  file_raster <- raster::raster(nc_in_file)
  for (year in get_year_start:get_year_stop){
    for (step in 1:nc_header$nstep){
      if (nc_header$nbands == 1){
        print(paste(year,nc_header$firstyear,step,nc_header$nstep,nc_header$nbands,((year - nc_header$firstyear)*nc_header$nstep + step),sep=","))
        data <- ncdf4::ncvar_get(nc = file_nc, varid = var, count=c(-1,-1,1),
                              start=c(1,1,((year - nc_header$firstyear)*nc_header$nstep + step)))
        # check whether data needs to be flipped vertically
        # lat axes in nc files can be from north - south or from south - north
        if((raster::yFromRow(file_raster, 1) < raster::yFromRow(file_raster, 2)) 
                 != (file_nc$dim$lat$vals[1] < file_nc$dim$lat$vals[2])) {
          # flip vertically
          data <- data[, nlatin:1]
        }
        outdata[,,1,step,(year - get_year_start + 1)] <- data

      }else{ #nbands>1
        print(paste(year,nc_header$firstyear,step,nc_header$nstep,nc_header$nbands,((year - nc_header$firstyear)*nc_header$nstep + step),sep=","))
        data <- ncdf4::ncvar_get(nc = file_nc, varid = var, count=c(-1,-1,-1,1),
                          start=c(1,1,1,((year - nc_header$firstyear)*nc_header$nstep + step)))
        # check whether data needs to be flipped vertically
        # lat axes in nc files can be from north - south or from south - north
        if((raster::yFromRow(file_raster, 1) < raster::yFromRow(file_raster, 2)) 
                 != (file_nc$dim$lat$vals[1] < file_nc$dim$lat$vals[2])) {
          # flip vertically
          data <- data[, nlatin:1,]
        }
        outdata[,,,step,(year - get_year_start + 1)] <- data

      }# end if nbands == 1
    }# end for step in 1:nc_header$nstep
  }# end for year in ...
  ncdf4::nc_close(file_nc)
  dim(outdata) <- c(lon = nlonin, lat = nlatin, band = nc_header$nbands,
                    step = nc_header$nstep, year = ngetyears)
  dimnames(outdata) <- list(lon = lon, lat = lat, band = 1:nc_header$nbands,
                            step = 1:nc_header$nstep, year = get_year_start:get_year_stop)

  return(outdata)
}
