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
  empty_meta <- lpjmlkit::LPJmLMetaData
  
  varnames <- names(file_nc$var)
  file_nc <- ncdf4::nc_open(filename = nc_in_file)
  if (is.null(var)) var <- get_main_variable(file_nc = file_nc)
  
  # get the first of the variable names that has not been identified as the 
  # main variable
  bands_var_name <- varnames[-match(var,varnames)][1]
  bands <- ncdf4::ncvar_get(nc = file_nc, varid = bands_var_name)
                           
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
  
  global_attributes <- ncdf4::ncatt_get(file_nc, 0)
  var_attributes <- ncdf4::ncatt_get(file_nc, var)
  
  empty_meta$sim_name <- global_attributes$title
  empty_meta$source global_attributes$source
  empty_meta$history global_attributes$history
  empty_meta$variable <- tolower(var)
  empty_meta$descr <- var_attributes$longname
  empty_meta$unit <- var_attributes$units
  empty_meta$nbands <- length(bands)
  empty_meta$band_names <- bands
  empty_meta$nyear <- length(tvals)
  empty_meta$firstyear <- firstyr
  empty_meta$lastyear <- tvals[length(tvals)]
  empty_meta$ncell <- nlonin*nlatin
  empty_meta$firstcell <- 0
  empty_meta$cellsize_lon <- resolution_lon
  empty_meta$cellsize_lat <- resolution_lat
  empty_meta$scalar <- 1
  empty_meta$format <- file_type
  empty_meta$filename <- basename(nc_in_file)
  
  empty_meta$nstep <- 1
  empty_meta$timestep <- 1
  empty_meta$datatype <- "float"
  empty_meta$order <- "cellseq"
  empty_meta$bigendian <- FALSE
  empty_meta$subset <- FALSE
  
  return(empty_meta)
}

#' Reads netcdf and returns it as array
#'
#' Reads an arbitrary netcdf and returns the values at the lon/lat locations..
#' Todos:
#' - deal with input ncs in different resolutions 
#' - band name variables -> extract them
#' - annual data which is written out only every X years (what about monthly?):
#'   (/p/tmp/sibylls/Methane/output_holocene/soilc.nc)
#' - daily 
#' - check calendar
#' - check lpj code for nc output creation
#'
#' @param nc_in_file netcdf file name
#' @param get_year_start first year to be read 
#'        (if not specified will default to first record year)
#' @param get_year_stop final year to be read 
#'        (if not specified will default to last record year)
#' @param var optional variable to be read, in case automatic detection does 
#'        not work as intended or several variables are stored within the file
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
                             var = NULL,
                             get_year_start = NULL,
                             get_year_stop = NULL,
                             suppress_read_print = TRUE
) {

  file_nc <- ncdf4::nc_open(filename = nc_in_file)
  if (!suppress_read_print) {
    print(paste0("Reading in: ", nc_in_file))
    print(paste0("Attempting to read variable: ",var,
                 ". If this is not correct,",
                 " please specify manually via argument var."))
  }
  if (ndims == 3){
    data <- ncdf4::ncvar_get(nc = file_nc, varid = var,
                             start = c(1,1,1), count = c(1,1,-1))
    data_dim <- dim(data)
    nbands <- 1
    tsteps <- data_dim[1]
  }else if (ndims >= 4){
    data <- ncdf4::ncvar_get(nc = file_nc, varid = var,
                             start = c(1,1,1,1), count = c(1,1,-1,-1))
    data_dim <- dim(data)
    nbands <- data_dim[1]
    tsteps <- data_dim[2]
  }else{
    stop("Less than 3 dimensions for file. aborting.")
  }
  if (tres == "annual"){
    nyears <- tsteps
    nmonths <- 1
  }else if (tres == "monthly"){
    nyears <- tsteps/12
    nmonths <- 12
  }
  if (is.null(get_year_start)) get_year_start <- firstyr
  if (is.null(get_year_stop)) get_year_stop <- firstyr + nyears - 1
  ngetyears <- get_year_stop - get_year_start + 1


  outdata <- array(0, dim = c(nlonin,nlatin,nbands,nmonths,ngetyears))


  # get spatial extent and resolution
  # this will give a warning if the NetCDF has more than one data field, 
  # e.g. crop bands or time axis
  file_raster <- raster::raster(nc_in_file)
  cells <- raster::cellFromXY(file_raster, cbind(lon,lat))
  for (year in get_year_start:get_year_stop){
    for (month in 1:nmonths){
      if (nbands == 1){
        print(paste(year,firstyr,month,nmonths,nbands,((year - firstyr)*nmonths + month),sep=","))
        data <- ncdf4::ncvar_get(nc = file_nc, varid = var, count=c(-1,-1,1),
                              start=c(1,1,((year - firstyr)*nmonths + month)))
        # check whether data needs to be flipped vertically
        # lat axes in nc files can be from north - south or from south - north
        if((raster::yFromRow(file_raster, 1) < raster::yFromRow(file_raster, 2)) 
                 != (file_nc$dim$lat$vals[1] < file_nc$dim$lat$vals[2])) {
          # flip vertically
          data <- data[, nlatin:1]
        }
        outdata[,,1,month,(year - get_year_start + 1)] <- data

      }else{ #nbands>1
        data <- ncvar_get(nc = file_nc, varid = var, count=c(-1,-1,-1,1),
                          start=c(1,1,1,((year - firstyr)*nmonths + month)))
        # check whether data needs to be flipped vertically
        # lat axes in nc files can be from north - south or from south - north
        if((raster::yFromRow(file_raster, 1) < raster::yFromRow(file_raster, 2)) 
                 != (file_nc$dim$lat$vals[1] < file_nc$dim$lat$vals[2])) {
          # flip vertically
          data <- data[, nlatin:1,]
        }
        outdata[,,,month,(year - get_year_start + 1)] <- data

      }# end if nbands == 1
    }# end for month in 1:nmonths
  }# end for year in ...
  ncdf4::nc_close(file_nc)
  dim(outdata) <- c(lon = nlonin, lat = nlatin, band = nbands,
                    step = nmonths, year = ngetyears)
  dimnames(outdata) <- list(lon = lon, lat = lat, band = 1:nbands,
                            step = 1:nmonths, year = get_year_start:get_year_stop)

  return(outdata)
}
