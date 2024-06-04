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
    baseyr <- as.integer(unlist(strsplit(unlist(strsplit(tunit, split = ' ', 
                            fixed = TRUE))[3], split = '-', fixed = TRUE))[1])
    offset_yr <- floor(tvals[1]/365)
    firstyr <- baseyr + offset_yr
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
                             silent = TRUE
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
  latdim <- which(grepl("lat", dimnames, ignore.case = TRUE))
  londim <- which(grepl("lon", dimnames, ignore.case = TRUE))
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
  #browser()
  if (tres == "annual"){
    nyears <- length(tvals)
    nsteps <- 1
  }else if (tres == "monthly"){
    nyears <- length(tvals)/12
    nsteps <- 12
  }else if (tres == "daily"){
    nyears <- length(tvals)/365
    nsteps <- 365
    if (!all.equal(round(nyears,0), nyears)) 
       stop("Number of time records not a multiple of 365 - pls check calendar")
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
  empty_meta$datatype <- "float"
  empty_meta$order <- ""
  empty_meta$subset <- FALSE
  
  empty_meta$timestep <- 1 # todo: deal with this type
  empty_meta$bigendian <- FALSE # ?
  ncdf4::nc_close(file_nc)
  
  header_data <- lpjmlkit::LPJmLMetaData$new(x = empty_meta)
  header_data
}

#' Reads netcdf and returns it as array
#'
#' Reads an arbitrary netcdf and returns the values at the lon/lat locations..
#' Todos:
#' - annual data which is written out only every X years:
#'   (/p/tmp/sibylls/Methane/output_holocene/soilc.nc)
#' - daily timesteps
#' - check calendar
#' - check lpj code for nc output creation
#' - how to distribute nlat and nlon from nc header to read function? 
#'   or read in again?
#' - subsetting cells does not work yet
#'
#' @param nc_in_file netcdf file name
#' @param nc_header header data, read in from either meta file or data in 
#'        netcdf file
#' @param subset list object defining which subset of the data to be read
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
                      subset = list(),
                      silent = TRUE
) {
  var <- nc_header$variable
  file_nc <- ncdf4::nc_open(filename = nc_in_file)
  if (!suppress_read_print) {
    print(paste0("Reading in: ", nc_in_file))
    print(paste0("Attempting to read variable: ",var,
                 ". If this is not correct,",
                 " please specify manually via argument var."))
  }

  # Determine all years in the file
  years <- seq(
    from       = default(nc_header$firstyear, 1901),
    by         = default(nc_header$timestep, 1),
    length.out = default(nc_header$nyear, 1)
  )
  # Years to read
  if ("year" %in% names(subset)) {
    if (is.numeric(subset[["year"]])) {
      years <- years[subset[["year"]]]
    } else {
      years <- as.integer(subset[["year"]])
    }
  }
  ngetyears <- years[length(years)] - years[1] + 1
  
  # bands to read
  if ("band" %in% names(subset)) {
    if (nc_header$nbands == 1) stop("Can't extract bands from single band input.")
    if (is.numeric(subset[["band"]])) {
      band_subset_ids <- subset[["band"]]
    } else {
      band_subset_ids <- match(subset[["band"]], nc_header$band_names)
    }
  }
  nbands <- length(band_subset_ids)
  
  # get lon/lat information - not part of header yet
  dimnames <- names(file_nc$dim)
  latdim <- which(grepl("lat", dimnames, ignore.case = TRUE)) # todo: check if this variable is not the data variable
  londim <- which(grepl("lon", dimnames, ignore.case = TRUE))
  lon <- file_nc$dim[[londim]]$vals
  lat <- file_nc$dim[[latdim]]$vals
  nlonin <- length(lon)
  nlatin <- length(lat)
  
  outdata <- array(0, dim = c(nlonin, nlatin, nbands, nc_header$nstep, ngetyears))

  for (year in years){
    for (step in 1:nc_header$nstep){
      if (nc_header$nbands == 1){
        #print(paste(year,nc_header$firstyear,step,nc_header$nstep,nc_header$nbands,((year - nc_header$firstyear)*nc_header$nstep + step),sep=","))
        data <- ncdf4::ncvar_get(nc = file_nc, varid = var, count=c(-1,-1,1),
                              start=c(1,1,((year - nc_header$firstyear)*nc_header$nstep + step)))
        outdata[,,1,step,(year - years[1] + 1)] <- data

      }else{ #nbands>1
        data <- ncdf4::ncvar_get(nc = file_nc, varid = var, count=c(-1,-1,-1,1),
                          start=c(1,1,1,((year - nc_header$firstyear)*nc_header$nstep + step)))
        outdata[,,,step,(year - years[1] + 1)] <- data[,,band_subset_ids]

      }# end if nbands == 1
    }# end for step in 1:nc_header$nstep
  }# end for year in ...
  ncdf4::nc_close(file_nc)
  dim(outdata) <- c(lon = nlonin, lat = nlatin, band = nbands,
                    step = nc_header$nstep, year = ngetyears)
  dimnames(outdata) <- list(lon = lon, lat = lat, band = nc_header$band_names[band_subset_ids],
                            step = 1:nc_header$nstep, year = years)

  return(outdata)
}
