#' Reads daily netcdf and returns it as LPJ-array
#'
#' Reads a daily netcdf and returns the values at the LPJ-grid locations
#' the netcdf resolution needs to correspond to the LPJmL grid resolution
#' (i.e. the LPJ cells need to have the same center coordinates than the netcdf)
#'
#' @param ncInFile netcdf file name
#' @param var netcdf variable name
#' @param years total years stored in netcdf file
#' @param lon array with longitude coordinates of the centers of the LPJmL grid
#' @param lat array with latitude coordinates of the centers of the LPJmL grid
#'
#' @return lpjml array with data from netcdf, dim=c(ncells,days,years)
#'
#' @examples
#' \dontrun{
#' netcdfDaily2lpjarray(ncInFile=ddischarge_1901-2005.nc4,var="dis",years=105,lon=lon,lat=lat)
#' }
#'
#' @export
netcdfDaily2lpjarray <- function(ncInFile,var,years,lon,lat){
  require(raster)
  require(ncdf4)
  # NetCDF file
  file_nc <- nc_open(ncInFile)
  # get spatial extent and resolution
  # this will give a warning if the NetCDF has more than one data field, e.g. crop bands or time axis
  file_raster <- raster(ncInFile)
  cells=cellFromXY(file_raster, cbind(lon,lat))
  time=ncvar_get(file_nc,"time")
  days=length(time)/years
  if (!days==round(days)) print("Potential error in number of days - check leap days?")
  outdata=array(0,dim=c(length(lon),days,years))
  # loop over individual days and years in NetCDF
  for (year in 1:years){
    for (day in 1:days){
      # this assumes a 3-dimensional array: lon, lat, time
      data <- ncvar_get(file_nc, var, start=c(1,1, year*(year-1)+day), count=c(-1,-1,1))
      # check whether data needs to be flipped vertically
      # lat axes in NetCDF files can be from north to south or from south to north
      if((yFromRow(file_raster, 1)<yFromRow(file_raster, 2)) != (file_nc$dim$lat$vals[1]<file_nc$dim$lat$vals[2])) {
        # flip vertically
        data <- data[, file_nc$dim$lat$len:1,]
      }
      # dissolve lon and lat dimensions into one
      dim(data) <- c(file_nc$dim$lat$len*file_nc$dim$lon$len)
      # extract data for LPJmL coordinates using cellFromXY()
      # resulting array has dimensions c(ncell, nbands)
      outdata[,day,year] <- data[cells]
    }
  }
  nc_close(file_nc)
  return(outdata)
}
