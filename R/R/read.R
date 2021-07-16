Sys.setlocale('LC_ALL','C')
### ================ global variable definitions ============
ncells=67420
ndays=c(31,28,31,30,31,30,31,31,30,31,30,31)

### ================ read routines ===================

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

#' Reads monthly netcdf and returns it as LPJ-array
#'
#' Reads a monthly netcdf and returns the values at the LPJ-grid locations
#' the netcdf resolution needs to correspond to the LPJmL grid resolution
#' (i.e. the LPJ cells need to have the same center coordinates than the netcdf)
#'
#' @param ncInFile netcdf file name
#' @param var netcdf variable name
#' @param lon array with longitude coordinates of the centers of the LPJmL grid
#' @param lat array with latitude coordinates of the centers of the LPJmL grid
#'
#' @return lpjml array with data from netcdf, dim=c(ncells,months,years)
#'
#' @examples
#' \dontrun{
#' netcdfMonthly2lpjarray(ncInFile=mdischarge_1901-2005.nc4,var="dis",years=105,lon=lon,lat=lat)
#' }
#'
#' @export
netcdfMonthly2lpjarray <- function(ncInFile,var,lon,lat){
  require(raster)
  require(ncdf4)
  # NetCDF file
  file_nc <- nc_open(ncInFile)
  # get spatial extent and resolution
  # this will give a warning if the NetCDF has more than one data field, e.g. crop bands or time axis
  file_raster <- raster(ncInFile)
  cells=cellFromXY(file_raster, cbind(lon,lat))
  time=ncvar_get(file_nc,"time")
  years=length(time)/12
  outdata=array(0,dim=c(length(lon),12,years))
  # loop over individual days and years in NetCDF
  for (year in 1:years){
    for (month in 1:12){
      # this assumes a 3-dimensional array: lon, lat, time
      data <- ncvar_get(file_nc, var, start=c(1,1, (year-1)*12+month), count=c(-1,-1,1))
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
      outdata[,month,year] <- data[cells]
    }
  }
  nc_close(file_nc)
  return(outdata)
}

#' Reads a header from an LPJ input file
#'
#' tries to determine header version unless force_version is provided
#' return value: list with 3 components:                              ##
#' - header name, e.g. LPJGRID                                        ##
#' - header values (11 in total), if header version is <3, partially  ##
#'   filled with default values                                       ##
#' - endian of file (little or big)
#'
#' @param filename filename to read header from
#' @param force_version force clm version (default NULL - no)
#' @param endian endianness (default copied from local platform -- .Platform$endian)
#'
#' @return list-object with header content
#'
#' @examples
#' \dontrun{
#' readheader(filename, force_version=NULL)
#' }
#'
#' @export
readheader <- function(filename, force_version=NULL,endian=.Platform$endian) {
  if(!file.exists(filename)) {
    stop(paste("Error in readheader:", filename, "does not exist"))
  }
  zz <- file(filename, "rb")
  headername <- rawToChar(readBin(zz, raw(), n=30), multiple=TRUE)
  headername <- headername[1:(min(which(!grepl("[[:alpha:]_]", headername)))-1)]
  headername <- paste(headername, collapse="")
  if(substr(headername, 1,3) != "LPJ") {
    close(zz)
    stop(paste("Error in readheader: invalid header name", headername))
  }
  seek(zz, nchar(headername))
  endian <- .Platform$endian
  version <- readBin(zz, integer(), size=4, n=1, endian=endian)
  if(bitwAnd(version, 0xff)==0) {
    endian <- ifelse(endian=="little", "big", "little")
    seek(zz, nchar(headername))
    version <- readBin(zz, integer(), size=4, n=1, endian=endian)
  }
  if(!is.null(force_version)) {
    print(paste("Forcing header version to", force_version))
    version <- force_version
  }
  headerdata <- readBin(zz, integer(), size=4, n=6, endian=endian)
  names(headerdata) <- c("order", "firstyear", "nyear", "firstcell", "ncell", "nbands")
  if(version == 2) {
    headerdata <- c(headerdata, readBin(zz, double(), size=4, n=2, endian=endian))
    names(headerdata) <- c(names(headerdata[1:6]), "cellsize_lon", "scalar")
  }
  if(version == 3) {
    headerdata <- c(headerdata, readBin(zz, double(), size=4, n=3, endian=endian))
    headerdata <- c(headerdata, readBin(zz, integer(), size=4, n=1, endian=endian))
    names(headerdata) <- c(names(headerdata[1:(length(headerdata)-4)]), "cellsize_lon", "scalar", "cellsize_lat", "datatype")
  } else {
    if(length(headerdata)==6) {
      headerdata <- c(headerdata, cellsize_lon=0.5, scalar=1, cellsize_lat=0.5, datatype=1)
      warning("Type 1 header. Adding default values for cellsize, scalar and datatype which may not be correct in all cases")
    }
    if(length(headerdata)==8) {
      headerdata <- c(headerdata, cellsize_lat=as.double(headerdata["cellsize_lon"]), datatype=1)
      warning("Type 2 header. Adding default value for datatype which may not be correct in all cases")
    }
  }
  close(zz)
  return(list(name=headername, header=c(version=version, headerdata), endian=endian))
}

#' Automatically reads LPJmL input including header
#'
#' most header variables will be automatically read
#' for some combinations this won't work, thus
#' a manual override switch can be used to set size
#'
#' @param inFile name of file to read
#' @param getyearstart first year to be read (if not specified will default to startyear from header)
#' @param getyearstop final year to be read (if not specified will default to stopyear from header)
#' @param manu switch to allow for manual override of size (default False)
#' @param msize manual size, only applied if manu==TRUE (default 4)
#' @param mheadersize manual header size, only applied if manu==TRUE (default 43)
#'
#' @return data array
#'
#' @examples
#' \dontrun{
#' autoReadInput(inFile,getyearstart=-1,getyearstop=-1,manu=T,msize=2,mheadersize=0)
#' }
#'
#' @export
autoReadInput <- function(inFile,getyearstart=-1,getyearstop=-1,manu=F,msize=4,mheadersize=43){
  hdr=readheader(filename=inFile)$header
  print(hdr)
  startyear=hdr[3]
  stopyear=hdr[3]+hdr[4]-1
  ncells=hdr[6]
  nbands=hdr[7]
  if (getyearstart==-1){
    getyearstart=startyear
  }
  if (getyearstop==-1){
    getyearstop=stopyear
  }
  if (hdr[1]==1){#header version 1
    headersize=36
  }else if (hdr[1]==2){#header version 2
    headersize=43
  }else{ #header version 3
    headersize=51
  }
  if (length(hdr)>10){
    if (hdr[11]==0){
      size=1
      inputType="char"
    }else if(hdr[11]==1){
      size=2
      inputType="integer"
    }else if(hdr[11]==2){
      size=4
      inputType="integer"
    }else if(hdr[11]==3){
      size=4
      inputType="double"
    }else if(hdr[11]==4){
      size=8
      inputType="double"
    }
  }
  if (manu){
    size=msize
    headersize=mheadersize
  }
  if (getyearstop>stopyear){
    stop(paste("unexpected usage: getyearstop (",getyearstop,") larger than stopyear (",stopyear,") -- stopping"))
  }
  if (getyearstart<startyear){
    stop(paste("unexpected usage: getyearstart (",getyearstart,") smaller than startyear (",startyear,") -- stopping"))
  }
  nyears=getyearstop-getyearstart+1
  input <- file(inFile,"rb")
  seek(input, where = headersize+(getyearstart-startyear)*ncells*size*nbands, origin="start")
  if (inputType == "integer"){
    dataIn <- readBin(input,integer(),n = nyears*ncells*nbands, size=size)
  }else if (inputType == "double"){
    dataIn <- readBin(input,double(),n = nyears*ncells*nbands, size=size)
  }else{
    dataIn <- readBin(input,character(),n = nyears*ncells*nbands, size=size)
  }
  close(input)      #remove to save space
  print(paste("nyears=",nyears," nbands=",nbands," ncells=",ncells," size=",size," headersize=",headersize))
  if (nyears==1){
    dim(dataIn) <- c(nbands,ncells)
  }else{
    dim(dataIn) <- c(nbands,ncells,nyears)
  }
  return(dataIn*hdr[["scalar"]])
}

#' Transform even grid to lpjml grid
#'
#' Transform even grid (e.g. from a netcdf) to lpjml grid
#'
#' @param gridIn even grid to be transformed (e.g. c(720,360) for 0.5 deg)
#' @param lonIn longitude vector of input grid
#' @param latIn latitude vector of input grid
#' @param ncells number of lpj cells (default is 67420 for 30min res), 2298847 for 5min res
#' @param res resolution (default 0.5)
#'
#' @return outlist transformed data in lpjml format
#'
#' @examples
#' evenGrid2lpjGrid(gridIn=climateCategories,lonIn=seq(-179.75,179.75,0.5),latIn=seq(-89.75,89.75,0.5))
#'
#' @export
evenGrid2lpjGrid <- function(gridIn,lonIn,latIn,ncells=67420,res=0.5){
  l=length(dim(gridIn))
  if (l==2){
    outlist <- array(0,dim=c(ncells))
  }else if(l==3){
    outlist <- array(0,dim=c(ncells,length(gridIn[1,1,])))
  }
  if (l==2){
    for (i in 1:ncells){
      outlist[i] <- gridIn[which(round(lonIn,2)==lon[i]),which(round(latIn,2)==lat[i])] # minus lat[i] only if the grid is flipped in lat
    }
  }else if (l==3){
    for (y in c(1:length(gridIn[1,1,])) ){
      for (i in 1:ncells){
        outlist[i,y] <- gridIn[which(round(lonIn,2)==lon[i]),which(round(latIn,2)==lat[i]),y] # minus lat[i] only if the grid is flipped in lat
      }
    }
  }
  return(outlist)
}


#' Get header of LPJmL input file
#'
#' Reads the header of a binary CLM(2) LPJmL input file and returns the content as data_frame
#'
#' @param f.in file connection to read the header from
#'
#' @return data_frame of header data
#'     h$v : header version,
#'     h$o : order,
#'     h$fy : firstyear,
#'     h$ny : nyear,
#'     h$fc : firstcell,
#'     h$nc : ncells,
#'     h$nb : nbands,
#'     h$r : resolution,
#'     h$b : scaling
#'
#' @examples
#' freadheader(system.file("extdata", "grid.bin", package = "lpjmliotools", mustWork = TRUE))
#'
#' @export
freadheader <- function(f.in){
  h <- list()
  h$name <- readChar(f.in, 7)                  # header name
  #h$name <- readBin(f.in,character(), size=8)   # header name
  h$v <- readBin(f.in, integer(), n=1, size=4) # header version
  h$o <- readBin(f.in, integer(), n=1, size=4) # order
  h$fy <- readBin(f.in,integer(), n=1, size=4) # firstyear
  h$ny <- readBin(f.in,integer(), n=1, size=4) # nyear
  h$fc <- readBin(f.in, integer(),n=1,size=4)  # firstcell
  h$nc <- readBin(f.in,integer(),n=1,size=4)   # ncells
  h$nb <- readBin(f.in,integer(),n=1,size=4)   # nbands
  h$r <- readBin(f.in,double(),n=1, size=4)    # resolution
  h$b <- readBin(f.in,double(),n=1, size=4)    # scaling
  return(h)
}

#' Get country code list
#'
#' Reads and returns the list of country codes contained in
#'
#' @param fileName character string containing the file to read the cow from ()
#' @param ncells number of lpj cells (67420 for 30min res, 2298847 for 5min res)
#'
#' @return array of 2 IDs (countrycode,regioncode) for each of the 67420 gridcells
#'
#' @examples
#' lpjCOW(system.file("extdata", "cow_mg_2006.bin", package = "lpjmliotools", mustWork = TRUE))
#'
#' @export
lpjCOW <- function(fileName,ncells=67420){
  zz    <- file(fileName,"rb")
  seek(zz, where = 43, origin="start")
  cow <- readBin(zz, integer(), 2*ncells, size=2)
  dim(cow)=c(2,ncells)
  close(zz)
  return(cow)
}

#' Read monthly LPJmL output file
#'
#' Returns a range of years from a monthly LPJmL output of dimension c(ncells,12,nyears)
#'
#' @param inFile character string containing the file to read the data from
#' @param startyear absolute startyear of output
#' @param stopyear absolute stopyear of output
#' @param size size of each cell's data (2 or 4 bytes)
#' @param headersize size of header data (defaults to 0 bytes)
#' @param getyearstart start of range to return
#' @param getyearstop end of range to return
#' @param ncells number of lpj cells (67420 for 30min res, 2298847 for 5min res)
#'
#' @return array of monthly data c(ncells,12,nyears) for each of the 67420 gridcells over requested range of years
#'
#' @examples
#' \dontrun{
#' readMonthly(inFile="mwateramount.bin",startyear=1861,stopyear=2005,size=4,headersize=0,
#'             getyearstart=1984,getyearstop=2005)
#' }
#'
#' @export
readMonthly <- function(inFile,startyear,stopyear,size,headersize=0,getyearstart,getyearstop,ncells=67420){
  if (getyearstop>stopyear){
    stop(paste("unexpected usage: getyearstop (",getyearstop,") larger than stopyear (",stopyear,") -- stopping"))
  }
  if (getyearstart<startyear){
    stop(paste("unexpected usage: getyearstart (",getyearstart,") smaller than startyear (",startyear,") -- stopping"))
  }
  nyears=getyearstop-getyearstart+1
  input <- file(inFile,"rb")
  seek(input, where = headersize+(getyearstart-startyear)*12*ncells*size, origin="start")
  monthly <- readBin(input,double(),n = nyears*ncells*12, size=size)
  close(input)      #remove to save space
  if (nyears==1){
    dim(monthly) <- c(ncells,12)
  }else{
    dim(monthly) <- c(ncells,12,nyears)
  }
  return(monthly)
}

#' Read yearly LPJmL input file
#'
#' Returns a range of years from a LPJmL input of dimension c(ncells,nyears)
#'
#' @param inFile character string containing the file to read the data from
#' @param startyear absolute startyear of output
#' @param stopyear absolute stopyear of output
#' @param size size of each cell's data (2 or 4 bytes)
#' @param inputType type of variable to read, integer/double
#' @param headersize size of header data (defaults to 43 bytes)
#' @param getyearstart start of range to return
#' @param getyearstop end of range to return
#' @param ncells number of lpj cells (67420 for 30min res, 2298847 for 5min res)
#'
#' @return array of data c(ncells,nyears) for each of the 67420 gridcells over requested range of years
#'
#' @examples
#' \dontrun{
#' readYearlyInput(inFile="wateruse_1900_2005.bin",startyear=1900,stopyear=2005,size=4,headersize=43,
#'            getyearstart=1984,getyearstop=2005)
#' }
#'
#' @export
readYearlyInput <- function(inFile,startyear,stopyear,size,inputType,headersize=43,getyearstart,getyearstop,ncells=67420){
  if (getyearstop>stopyear){
    stop(paste("unexpected usage: getyearstop (",getyearstop,") larger than stopyear (",stopyear,") -- stopping"))
  }
  if (getyearstart<startyear){
    stop(paste("unexpected usage: getyearstart (",getyearstart,") smaller than startyear (",startyear,") -- stopping"))
  }
  nyears=getyearstop-getyearstart+1
  input <- file(inFile,"rb")
  seek(input, where = headersize+(getyearstart-startyear)*ncells*size, origin="start")
  if (inputType == "integer"){
    dataIn <- readBin(input,integer(),n = nyears*ncells, size=size)
  }else if (inputType == "double"){
    dataIn <- readBin(input,double(),n = nyears*ncells, size=size)
  }else{
    print("unknown input type")
  }
  close(input)      #remove to save space
  if (nyears==1){
    dim(dataIn) <- c(ncells)
  }else{
    dim(dataIn) <- c(ncells,nyears)
  }
  return(dataIn)
}

#' Read daily LPJmL output file
#'
#' Returns a range of years from a daily LPJmL output of dimension c(ncells,365,nyears)
#'
#' @param inFile character string containing the file to read the data from
#' @param startyear absolute startyear of output
#' @param stopyear absolute stopyear of output
#' @param size size of each cell's data (2 or 4 bytes)
#' @param headersize size of header data (defaults to 0 bytes)
#' @param getyearstart start of range to return
#' @param getyearstop end of range to return
#' @param ncells number of lpj cells (67420 for 30min res, 2298847 for 5min res)
#'
#' @return array of daily data c(ncells,365,nyears) for each of the 67420 gridcells over requested range of years
#'
#' @examples
#' \dontrun{
#' readDaily(inFile="mwateramount.bin",startyear=1861,stopyear=2005,size=4,headersize=0,
#'             getyearstart=1984,getyearstop=2005)
#' }
#'
#' @export
readDaily <- function(inFile,startyear,stopyear,size,headersize=0,getyearstart,getyearstop,ncells=67420){
  if (getyearstop>stopyear){
    stop(paste("unexpected usage: getyearstop (",getyearstop,") larger than stopyear (",stopyear,") -- stopping"))
  }
  if (getyearstart<startyear){
    stop(paste("unexpected usage: getyearstart (",getyearstart,") smaller than startyear (",startyear,") -- stopping"))
  }
  nyears=getyearstop-getyearstart+1
  input <- file(inFile,"rb")
  seek(input, where = headersize+(getyearstart-startyear)*365*ncells*size, origin="start")
  daily <- readBin(input,double(),n = nyears*ncells*365, size=size)
  close(input)      #remove to save space
  if (nyears==1){
    dim(daily) <- c(ncells,365)
  }else{
    dim(daily) <- c(ncells,365,nyears)
  }
  return(daily)
}

#' Read yearly LPJmL output file
#'
#' Returns a range of years from a LPJmL output of dimension c(ncells,nyears)
#'
#' @param inFile character string containing the file to read the data from
#' @param startyear absolute startyear of output
#' @param stopyear absolute stopyear of output
#' @param size size of each cell's data (2 or 4 bytes)
#' @param headersize size of header data (defaults to 0 bytes)
#' @param getyearstart start of range to return
#' @param getyearstop end of range to return
#' @param ncells number of lpj cells (67420 for 30min res, 2298847 for 5min res)
#'
#' @return array of data c(ncells,nyears) for each of the 67420 gridcells over requested range of years
#'
#' @examples
#' \dontrun{
#' readYearly(inFile="hdates.bin",startyear=1901,stopyear=2005,size=2,headersize=0,
#'            getyearstart=1984,getyearstop=2005)
#' }
#'
#' @export
readYearly <- function(inFile,startyear,stopyear,size,headersize=0,getyearstart,getyearstop,ncells=67420){
  if (getyearstop>stopyear){
    stop(paste("unexpected usage: getyearstop (",getyearstop,") larger than stopyear (",stopyear,") -- stopping"))
  }
  if (getyearstart<startyear){
    stop(paste("unexpected usage: getyearstart (",getyearstart,") smaller than startyear (",startyear,") -- stopping"))
  }
  nyears=getyearstop-getyearstart+1
  input <- file(inFile,"rb")
  seek(input, where = headersize+(getyearstart-startyear)*ncells*size, origin="start")
  if (size==2){
    dataIn <- readBin(input,integer(),n = nyears*ncells, size=size)
  }else if (size==4){
    dataIn <- readBin(input,double(),n = nyears*ncells, size=size)
  }else{
    print("unknown data size")
  }
  close(input)      #remove to save space
  if (nyears==1){
    dim(dataIn) <- c(ncells)
  }else{
    dim(dataIn) <- c(ncells,nyears)
  }
  return(dataIn)
}

#' Read cft LPJmL output
#'
#' Returns a range of years from a cft LPJmL output of dimension c(ncells,bands,nyears).
#'
#' @param inFile character string containing the file to read the data from
#' @param startyear absolute startyear of output
#' @param stopyear absolute stopyear of output
#' @param bands number of bands (32 for standard LPJmL output, 64 for standard input)
#' @param size size of each cell's data (2 or 4 bytes)
#' @param headersize size of header data (defaults to 0 bytes)
#' @param getyearstart start of range to return
#' @param getyearstop end of range to return
#' @param ncells number of lpj cells (67420 for 30min res, 2298847 for 5min res)
#'
#' @return array of data c(ncells,bands,nyears) for each of the 67420 gridcells over requested range of years or c(ncells,bands) for one year
#
#' @examples
#' \dontrun{
#' readCFToutput(inFile="cftfrac.bin",startyear=1861,stopyear=2005,bands=32,size=4,headersize=0,
#'         getyearstart=2005,getyearstop=2005,ncells=67420)
#' }
#'
#' @export
readCFToutput <- function(inFile,startyear,stopyear,bands,size,headersize,getyearstart,getyearstop,ncells=67420){
  if (getyearstop>stopyear){
    stop(paste("unexpected usage: getyearstop (",getyearstop,") larger than stopyear (",stopyear,") -- stopping"))
  }
  if (getyearstart<startyear){
    stop(paste("unexpected usage: getyearstart (",getyearstart,") smaller than startyear (",startyear,") -- stopping"))
  }
  nyears=getyearstop-getyearstart+1
  input <- file(inFile,"rb")
  seek(input, where = headersize+(getyearstart-startyear)*bands*ncells*size, origin="start")
  if (size==2){
    cftfracs <- readBin(input,integer(),n = nyears*ncells*bands, size=size)
  }else if (size==4){
    cftfracs <- readBin(input,double(),n = nyears*ncells*bands, size=size)
  }else{
    print("unknown data size")
  }
  close(input)
  if (nyears==1){
    dim(cftfracs) <- c(ncells,bands)
  }else{
    dim(cftfracs) <- c(ncells,bands,nyears)
  }
  return(cftfracs)
}

#' Read cft LPJmL input
#'
#' Returns a range of years from a cft LPJmL input of dimension c(ncells,bands,nyears).
#'
#' @param inFile character string containing the file to read the data from
#' @param startyear absolute startyear of output
#' @param stopyear absolute stopyear of output
#' @param bands number of cft-bands (32 for standard LPJmL input, 64 for standard input)
#' @param size size of each cell's data (2 or 4 bytes)
#' @param headersize size of header data (defaults to 43 bytes)
#' @param getyearstart start of range to return
#' @param getyearstop end of range to return
#' @param ncells number of lpj cells (67420 for 30min res, 2298847 for 5min res)
#'
#' @return array of cft-fractions c(bands,ncells,nyears) for each of the 67420 gridcells over requested range of years
#
#' @examples
#' \dontrun{
#' readCFTinput(inFile="cftinput.bin",startyear=1901,stopyear=2015,bands=64,size=4,headersize=43,
#'         getyearstart=2005,getyearstop=2005)
#' }
#'
#' @export
readCFTinput <- function(inFile,startyear,stopyear,bands,size,dtype,headersize=43,getyearstart,getyearstop,ncells=67420){
  if (getyearstop>stopyear){
    stop(paste("unexpected usage: getyearstop (",getyearstop,") larger than stopyear (",stopyear,") -- stopping"))
  }
  if (getyearstart<startyear){
    stop(paste("unexpected usage: getyearstart (",getyearstart,") smaller than startyear (",startyear,") -- stopping"))
  }
  nyears=getyearstop-getyearstart+1
  input <- file(inFile,"rb")
  seek(input, where = (getyearstart-startyear)*bands*ncells*size+headersize, origin="start")
  if (dtype=="integer"){
      cftfracs <- readBin(input,integer(),n = nyears*ncells*bands, size=size)
  }else if (dtype=="double"){
      cftfracs <- readBin(input,double(),n = nyears*ncells*bands, size=size)
  }else{
    print("unknown data type")
  }
  close(input)
  if (nyears==1){
    dim(cftfracs) <- c(bands,ncells)
  }else{
    dim(cftfracs) <- c(bands,ncells,nyears)
  }
  return(cftfracs)
}
