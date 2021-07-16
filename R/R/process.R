Sys.setlocale('LC_ALL','C')
### ================ global variable definitions ============
ncells=67420
ndays=c(31,28,31,30,31,30,31,31,30,31,30,31)

### ================ processing routines ===================

#' Creates a CLM header list-object for LPJmL
#'
#' Creates a header from scratch which can be saved to a file using
#' function writeheader()
#' Parameters without defaults that you always need to provide:
#' nyear, ncell, nbands
#'
#' @param name header name (default "LPJGRID")
#' @param version header CLM version (default 2)
#' @param order order of data items , either CELLYEAR (1) or YEARCELL (2) (default 1)
#' @param firstyear first year for data (default 1901)
#' @param nyear number of years contained in file
#' @param firstcell index of first data item (default 0)
#' @param ncell number of data items per year
#' @param nbands number of data elements per cell
#' @param cellsize_lon longitude cellsize in deg (default 0.5)
#' @param cellsize_lat latitude cellsize in deg (default 0.5)
#' @param scalar conversion factor (default 1)
#' @param datatyp data type in file (default 1)
#' @param endian endianness (default copied from local platform -- .Platform$endian)
#'
#' @return list-object with header content
#'
#' @examples
#' \dontrun{
#' new_header(name="LPJGRID", version=2, order=1, firstyear=1901, nyear=1, firstcell=0, ncell=67420, nbands=1, cellsize_lon=0.5, scalar=1, cellsize_lat=cellsize_lon, datatype=1, endian=.Platform$endian)
#' }
#'
#' @export
new_header <- function(name="LPJGRID", version=3, order=1, firstyear=1901, nyear, firstcell=0, ncell, nbands, cellsize_lon=0.5, scalar=1, cellsize_lat=cellsize_lon, datatype=1, endian=.Platform$endian) {
  header <- list()
  if(is.character(name) && length(name) == 1) {
    header[["name"]] <- name
  } else {
    stop("name must be a character vector of length 1")
  }
  header[["header"]] <- numeric(0)
  for(check in c("version", "order", "firstyear", "nyear", "firstcell", "ncell", "nbands")) {
    if(is.numeric(get(check)) && length(get(check)) == 1 && get(check)==as.integer(get(check))) {
      header[["header"]] <- c(header[["header"]], get(check))
      names(header[["header"]])[length(header[["header"]])] <- check
    } else {
      stop(paste(check, "must be an integer of length 1"))
    }
  }
  if(version >= 2) {
    for(check in c("cellsize_lon", "scalar")) {
      if(is.numeric(get(check)) && length(get(check)) == 1) {
        header[["header"]] <- c(header[["header"]], get(check))
        names(header[["header"]])[length(header[["header"]])] <- check
      } else {
        stop(paste(check, "must be a float of length 1"))
      }
    }
    if(version >= 3) {
      if(is.numeric(cellsize_lat) && length(cellsize_lat)==1) {
        header[["header"]] <- c(header[["header"]], cellsize_lat=cellsize_lat)
      } else {
        stop("cellsize_lat must be a float of length 1")
      }
      if(length(datatype)==1) {
        if(!is.null(LPJ_datatype(c(header[["header"]], datatype=datatype)))) {
          header[["header"]] <- c(header[["header"]], datatype=datatype)
          print(paste("Setting datatype to", typeof(LPJ_datatype(header)$type), "with size", LPJ_datatype(header)$size))
        } else {
          stop(paste("Unknown datatype", datatype))
        }
      } else {
        stop("datatype must be integer of length 1")
      }
    }
  }
  header[["endian"]] <- endian
  return(header)
}

#' Get R-specific datatype and size from LPJmL header CLM3
#'
#' This function only works properly with headers of CLM version3 !
#' it can be used within readBin()
#' given an LPJ file header (as returned by readheader()),
#' it returns a list with 2 items:
#' - datatype (to be used as the "what" parameter in readBin()
#' - size (to be used as the "size" parameter in readBin()
#' datatypes as currently implemented:
#' 0: character (size 1)
#' 1: short (integer size 2)
#' 2: integer (integer size 4)
#' 3: float (numeric size 4)
#' 4: double (numeric size 8)
#'
#' @param header header list
#'
#' @return list(datatype,size)
#'
#' @examples
#' \dontrun{
#' LPJ_datatype(header=header)
#' }
#'
#' @export
LPJ_datatype <- function(header) {
  if(is.list(header))
    header <- header$header
  if(!is.finite(as.integer(header["datatype"]))) {
    stop("Error in LPJ_datatype: header does not contain datatype field")
  }
  return(switch(as.integer(header["datatype"])+1, list(type=character(), size=1), list(type=integer(), size=2), list(type=integer(), size=4), list(type=double(), size=4), list(type=double(), size=8)))
}

#' Computes header size from header list
#'
#' this function can be used within seek() to skip over a header
#' (e.g. from readheader()) in a LPJmL input file
#'
#' @param header header list
#'
#' @return byte size of header
#'
#' @examples
#' \dontrun{
#' headersize(header=header)
#' }
#'
#' @export
headersize <- function(header) {
  if(!is.list(header)) {
    stop("Error in headersize: header must be a list")
  }
  if(!("name" %in% names(header)) || !("version" %in% names(header$header))) {
    stop("Error in headersize: invalid header")
  }
  return(nchar(header$name)+switch(as.integer(header$header["version"]), 7, 9, 11)*4)
}

#' Computes cellarea of ordinary lat/lon grids
#'
#' this function computes gridcell areas in m2 based on latitude
#' coordinate and angular cell size (resolution), assuming the earth
#' to be a sphere
#' lat can be a vector, res.lon and res.lat be single
#' values.
#'
#' @param lat vector of latutidinal grid-midpoints
#' @param res.lon resolution (degree) in longitude dimension (default 0.5)
#' @param res.lat resolution (degree) in latitude dimension (default res.lon)
#'
#' @return vector of cellsizes in m2
#'
#' @examples
#' \dontrun{
#' cellarea(header=header)
#' }
#'
#' @export
cellarea <- function(lat, res.lon=0.5, res.lat=res.lon) {
  earthradius <- 6371000.785
  cellwidth <- earthradius*pi/180
  return((cellwidth*res.lon)*(cellwidth*res.lat)*cos(lat/180*pi))
}

#' Create a raster object from an LPJmL data array
#'
#' Create a raster object from an LPJmL data array and the corresponding lat/lon arrays
#'
#' @param lpjarray lpjml array filled with data dim(lpjmlarray)==c(ncells). ncells==67420 for 30min resolution, or ncells==2298847 for 5min resolution
#' @param lat latitude vector of lpj array
#' @param lon longitude vector of lpj array
#'
#' @return ra raster object filled with the lpjml data on land
#'
#' @examples
#' \dontrun{
#' lpj2raster(lpjarray=discharge2000,lat=lat,lon=lon)
#' }
#'
#' @export
lpj2raster <- function(lpjarray,lat,lon){
    ncells=length(lat)
    if (ncells==67420){  # 30min resolution
      ra <- raster::raster(ncols=720, nrows=360)
    }else if (ncells==2298847){ # 5min res
      ra <- raster::raster(ncols=360*12, nrows=180*12)
    }
    range <- range(lpjarray)
    ra[raster::cellFromXY(ra,cbind(lon,lat))] <-  lpjarray
    return(ra)
  }

#' Show the full drainage route for cell ind
#'
#' Show the full downstream drainage route for cell ind until final drainage (ocean or inland sink)
#'
#' @param ind cell index to look at
#' @param routingTable routing table obtained from reading in drainage network and adding 1, listing for each cell the next cell discharge is routed to and for sinks -0
#'
#' @return list of downstream cells
#'
#' @examples
#' \dontrun{
#' showRoute(ind=20000,routingTable=routing)
#' }
#'
#' @export
showRoute <- function(ind,routingTable){
    if (routingTable[ind]<1){return(ind)} #can be 0 or -8 -> endcell or nacell
    else{return(c(ind,showRoute(routingTable[ind],routingTable)))}
  }

#' Return next direct upstream cells for cell ind
#'
#' Return a list of the next direct upstream cells for cell ind (draining into ind)
#'
#' @param ind cell index to look at
#' @param routingTable routing table obtained from reading in drainage network and adding 1, listing for each cell the next cell discharge is routed to and for sinks -0
#'
#' @return list of direct (neighboring) upstream cells
#'
#' @examples
#' \dontrun{
#' upstreamCells(ind=20000,routingTable=routing)
#' }
#'
#' @export
upstreamCells <- function(ind,routingTable){
    return(which(routingTable==ind))
  }

#' Calculate environmental flow requirements (EFRs)
#'
#' Calculate environmental flow requirements (EFRs), based on 30 years of monthly discharge input
#'
#' @param discharge 30 years monthly discharge array dim(discharge)=c(ncells,12,30)
#' @param method EFR method to be used ("VMF","Q90Q50","PBpaper"), "PBpaper" is the modified version of VMF used for the Steffen et al. 2015 PB paper
#'
#' @return EFRs as volumes like in discharge input, dim(EFRs)=c(ncells,12)
#'
#' @examples
#' \dontrun{
#' calcEFRs(discharge=mdischarge_preindustrial,method="VMF")
#' }
#'
#' @export
calcEFRs <- function(discharge,method="VMF"){
    ncells=dim(discharge)[1]
    efrMethodsSet = c("PBpaper","VMF","Q90Q50")
    #method valid?
    if (!method %in% efrMethodsSet){
      print(paste("EFR method not recognized, use one of: ",paste(efrMethodsSet,collapse = " ")))
      return(NA)
    }
    #make sure discharge is a ncells,12month,30year array
    if (!all(dim(discharge) == c(ncells,12,30))){
      print(paste("discharge array has wrong dimension, use c(ncells,nmonths=12,nyears=30)"))
      return(NA)
    }
    efrs=discharge[,,1]*0 #initialize efrs array

    #calculate mean monthly flow and mean annual flow
    MMF=apply(discharge,c(1,2),mean)
    quantiles=apply(discharge,c(1,2),quantile,probs=c(0.5,0.9),na.rm=T)
    MAF=rep(rowMeans(MMF),times=12)
    dim(MAF)=c(ncells,12)
    Q90=quantiles[2,,]
    Q50=quantiles[1,,]
    remove(quantiles)

    if (method==efrMethodsSet[1]){ #"PBpaper" - Steffen et al. 2015
      efrs[MMF<=0.4*MAF]=0.75*MMF[MMF<=0.4*MAF] # low flow months
      efrs[MMF>0.4*MAF & MMF<=0.8*MAF]=0.7*MMF[MMF>0.4*MAF & MMF<=0.8*MAF] # intermediate flow months
      efrs[MMF>0.8*MAF]=0.45*MMF[MMF>0.8*MAF] # high flow months
    }else if (method==efrMethodsSet[2]){ # "VMF" - Pastor et al. 2014
      efrs[MMF<=0.4*MAF]=0.6*MMF[MMF<=0.4*MAF] # low flow months
      efrs[MMF>0.4*MAF & MMF<=0.8*MAF]=0.45*MMF[MMF>0.4*MAF & MMF<=0.8*MAF] # intermediate flow months
      efrs[MMF>0.8*MAF]=0.3*MMF[MMF>0.8*MAF] # high flow months
    }else if (method==efrMethodsSet[3]){ # "Q90Q50" - Pastor et al. 2014
      efrs[MMF<=MAF]=Q90[MMF<=MAF] # low flow months
      efrs[MMF>MAF]=Q50[MMF>MAF] # high flow months
    }
    return(efrs)
}

#' Return flow season
#'
#' Return flow season based on the chosen EFR method. "l" (low flow), "i" (intermediate - if applicable), or "h" (high flow)
#'
#' @param discharge 30 years monthly discharge array dim(discharge)=c(ncells,12,30)
#' @param method EFR method to be used ("VMF","Q90Q50","PBpaper"), "PBpaper" is the modified version of VMF used for the Steffen et al. 2015 PB paper
#'
#' @return flow season array:  "l" (low flow), "i" (intermediate - if applicable), or "h" (high flow)
#'
#' @examples
#' \dontrun{
#' flowSeason(discharge=mdischarge_preindustrial,method="VMF")
#' }
#'
#' @export
flowSeason <- function(discharge,method="VMF"){
    ncells=dim(discharge)[1]
    efrMethodsSet = c("PBpaper","VMF","Q90Q50")
    #method valid?
    if (!method %in% efrMethodsSet){
      print(paste("EFR method not recognized, use one of: ",paste(efrMethodsSet,collapse = " ")))
      return(NA)
    }
    #make sure discharge is a ncells,12month array
    if (!all(dim(discharge) == c(ncells,12))){
      print(paste("discharge array has wrong dimension, use c(ncells,nmonths=12)"))
      return(NA)
    }
    fs=discharge*0 #initialize fs array

    #calculate mean monthly flow and mean annual flow
    MMF=discharge
    MAF=rep(rowMeans(MMF),times=12)
    dim(MAF)=c(ncells,12)

    if (method==efrMethodsSet[1]){ #"PBpaper" - Steffen et al. 2015
      fs[MMF<=0.4*MAF]="l"
      fs[MMF>0.4*MAF & MMF<=0.8*MAF]="i"
      fs[MMF>0.8*MAF]="h"
    }else if (method==efrMethodsSet[2]){ # "VMF" - Pastor et al. 2014
      fs[MMF<=0.4*MAF]=1#"l"
      fs[MMF>0.4*MAF & MMF<=0.8*MAF]=2#"i"
      fs[MMF>0.8*MAF]=3#"h"
    }else if (method==efrMethodsSet[3]){ # "Q90Q50" - Pastor et al. 2014
      fs[MMF<=MAF]="l"
      fs[MMF>MAF]="h"
    }

    return(fs)
  }

#' Aggregate from daily values to monthly values
#'
#' Function to aggregate from daily values to monthly values. Can also be applied for several year arrays:
#' apply(X = daily_discharge,MARGIN = c(1,3),FUN = daily2monthly,method="sum")
#' with dim(daily_discharge)=c(67420,365,30)
#'
#' @param daily array with daily values dim(daily)=c(365)
#' @param method function for aggregation: sum or mean
#'
#' @return monthly array
#'
#' @examples
#' \dontrun{
#' daily2monthly(daily=ddischarge,method="sum")
#' }
#'
#' @export
daily2monthly <- function(daily,method="sum"){
    nDays=c(31,28,31,30,31,30,31,31,30,31,30,31)
    if (!length(daily)==365) print("Error - input is not in daily format")
    monthly=array(0,dim=c(12))
    for (m in 1:12){
      if (m==1) fdom=1
      else fdom=sum(nDays[1:(m-1)])+1
      ldom=fdom+nDays[m]-1
      if (method=="sum"){
        monthly[m]=sum(daily[fdom:ldom])
      }else if (method=="mean"){
        monthly[m]=mean(daily[fdom:ldom])
      }else print("Error - method not recognized")
    }
    return(monthly)
  }

#' Remove highflows from a discharge input
#'
#' Remove highflows (above the given quantile) from a discharge input and return the modified discharge
#'
#' @param ddischarge daily discharge input
#' @param threshold all highflows above this quantile (0.9 for Q90) will be removed
#' @param period calculate the highflows based on all days from all years ("all"), separately for each year ("year"), or just from the days which have positive discharge ("posDischarge")
#'
#' @return modified discharge array with the highflows removed
#'
#' @examples
#' \dontrun{
#' removeHighFlows(ddischarge=daily_discharge_preindustrial,threshold=0.8,period="year")
#' }
#'
#' @export
removeHighFlows <- function(ddischarge,threshold=0.9,period="all"){
    years=dim(ddischarge)[3]
    #make sure discharge is a ncells,365,X array
    if (!all(dim(ddischarge)[1:2] == c(ncells,365))){
      print(paste("discharge array has wrong dimension, use c(ncells,ndays=365)"))
      return(NA)
    }
    if (period=="all"){
      daily_quantiles=apply(ddischarge,c(1),quantile,probs=c(threshold))
      dailyQ90=rep(daily_quantiles,years*365)
      dim(dailyQ90)=c(ncells,365,years)
    }else if (period=="year"){
      daily_quantiles=apply(ddischarge,c(1,3),quantile,probs=c(threshold))
      dailyQ90=rep(daily_quantiles,365)
      dim(dailyQ90)=c(ncells,years,365)
      dailyQ90=aperm(dailyQ90,c(1,3,2))
    }else if (period=="posDischarge"){
      a=ddischarge
      a[a==0]=NA
      daily_quantiles=apply(a,c(1),quantile,probs=c(threshold),na.rm=T)
      dailyQ90=rep(daily_quantiles,years*365)
      dim(dailyQ90)=c(ncells,365,years)
    }else{
      print(paste("parameter period needs to be set as 'all', 'posDischarge' or 'year'"))
      return(NA)
    }
    ddischarge[ddischarge>dailyQ90]=0
    return(ddischarge)
  }

#' Aggregate LPJmL to country level
#'
#' Aggregates all data in lpjml format to country level, using the aggregation Method provided (e.g. sum, mean)
#'
#' @param input lpjml array to be aggregated c(ncells)
#' @param cowList country-list giving the country code for each LPJmL cell dim=c(ncells)
#' @param aggMethod method to aggregate data to country level ("sum" - e.g. for precipitation - or "mean" - e.g. for temperatures)
#'
#' @return country-array
#'
#' @examples
#' aggregateLPJmLdata2Country(input=surface_temperature,cowList=cowList,aggMethod="mean")
#'
#' @export
aggregateLPJmLdata2Country <- function(input,cowList,aggMethod="sum"){
  clist=sort(unique(cowList))
  dataList=array(0,dim=length(clist))
  for (c in 1:length(dataList)){
    if (aggMethod=="sum"){
      dataList[c]=sum(input[which(cowList==clist[c])])
    }else if (aggMethod=="mean"){
      dataList[c]=mean(input[which(cowList==clist[c])])
    }else{print("Unknown aggregation Method aggMethod")}
  }
  rownames(dataList)=clist
  return(dataList)
}
