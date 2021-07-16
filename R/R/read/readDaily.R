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
