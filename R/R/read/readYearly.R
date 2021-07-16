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
