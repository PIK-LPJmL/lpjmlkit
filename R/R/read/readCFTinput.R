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
