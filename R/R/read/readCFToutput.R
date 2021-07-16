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
