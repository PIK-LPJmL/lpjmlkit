
#' Write header of LPJmL input file
#'
#' Writes the header of a binary CLM(2) LPJmL input file
#' into an open file connection
#'
#' @param file.out file connection to the file to append the header to
#' @param headername character string containing the name of the header
#' @param version CLM Version (default is 2)
#' @param firstyear first year of data set
#' @param lastyear last year covered in the data set
#' @param bands number of cft-bands (32 or 64 for standard input)
#' @param scalar for cft fracs set to 0.001
#' @param ncells number of lpj cells (67420 for 30min res, 2298847 for 5min res)
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' fwriteheader(file.out=cftfracs_file,headername="LPJLUSE",version=2, firstyear = 2006,lastyear = 2100, bands=64, scalar=0.001)
#' }
#'
#' @export
fwriteheader <- function(file.out,headername,version=2,firstyear,lastyear,bands,scalar,ncells=67420){
  nyears=lastyear-firstyear+1
  writeChar(headername,file.out,eos=NULL)
  writeBin(as.integer(version),file.out,size=4,endian=.Platform$endian)   # CLIMATE VERSION
  writeBin(as.integer(1),file.out,size=4,endian=.Platform$endian)       # ORDER
  writeBin(as.integer(firstyear),file.out,size=4,endian=.Platform$endian) # FIRSTYEAR
  writeBin(as.integer(nyears),file.out,size=4,endian=.Platform$endian)       # NYEAR
  writeBin(as.integer(0),file.out,size=4,endian=.Platform$endian)       # FIRSTCELL
  writeBin(as.integer(ncells),file.out,size=4,endian=.Platform$endian)       # NCELL
  writeBin(as.integer(bands),file.out,size=4,endian=.Platform$endian)       # NBAND
  writeBin(0.5,file.out,size=4,endian=.Platform$endian)               # CELLSIZE
  writeBin(scalar,file.out,size=4,endian=.Platform$endian)               # SCALAR
}
