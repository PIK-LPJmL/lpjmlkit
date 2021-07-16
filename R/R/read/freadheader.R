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
