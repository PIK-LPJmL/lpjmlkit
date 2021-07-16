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
