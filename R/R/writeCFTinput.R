#' Write cft LPJmL input file
#'
#' Writes a binary CLM(2) LPJmL input file with header from an array of dimension c(ncells,bands,nyears)
#' and checks whether the sums of all cft fracs per grid cell are always <= 1000.
#'
#' @param filename character string containing the name of the file to write the cft input in
#' @param input_array array of dimension c(ncells,bands,nyears)
#' @param firstyear first year in array
#' @param lastyear last year in array
#' @param bands number of cft-bands (32 or 64 for standard input)
#'
#' @return boolean containing TRUE (the sums of all cft fracs per grid cell are always equal or below 1000) or FALSE (the sums of all cft fracs per grid cell are not always equal or below 1000)
#'
#' @examples
#' \dontrun{
#' writeCFTinput(filename="cftfrac_rcp26_2006-2100_64bands.clm", input_array = CFT_2006_2100, firstyear=2006,lastyear=2100,bands=64)
#' }
#'
writeCFTinput <- function(filename, input_array, firstyear, lastyear, bands){
  nyears=lastyear-firstyear+1
  #check if sum of lu shares in array always <= 1000
  lutotal<-apply(input_array,c(2,3),sum)
  check<-FALSE
  range<-range(lutotal)
  if (range[2]<=1000){check[1]<-TRUE}
  print("LU share sums always equal to or below 1000?");print(check)
  # open file for binary writing
  f.out<- file(filename,"wb")
  # write header
  fwriteheader(f.out,"LPJLUSE",2, firstyear,nyears, bands, 0.001)
  # write data
  for (i in firstyear:lastyear){
    writeBin(as.integer(as.vector(input_array[,,i-firstyear+1])),f.out,size=2)
  }
  close(f.out)
}
