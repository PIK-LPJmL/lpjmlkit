Sys.setlocale('LC_ALL','C')
### ================ global variable definitions ============
ncells=67420
ndays=c(31,28,31,30,31,30,31,31,30,31,30,31)

### ================ writing routines ===================

#' write a header to a file
#'
#' expects a list following the structure returned by readheader() or
#' new_header()
#' will fail if output file exists, unless overwrite set to TRUE
#'
#' @param filename filename to write header into
#' @param header header list
#' @param overwrite overwrites output file if exists (default FALSE)
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' writeheader(filename="filename.clm", header=header, overwrite=FALSE)
#' }
#'
#' @export
writeheader <- function(filename, header, overwrite=FALSE) {
  if(!is.list(header)) stop("Error: header must be a list() object")
  if(is.null(header[["name"]]) || is.null(header[["header"]]) || is.null(header[["endian"]])) stop("Error: header must have elements name, header and endian")
  if(file.exists(filename)) {
    if(!overwrite) stop(paste("Error:", filename, "exists already. Set overwrite to TRUE if you want to force it to be overwritten"))
    print(paste("Warning:", filename, "exists already and will be overwritten."))
  }
  zz <- file(filename, "wb")
  writeBin(charToRaw(header$name), zz)
  writeBin(as.integer(header$header[c("version", "order", "firstyear", "nyear", "firstcell", "ncell", "nbands")]), zz, size=4, endian=header$endian)
  if(header$header["version"] > 1) {
    writeBin(as.double(header$header[c("cellsize_lon", "scalar")]), zz, size=4, endian=header$endian)
  }
  if(header$header["version"] > 2) {
    writeBin(as.double(header$header["cellsize_lat"]), zz, size=4, endian=header$endian)
    writeBin(as.integer(header$header["datatype"]), zz, size=4, endian=header$endian)
  }
  close(zz)
}

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

#' Write wateruse LPJmL input file
#'
#' Writes a binary CLM(2) LPJmL input file with header from an array of dimension c(ncells,bands,nyears)
#'
#' @param filename character string containing the name of the file to write the cft input in
#' @param input_array array of dimension c(ncells,bands,nyears) or c(ncells,nyears)
#' @param firstyear first year in array
#' @param lastyear last year in array
#' @param bands number of cft-bands (32 or 64 for standard input)
#' @param out_name LPJmL variable to write out e.g. "LPJLUSE" oder "LPJWUSE"
#' @param out_size size of integer to be written out (2/4)
#' @param out_scaling optional scaling factor to be applied, defaults to 1
#'
#' @examples
#' \dontrun{
#' writeWUinput(filename="wateruse_wd_2band_2006_2050.clm", input_array = waterinput, firstyear=2006, lastyear=2050, bands=2, out_name="LPJWUSE", out_size=4, out_scaling=1)
#' }
#'
writeWUinput <- function(filename, input_array, fromyear, toyear, out_bands, out_name, out_size, out_scaling=1){
  ncells=dim(input_array)[1]
  # open file for binary writing
  f.out<- file(filename,"wb")
  # write header
  print(paste("Writing Header to: ",filename, out_name, 2, fromyear, toyear, out_bands, out_scaling))

  fwriteheader(file.out = f.out, headername = out_name, version = 2, firstyear = fromyear, lastyear = toyear, bands = out_bands, scalar = out_scaling)
  # write data
  for (i in fromyear:toyear){
    if (out_bands==1){
      writeBin(as.integer(as.vector(input_array[,i-fromyear+1]*1/out_scaling)),f.out,size=out_size)
    }else{
      for (c in 1:ncells){
        writeBin(as.integer(as.vector(input_array[c,,i-fromyear+1]*1/out_scaling)),f.out,size=out_size)
      }
    }
  }
  close(f.out)
}
