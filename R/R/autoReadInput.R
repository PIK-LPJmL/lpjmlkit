#' Automatically reads LPJmL input including header
#'
#' most header variables will be automatically read
#' for some combinations this won't work, thus
#' a manual override switch can be used to set size
#'
#' @param inFile name of file to read
#' @param getyearstart first year to be read (if not specified will default to startyear from header)
#' @param getyearstop final year to be read (if not specified will default to stopyear from header)
#' @param manu switch to allow for manual override of size (default False)
#' @param msize manual size, only applied if manu==TRUE (default 4)
#' @param mheadersize manual header size, only applied if manu==TRUE (default 43)
#'
#' @return data array
#'
#' @examples
#' \dontrun{
#' autoReadInput(inFile,getyearstart=-1,getyearstop=-1,manu=T,msize=2,mheadersize=0)
#' }
#'
#' @export
autoReadInput <- function(inFile,getyearstart=-1,getyearstop=-1,manu=F,msize=4,mheadersize=43){
  hdr=readheader(filename=inFile)$header
  print(hdr)
  startyear=hdr[3]
  stopyear=hdr[3]+hdr[4]-1
  ncells=hdr[6]
  nbands=hdr[7]
  if (getyearstart==-1){
    getyearstart=startyear
  }
  if (getyearstop==-1){
    getyearstop=stopyear
  }
  if (hdr[1]==1){#header version 1
    headersize=36
  }else if (hdr[1]==2){#header version 2
    headersize=43
  }else{ #header version 3
    headersize=51
  }
  if (length(hdr)>10){
    if (hdr[11]==0){
      size=1
      inputType="char"
    }else if(hdr[11]==1){
      size=2
      inputType="integer"
    }else if(hdr[11]==2){
      size=4
      inputType="integer"
    }else if(hdr[11]==3){
      size=4
      inputType="double"
    }else if(hdr[11]==4){
      size=8
      inputType="double"
    }
  }
  if (manu){
    size=msize
    headersize=mheadersize
  }
  if (getyearstop>stopyear){
    stop(paste("unexpected usage: getyearstop (",getyearstop,") larger than stopyear (",stopyear,") -- stopping"))
  }
  if (getyearstart<startyear){
    stop(paste("unexpected usage: getyearstart (",getyearstart,") smaller than startyear (",startyear,") -- stopping"))
  }
  nyears=getyearstop-getyearstart+1
  input <- file(inFile,"rb")
  seek(input, where = headersize+(getyearstart-startyear)*ncells*size*nbands, origin="start")
  if (inputType == "integer"){
    dataIn <- readBin(input,integer(),n = nyears*ncells*nbands, size=size)
  }else if (inputType == "double"){
    dataIn <- readBin(input,double(),n = nyears*ncells*nbands, size=size)
  }else{
    dataIn <- readBin(input,character(),n = nyears*ncells*nbands, size=size)
  }
  close(input)      #remove to save space
  print(paste("nyears=",nyears," nbands=",nbands," ncells=",ncells," size=",size," headersize=",headersize))
  if (nyears==1){
    dim(dataIn) <- c(nbands,ncells)
  }else{
    dim(dataIn) <- c(nbands,ncells,nyears)
  }
  return(dataIn*hdr[["scalar"]])
}
