#' Transform even grid to lpjml grid
#'
#' Transform even grid (e.g. from a netcdf) to lpjml grid
#'
#' @param gridIn even grid to be transformed (e.g. c(720,360) for 0.5 deg)
#' @param lonIn longitude vector of input grid
#' @param latIn latitude vector of input grid
#' @param ncells number of lpj cells (default is 67420 for 30min res), 2298847 for 5min res
#' @param res resolution (default 0.5)
#'
#' @return outlist transformed data in lpjml format
#'
#' @examples
#' evenGrid2lpjGrid(gridIn=climateCategories,lonIn=seq(-179.75,179.75,0.5),latIn=seq(-89.75,89.75,0.5))
#'
#' @export
evenGrid2lpjGrid <- function(gridIn,lonIn,latIn,ncells=67420,res=0.5){
  l=length(dim(gridIn))
  if (l==2){
    outlist <- array(0,dim=c(ncells))
  }else if(l==3){
    outlist <- array(0,dim=c(ncells,length(gridIn[1,1,])))
  }
  if (l==2){
    for (i in 1:ncells){
      outlist[i] <- gridIn[which(round(lonIn,2)==lon[i]),which(round(latIn,2)==lat[i])] # minus lat[i] only if the grid is flipped in lat
    }
  }else if (l==3){
    for (y in c(1:length(gridIn[1,1,])) ){
      for (i in 1:ncells){
        outlist[i,y] <- gridIn[which(round(lonIn,2)==lon[i]),which(round(latIn,2)==lat[i]),y] # minus lat[i] only if the grid is flipped in lat
      }
    }
  }
  return(outlist)
}
