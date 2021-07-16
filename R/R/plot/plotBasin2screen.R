#' Plot basin to screen
#'
#' Plot all cells of a basin for given cell index to R internal screen
#' target cell is marked red, basin cells blue
#'
#' @param cell index of the cell, whose basin should be plotted
#' @param endcellList lpjml array giving the index of the final drainage cell for each cell
#' @param zoom zoom in? (boolean) otherwise global plot
#' @param off offset amount in degrees to each side of the cell, only applied of zoom==T (defaults to 20)
#' @param ncells number of lpj cells (67420 for 30min res, 2298847 for 5min res)
#'
#' @return None
#'
#' @examples
#' plotBasin2screen(cell=20000,endcellList=endcell,zoom=F,off=20)
#'
#' @export
plotBasin2screen <- function(cell,endcellList,zoom=F,off=20,ncells=67420){#ind is vector
  data=array(0,dim=c(ncells))
  e=endcellList[cell]
  data[which(endcellList==e)]=1
  data[cell]=2
  palette <- c("white","blue","red")
  brks <- c(-0.5,0.5,1.5,2.5)
  ra <- raster::raster(ncols=720, nrows=360)
  range <- range(data)
  ra[raster::cellFromXY(ra,cbind(lon,lat))] <-  data
  ext=c(-180, 180, -60, 90)
  if (zoom){ext=c(lon[cell]-off,lon[cell]+off,lat[cell]-off,lat[cell]+off)}
  extent <- raster::extent(ext)
  par(bty="n",mar=c(0,0,0,0),oma=c(0,0,0,0))
  raster::plot(ra,ext=extent,breaks=brks,col=palette,main="",legend=FALSE,axes=FALSE)
  maps::map('world',add=TRUE,res=0.4, lwd=0.25,ylim=c(-60,90))
}
