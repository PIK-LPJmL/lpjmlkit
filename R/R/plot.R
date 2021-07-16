Sys.setlocale('LC_ALL','C')
### ================ global variable definitions ============
ncells=67420
ndays=c(31,28,31,30,31,30,31,31,30,31,30,31)

### ================ plotting routines ===================

#' Plot global LPJmL data with manual breaks and colorramp
#'
#' Creates a PNG/eps plot of a global LPJmL array dim=c(67420) with manual
#' breaks and a custom colorramp (if manual colorramp is not provided, YlGnBu is used)
#'
#' @param data LPJmL specific array c(67420) to be plotted
#' @param file character string for location/file to save plot to
#' @param title character string title for plot
#' @param brks array with breaks to be used
#' @param palette color palette string (defaults to "YlGnBu")
#' @param legendtitle character string legend title
#' @param legYes plot legend (boolean)
#' @param eps write eps file instead of PNG (boolean)
#'
#' @return None
#'
#' @examples
#' plotMonth(data=irrigation2006,file=paste("~/","mwateramount_2005_06.png",sep=""),
#'             title = paste("irrigation amount 2006 in mm/yr",sep=""),
#'             legendtitle="legendtitle",legYes=TRUE,eps=FALSE)
#'
#' @export
plotGlobalMan <- function(data,file,title,brks,palette="YlGnBu",legendtitle,legYes,eps){
  if (!length(palette)==(length(brks)-1)){colorRampPalette(RColorBrewer::brewer.pal(9,palette))(length(brks)-1)}
  ires=2
  legendticks=seq(from=0,to = 100,length.out = length(brks))
  data[data<brks[1]] <- brks[1]
  data[data>brks[length(brks)]] <- brks[length(brks)]
  if (eps){
    file=strsplit(file,".",fixed=TRUE)[[1]]
    file=paste(c(file[1:(length(file)-1)],"eps"),collapse=".")
    ps.options(family = c("Helvetica"), pointsize = 18)
    postscript(file,horizontal = FALSE, onefile = FALSE, width=22, height=8.5,paper="special")
  }else{
    png(file, width=800*ires, height=400*ires, units="px",res=400,pointsize = 4)
  }
  ra <- raster::raster(ncols=360*ires, nrows=180*ires)
  range <- range(data)
  ra[raster::cellFromXY(ra,cbind(lon,lat))] <-  data
  extent <- raster::extent(c(-180, 180, -60, 90))
  if (legYes){
    par(bty="n",oma=c(0,0,0,0),mar=c(0,0,0,3),xpd=T)
  }else{
    par(bty="n",oma=c(0,0,0,0),mar=c(0,0,0,0))
  }
  raster::plot(ra,ext=extent,breaks=brks,col=palette,main="",legend=FALSE,axes=FALSE,maxpixels=360*180*ires*ires)
  title(title,line=-1)
  if (legYes){
    fields::image.plot(legend.only=TRUE,zlim=range(brks),col = palette,useRaster=FALSE,breaks=legendticks,
                       lab.breaks=round(brks,2),legend.shrink = 0.8,legend.args=list(legendtitle,side=3, font=2, line=1))
  }
  maps::map('world',add=TRUE,res=0, lwd=0.1,ylim=c(-60,90))
  dev.off()
}

#' Plot global country data
#'
#' Plot an array of country data, e.g. obtained from aggregateLPJmLdataCountry()
#'
#' @param data country array to be plotted c(ncountries)
#' @param cowList country-list giving the country code for each LPJmL cell dim=c(ncells)
#' @param file character string for location/file to save plot to
#' @param sty array with breaks to be used
#' @param title character string title for plot
#' @param palette color palette string (defaults to "YlGnBu")
#' @param legendtitle character string legend title
#' @param ncells number of lpj cells (67420 for 30min res, 2298847 for 5min res)
#'
#' @return None
#'
#' @examples
#' plotCountryData(data=irrigation2006,file=paste("~/","mwateramount_2005_06.png",sep=""),
#'             title = paste("irrigation amount 2006 in mm/yr",sep=""),
#'             legendtitle="legendtitle",legYes=TRUE,eps=FALSE)
#'
#' @export
plotCountryData <- function(data,cowList,file,sty="lin",title="",legendtitle="",ncells=67420){
  ra=range(data,na.rm = T)
  if (sty=="lin"){
    brks=seq(from=ra[1],to = ra[2],length.out = 12)
    palette=RColorBrewer::brewer.pal(11,"Spectral")
  }else if (sty=="log"){
    if (min(data)<0){
      brks=c(-(2^seq(from=log(ra[2],base=2),to = 1,length.out = 6)),2^seq(from=1,to = log(ra[2],base=2),length.out = 6))
      palette=RColorBrewer::brewer.pal(11,"RdBu")
    }else{
      palette=RColorBrewer::brewer.pal(11,"Spectral")
      brks=c(0,2^seq(from=2,to = log(ra[2],base=2),length.out = 11))
    }
  }else{
    print("Style not known, use 'log' or 'lin'")
  }
  legendticks=seq(from=0,to = 100,length.out = length(brks))
  lpjdata=array(NA,dim=ncells)
  co=sort(unique(cowList))
  for (c in 1:length(co)){
    lpjdata[which(cowList==co[c])]=data[c]
  }
  png(file, width=1600, height=800, units="px",res=400,pointsize = 4)
  ra <- raster::raster(ncols=720, nrows=360)
  ra[raster::cellFromXY(ra,cbind(lon,lat))] <-  lpjdata
  extent <- raster::extent(c(-180, 180, -60, 90))
  par(bty="n",oma=c(0,0,0,0),mar=c(0,0,0,3),xpd=T)
  raster::plot(ra,ext=extent,breaks=brks,col=palette,main="",legend=FALSE,axes=FALSE,maxpixels=360*180*4)
  title(title,line=-1)
  fields::image.plot(legend.only=TRUE,zlim=range(brks),col = palette,useRaster=FALSE,breaks=legendticks,
                     lab.breaks=round(brks,2),legend.shrink = 0.8,legend.args=list(legendtitle,side=3, font=2, line=1))
  maps::map('world',add=TRUE,res=0, lwd=0.1,ylim=c(-60,90))
  dev.off()
}

#' Plot a zoomed in part from a LPJmL array
#'
#' Creates a PNG/eps with a plot of a global LPJmL array
#'    Data is plotted in range: c(-2^pow2max,-2^-pow2min,0,2^-pow2min,2^pow2max)
#'    colors for pos and neg values can be given, default is Blues for the positive
#'    and Reds for the negative numbers
#'    0-range (from 2^-pow2min to 2^pow2min) is white.
#'    The negatives can be omitted by setting onlyPos=T, in case there are only pos values.
#'
#' @param data array with data to plot in LPJmL specific array c(67420)
#' @param file character string for location/file to save plot to
#' @param title character string title for plot
#' @param pow2max upper (positive) end of data range to plot (2^pow2max)
#' @param pow2min smallest positive number to be distinguished from 0 (2^-pow2min)
#' @param ext extent to be plotted c(xmin,xmax,ymin,ymax)
#' @param colPos color palette for the positives
#' @param colNeg color palette for the negatives
#' @param legendtitle character string legend title
#' @param legYes show legend (boolean)
#' @param onlyPos show only positive (boolean)
#' @param eps write eps file instead of PNG (boolean)
#' @param ires resolution factor (2 for 30min, 12 for 5min)
#' @param lon longitude array
#' @param lat latitude array
#' @param map boolean whether to add country borders to the plot (defaults to T)
#'
#' @return None
#'
#' @examples
#' plotRegionalW(data=irrigation2006,file=paste("~/","mwateramount_2005_06.png",sep=""),
#'             title = paste("irrigation amount 2006 in mm/yr",sep=""),pow2max=15,pow2min=0,ext=c(120,160,-10,10),
#'             legendtitle="legendtitle",legYes=TRUE,eps=FALSE,ires=2,lon=lon,lat=lat,map=T)
#'
#' @export
plotRegionalW <- function(data,file,title,pow2max,pow2min,ext,colPos="Blues",colNeg="Reds",legendtitle,legYes,onlyPos=F,eps,ires=12,lon,lat,map=T){
  if (onlyPos){
    legendticks <- c(0,2^seq(pow2min,pow2max,1))
    brks <- c(seq(pow2min,pow2max,length.out = length(legendticks)))
    palette <- c("white",colorRampPalette(RColorBrewer::brewer.pal(9,colPos))(length(legendticks)-2))
  }else{
    legendticks <- c(-(2^seq(pow2max,pow2min,-1)),2^seq(pow2min,pow2max,1))
    brks <- seq(-pow2max,pow2max,length.out = length(legendticks))
    palette <- c(rev(colorRampPalette(RColorBrewer::brewer.pal(9,colNeg))(length(legendticks)/2-1)),"white",colorRampPalette(RColorBrewer::brewer.pal(9,colPos))(length(legendticks)/2-1))
  }
  data[data<legendticks[1]] <- legendticks[1]
  data[data>legendticks[length(legendticks)]] <- legendticks[length(legendticks)]
  if (eps){
    file=strsplit(file,".",fixed=TRUE)[[1]]
    file=paste(c(file[1:(length(file)-1)],"eps"),collapse=".")
    ps.options(family = c("Helvetica"), pointsize = 18)
    postscript(file,horizontal = FALSE, onefile = FALSE, width=22, height=8.5,paper="special")
  }else{
    png(file, width=800*ires, height=400*ires, units="px",res=400,pointsize = 4)
  }
  #print(paste(ires,length(lon),length(lat)))
  ra <- raster::raster(ncols=360*ires, nrows=180*ires)
  range <- range(data)
  ra[raster::cellFromXY(ra,cbind(lon,lat))] <-  data
  extent <- raster::extent(ext)
  if (legYes){
    par(bty="n",oma=c(0,0,0,0),mar=c(0,0,0,3),xpd=T)
  }else{
    par(bty="n",oma=c(0,0,0,0),mar=c(0,0,0,0))
  }
  raster::plot(ra,ext=extent,breaks=legendticks,col=palette,main="",legend=FALSE,axes=FALSE,maxpixels=360*180*ires*ires)
  title(title,line=-1)
  if (legYes){
    fields::image.plot(legend.only=TRUE,zlim=c(-pow2max,pow2max),col = palette,useRaster=FALSE,breaks=brks,lab.breaks=round(legendticks,2),legend.shrink = 0.8,
                       legend.args=list(legendtitle,side=3, font=2, line=1))
  }
  if(map){maps::map('world',add=TRUE,res=0, lwd=0.1,ylim=c(-60,90))}
  dev.off()
}

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

#' Plot global LPJmL grid with flexibility to zoom into a region,
#'
#' Creates a PNG/eps with a plot
#'
#' @param data array with month index to plot in LPJmL specific array c(67420)
#' @param file character string for location/file to save plot to
#' @param title character string title for plot
#' @param legendtitle character string legend title
#' @param legYes plot legend (boolean)
#' @param eps write eps file instead of PNG (boolean)
#'
#' @return None
#'
#' @examples
#' plotMonth(data=irrigation2006,file=paste("~/","mwateramount_2005_06.png",sep=""),
#'             title = paste("irrigation amount 2006 in mm/yr",sep=""),
#'             legendtitle="legendtitle",legYes=TRUE,eps=FALSE)
#'
#' @export
plotGlobalWflex <- function(data,file,title,man=F,mbrk=-1,mpalette=-1,pow2max,pow2min,colPos="Blues",colNeg="Reds",legendtitle,legYes,onlyPos=F,eps,ires=12,lon,lat,map=T,ext=c(-180, 180, -60, 90),logscale=T){

  if (logscale){
    if (onlyPos){
      legendticks <- c(0,2^seq(pow2min,pow2max,1))
      brks <- c(seq(pow2min,pow2max,length.out = length(legendticks)))
      palette <- c("white",colorRampPalette(RColorBrewer::brewer.pal(9,colPos))(length(legendticks)-2))
    }else{
      legendticks <- c(-(2^seq(pow2max,pow2min,-1)),2^seq(pow2min,pow2max,1))
      brks <- seq(-pow2max,pow2max,length.out = length(legendticks))
      palette <- c(rev(colorRampPalette(RColorBrewer::brewer.pal(9,colNeg))(length(legendticks)/2-1)),"white",colorRampPalette(RColorBrewer::brewer.pal(9,colPos))(length(legendticks)/2-1))
    }
  }else{
    if (!man){
    lout=1+2*7
    if (onlyPos){
      legendticks <- round(seq(0,(2^pow2max),length.out = lout),0)
      brks <- legendticks
      palette <- c("white",colorRampPalette(RColorBrewer::brewer.pal(9,colPos))(length(legendticks)-2))
    }else{
      legendticks <- round(seq(-(2^pow2max),(2^pow2max),length.out = lout),0)
      brks <- legendticks
      palette <- c(rev(colorRampPalette(RColorBrewer::brewer.pal(9,colNeg))(length(legendticks)/2-1)),"white",colorRampPalette(RColorBrewer::brewer.pal(9,colPos))(length(legendticks)/2-1))
    }
    }else{
      brks=mbrk
      legendticks=mbrk
      palette=mpalette
    }
  }

  data[data<legendticks[1]] <- legendticks[1]
  data[data>legendticks[length(legendticks)]] <- legendticks[length(legendticks)]
  if (max(ext[1:2])>180){
    lon[lon<0]=lon[lon<0]+360
  }
  #print(paste0("range(lon)",range(lon)))
  xmagn=(ext[2]-ext[1])/360
  ymagn=(ext[4]-ext[3])/180
  if (eps){
    file=strsplit(file,".",fixed=TRUE)[[1]]
    file=paste(c(file[1:(length(file)-1)],"eps"),collapse=".")
    ps.options(family = c("Helvetica"), pointsize = 18)
    postscript(file,horizontal = FALSE, onefile = FALSE, width=22, height=8.5,paper="special")
  }else{
    png(file, width=800*ires*xmagn, height=360*ires*ymagn, units="px",res=400,pointsize = 4)
  }
  ra <- raster::raster(ncols=360*ires, nrows=180*ires)
  range <- range(data)
  ra[raster::cellFromXY(ra,cbind(lon,lat))] <-  data
  extent <- raster::extent(ext)
  if (legYes){
    par(bty="n",oma=c(0,0,0,6),mar=c(0,0,0,6),xpd=T)
  }else{
    par(bty="n",oma=c(0,0,0,0),mar=c(0,0,0,0))
  }
  raster::plot(ra,ext=extent,breaks=legendticks,col=palette,main="",legend=FALSE,axes=FALSE,maxpixels=360*180*ires*ires)
  title(title,line=-1,cex.main=4*xmagn*ires/12)
  if (legYes){
    fields::image.plot(legend.only=TRUE,zlim=range(data),col = palette,useRaster=FALSE,breaks=brks,lab.breaks=round(legendticks,2),legend.width = 6*xmagn,
                       legend.args=list(legendtitle,side=3, font=2, line=0),axis.args=list(cex.axis=4*xmagn*ires/12))
  }
  if(map){maps::map('world',add=TRUE,res=0, lwd=0.1,ylim=c(-60,90))}
  dev.off()
}


#' Plot global month indices on LPJmL grid, for example for months with highest precipitation
#'
#' Creates a PNG/eps with a plot of a month index from 1:12
#'
#' @param data array with month index to plot in LPJmL specific array c(67420)
#' @param file character string for location/file to save plot to
#' @param title character string title for plot
#' @param legendtitle character string legend title
#' @param legYes plot legend (boolean)
#' @param eps write eps file instead of PNG (boolean)
#'
#' @return None
#'
#' @examples
#' plotMonth(data=irrigation2006,file=paste("~/","mwateramount_2005_06.png",sep=""),
#'             title = paste("irrigation amount 2006 in mm/yr",sep=""),
#'             legendtitle="legendtitle",legYes=TRUE,eps=FALSE)
#'
#' @export
plotMonth <- function(data,file,title,legendtitle,legYes,eps){
  #legendticks <- c("extreme high","high","medium","low","no","low","medium","high","extreme high")
  brks <- seq(-0.5,12.5,1)

  data[data<0] <- 0
  data[data>12] <- 0

  palette <- c("gray",RColorBrewer::brewer.pal(12,"Paired")[c(1,2,11,3,4,7,8,5,6,12,9,10)])
  if (eps){
    file=strsplit(file,".",fixed=TRUE)[[1]]
    file=paste(c(file[1:(length(file)-1)],"eps"),collapse=".")
    ps.options(family = c("Helvetica"), pointsize = 18)
    postscript(file,horizontal = FALSE, onefile = FALSE, width=22, height=8.5,paper="special")
  }else{
    png(file, width=7.25, height=3.5, units="in", res=300, pointsize=6,type="cairo")
  }
  ra <- raster::raster(ncols=720, nrows=360)
  range <- range(data)
  ra[raster::cellFromXY(ra,cbind(lon,lat))] <-  data
  extent <- raster::extent(c(-180, 180, -60, 90))
  if (legYes){
    par(bty="n",oma=c(0,0,0,0),mar=c(0,0,0,3),xpd=T)
  }else{
    par(bty="n",oma=c(0,0,0,0),mar=c(0,0,0,0))
  }
  raster::plot(ra,ext=extent,breaks=brks,col=palette,main="",legend=FALSE,axes=FALSE)
  title(title,line=-1)
  maps::map('world',add=TRUE,res=0.4, lwd=0.25,ylim=c(-60,90))
  if (legYes){
    legend("right",legend=c("NA","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),fill = palette,title=legendtitle)
  }
  dev.off()
}


#' Plot global LPJmL array
#'
#' Creates a PNG/eps with a plot of a global LPJmL array
#'    Data is plotted in range: c(-2^pow2max,-2^-pow2min,0,2^-pow2min,2^pow2max)
#'    colors for pos and neg values can be given, default is Blues for the positive
#'    and Reds for the negative numbers
#'    0-range (from 2^-pow2min to 2^pow2min) is white.
#'    The negatives can be omitted by setting onlyPos=T, in case there are only pos values.
#'
#' @param data array with data to plot in LPJmL specific array c(67420)
#' @param file character string for location/file to save plot to
#' @param title character string title for plot
#' @param pow2max upper (positive) end of data range to plot (2^pow2max)
#' @param pow2min smallest positive number to be distinguished from 0 (2^-pow2min)
#' @param colPos color palette for the positives
#' @param colNeg color palette for the negatives
#' @param legendtitle character string legend title
#' @param legYes show legend (boolean)
#' @param onlyPos show only positive (boolean)
#' @param eps write eps file instead of PNG (boolean)
#'
#' @return None
#'
#' @examples
#' plotGlobalW(data=irrigation2006,file=paste("~/","mwateramount_2005_06.png",sep=""),
#'             title = paste("irrigation amount 2006 in mm/yr",sep=""),pow2max=15,pow2min=0,
#'             legendtitle="legendtitle",legYes=TRUE,eps=FALSE)
#'
#' @export
plotGlobalW <- function(data,file,title,pow2max,pow2min,colPos="Blues",colNeg="Reds",legendtitle,legYes,onlyPos=F,eps){
  if (onlyPos){
    legendticks <- c(0,2^seq(pow2min,pow2max,1))
    brks <- c(seq(pow2min,pow2max,length.out = length(legendticks)))
    palette <- c("white",colorRampPalette(RColorBrewer::brewer.pal(9,colPos))(length(legendticks)-2))
  }else{
    legendticks <- c(-(2^seq(pow2max,pow2min,-1)),2^seq(pow2min,pow2max,1))
    brks <- seq(-pow2max,pow2max,length.out = length(legendticks))
    palette <- c(rev(colorRampPalette(RColorBrewer::brewer.pal(9,colNeg))(length(legendticks)/2-1)),"white",colorRampPalette(RColorBrewer::brewer.pal(9,colPos))(length(legendticks)/2-1))
  }
  data[data<legendticks[1]] <- legendticks[1]
  data[data>legendticks[length(legendticks)]] <- legendticks[length(legendticks)]
  if (eps){
    file=strsplit(file,".",fixed=TRUE)[[1]]
    file=paste(c(file[1:(length(file)-1)],"eps"),collapse=".")
    ps.options(family = c("Helvetica"), pointsize = 18)
    postscript(file,horizontal = FALSE, onefile = FALSE, width=22, height=8.5,paper="special")
  }else{
    png(file, width=7.25, height=3.5, units="in", res=300, pointsize=6,type="cairo")
  }
  ra <- raster::raster(ncols=720, nrows=360)
  range <- range(data)
  ra[raster::cellFromXY(ra,cbind(lon,lat))] <-  data
  extent <- raster::extent(c(-180, 180, -60, 90))
  if (legYes){
    par(bty="n",oma=c(0,0,0,0),mar=c(0,0,0,3),xpd=T)
  }else{
    par(bty="n",oma=c(0,0,0,0),mar=c(0,0,0,0))
  }
  raster::plot(ra,ext=extent,breaks=legendticks,col=palette,main="",legend=FALSE,axes=FALSE)
  title(title,line=-1)
  if (legYes){
    fields::image.plot(legend.only=TRUE,zlim=c(-pow2max,pow2max),col = palette,useRaster=FALSE,breaks=brks,lab.breaks=round(legendticks,2),legend.shrink = 0.8,
                       legend.args=list(legendtitle,side=3, font=2, line=1))
  }
  maps::map('world',add=TRUE,res=0.4, lwd=0.25,ylim=c(-60,90))
  dev.off()
}

#' Plot global LPJmL array
#'
#' Creates a PNG/eps with a plot of a global LPJmL array
#'    Data is plotted linearly in range: c(-max,min,0,min,max)
#'    colors for pos and neg values can be given, default is Blues for the positive
#'    and Reds for the negative numbers
#'    0-range (from -min to min) is white.
#'    The negatives can be omitted by setting onlyPos=T, in case there are only pos values.
#'
#' @param data array with data to plot in LPJmL specific array c(67420)
#' @param file character string for location/file to save plot to
#' @param title character string title for plot
#' @param max upper (positive) end of data range to plot
#' @param min smallest positive number to be distinguished from 0
#' @param colPos color palette for the positives
#' @param colNeg color palette for the negatives
#' @param colrev reverse positive color palette if onlyPos (boolean)
#' @param legendtitle character string legend title
#' @param legYes show legend (boolean)
#' @param onlyPos show only positive (boolean)
#' @param eps write eps file instead of PNG (boolean)
#'
#' @return None
#'
#' @examples
#' plotGlobalWlin(data=irrigation2006,file=paste("~/","mwateramount_2005_06.png",sep=""),
#'             title = paste("irrigation amount 2006 in mm/yr",sep="")max=15,min=0,
#'             legendtitle="legendtitle",legYes=TRUE,eps=FALSE)
#'
#' @export
plotGlobalWlin <- function(data,file,title,max,min,colPos="Blues",colNeg="Reds",colrev=F,legendtitle,legYes,onlyPos=F,eps){
  if (onlyPos){
    lengthbrks <- 16
    if (max-min > 10){
      brks <- round(seq(min,max,length.out = lengthbrks),0)
    }else{
      brks <- round(seq(min,max,length.out = lengthbrks),1)
    }
    if (colrev) palette <- c("white",rev(colorRampPalette(RColorBrewer::brewer.pal(9,colPos))(lengthbrks-2)))
    else palette <- c("white",colorRampPalette(RColorBrewer::brewer.pal(9,colPos))(lengthbrks-2))
  }else{
    lengthbrks <- 24
    if (max-min > 10){
      brks <- round(seq(-max,max,length.out = lengthbrks),0)
    }else{
      brks <- round(seq(-max,max,length.out = lengthbrks),1)
    }
    palette <- c(rev(colorRampPalette(RColorBrewer::brewer.pal(9,colNeg))(lengthbrks/2-1)),"white",colorRampPalette(RColorBrewer::brewer.pal(9,colPos))(lengthbrks/2-1))
  }
  data[data<brks[1]] <- brks[1]
  data[data>brks[length(brks)]] <- brks[length(brks)]
  if (eps){
    file=strsplit(file,".",fixed=TRUE)[[1]]
    file=paste(c(file[1:(length(file)-1)],"eps"),collapse=".")
    ps.options(family = c("Helvetica"), pointsize = 18)
    postscript(file,horizontal = FALSE, onefile = FALSE, width=22, height=8.5,paper="special")
  }else{
    png(file, width=7.25, height=3.5, units="in", res=300, pointsize=6,type="cairo")
  }
  ra <- raster::raster(ncols=720, nrows=360)
  range <- range(data)
  ra[raster::cellFromXY(ra,cbind(lon,lat))] <-  data
  extent <- raster::extent(c(-180, 180, -60, 90))
  if (legYes){
    par(bty="n",oma=c(0,0,0,0),mar=c(0,0,0,3),xpd=T)
  }else{
    par(bty="n",oma=c(0,0,0,0),mar=c(0,0,0,0))
  }
  raster::plot(ra,ext=extent,breaks=brks,col=palette,main="",legend=FALSE,axes=FALSE)
  title(title,line=-1)
  if (legYes){
    fields::image.plot(legend.only=TRUE,zlim=c(-max,max),col = palette,useRaster=FALSE,breaks=brks,lab.breaks=brks,legend.shrink = 0.8,
                       legend.args=list(legendtitle,side=3, font=2, line=1))
  }
  maps::map('world',add=TRUE,res=0.4, lwd=0.25,ylim=c(-60,90))
  dev.off()
}

#' Plot global LPJmL array with weighted sums of the pos and neg values
#'
#' Creates a PNG/eps with a plot of a global LPJmL array
#'    Data is plotted in range: c(-2^pow2max,-2^-pow2min,0,2^-pow2min,2^pow2max)
#'    colors for pos and neg values can be given, default is Blues for the positive
#'    and Reds for the negative numbers
#'    0-range (from 2^-pow2min to 2^pow2min) is white.
#'    Also globally weighted sums for all pos and neg values are plotted.
#'
#' @param data array with data to plot in LPJmL specific array c(67420)
#' @param file character string for location/file to save plot to
#' @param title character string title for plot
#' @param pow2max upper (positive) end of data range to plot (2^pow2max)
#' @param pow2min smallest positive number to be distinguished from 0 (2^-pow2min)
#' @param colPos color palette for the positives
#' @param colNeg color palette for the negatives
#' @param legendtitle character string legend title
#' @param legYes show legend (boolean)
#' @param eps write eps file instead of PNG (boolean)
#'
#' @return None
#'
#' @examples
#' plotGlobalWsum(data=irrigation2006,file=paste("~/","mwateramount_2005_06.png",sep=""),
#'             title = paste("irrigation amount 2006 in mm/yr",sep=""),pow2max=15,pow2min=0,
#'             legendtitle="legendtitle",legYes=TRUE,eps=FALSE)
#'
#' @export
plotGlobalWsum <- function(data,popData=0,file,title,pow2max,pow2min,colPos="Blues",colNeg="Reds",posLab="Pos",negLab="Neg",legendtitle,legYes,eps,pie=T){
  legendticks <- c(-(2^seq(pow2max,pow2min,-1)),2^seq(pow2min,pow2max,1))
  brks <- seq(-pow2max,pow2max,length.out = length(legendticks))
  data[which(!is.finite(data))] <- 0
  if (!length(popData)==1){
    pdata=data*popData
    ptot=sum(pdata[pdata>0])-sum(pdata[pdata<0])
    psump=sum(pdata[pdata>0])/ptot*100
    psumn=-sum(pdata[pdata<0])/ptot*100
    pp=round(cbind(psump,psumn),0)
  }
  gdata=data*cellarea
  gtot=sum(gdata[gdata>0])-sum(gdata[gdata<0])
  gsump=sum(gdata[gdata>0])/gtot*100
  gsumn=-sum(gdata[gdata<0])/gtot*100
  gp=round(cbind(gsump,gsumn),0)
  data[data<legendticks[1]] <- legendticks[1]
  data[data>legendticks[length(legendticks)]] <- legendticks[length(legendticks)]
  palette <- c(rev(colorRampPalette(RColorBrewer::brewer.pal(9,colNeg))(length(legendticks)/2-1)),"white",colorRampPalette(RColorBrewer::brewer.pal(9,colPos))(length(legendticks)/2-1))
  if (eps){
    file=strsplit(file,".",fixed=TRUE)[[1]]
    file=paste(c(file[1:(length(file)-1)],"eps"),collapse=".")
    ps.options(family = c("Helvetica"), pointsize = 18)
    postscript(file,horizontal = FALSE, onefile = FALSE, width=22, height=8.5,paper="special")
  }else{
    png(file, width=7.25, height=3.5, units="in", res=300, pointsize=6,type="cairo")
  }
  ra <- raster::raster(ncols=720, nrows=360)
  range <- range(data)
  ra[raster::cellFromXY(ra,cbind(lon,lat))] <-  data
  extent <- raster::extent(c(-180, 180, -60, 90))
  modLegendTicks=seq(0,length(legendticks)-1,1)
  if (legYes){
    par(bty="n",oma=c(0,0,0,0),mar=c(0,0,0,3),xpd=T)
  }else{
    par(bty="n",oma=c(0,0,0,0),mar=c(0,0,0,0))
  }
  raster::plot(ra,ext=extent,breaks=legendticks,col=palette,main="",legend=FALSE,axes=FALSE)
  title(title,line=-1)
  if (legYes){
    fields::image.plot(legend.only=TRUE,zlim=c(-pow2max,pow2max),col = palette,useRaster=FALSE,breaks=brks,lab.breaks=round(legendticks,2),legend.shrink = 0.8,
                       legend.args=list(legendtitle,side=3, font=2, line=1))
  }
  maps::map('world',add=TRUE,res=0.4, lwd=0.25,ylim=c(-60,90))
  dev.off()
  file=strsplit(file,".",fixed=TRUE)[[1]]
  if (eps){
    file=paste(paste(c(file[1:(length(file)-1)]),collapse="."),"_legend.eps",sep="")
    ps.options(family = c("Helvetica"), pointsize = 18)
    postscript(file,horizontal = FALSE, onefile = FALSE, width=5, height=5,paper="special")
  }else{
    file=paste(paste(c(file[1:(length(file)-1)]),collapse="."),"_legend.png",sep="")
    png(file, width=2.5, height=2.5, units="in",bg = "transparent",res=300, pointsize=6,type="cairo")
  }
  par(oma=c(0,0,0,0),mar=c(4,4,4,4),xpd=T)
  if (pie){
    #par(fig=c(0,0.2,0,0.2),oma=c(0,0,0,0),mar=c(0,0,0,0))
    pie(x = gp,labels = paste(gp,"%",sep=""),col = palette[c(length(palette)-3,4)],cex=2)
  }else{
    plot(0,0,xlim=c(-205,-120),ylim=c(-60,25),xaxt="n",type="n",yaxt="n")
    legend(-200,20,c("Area weighted:",paste(c(paste(posLab,": ",sep=""),paste(negLab,": ",sep="")),gp,"%",sep="")),text.col=c("black",palette[c(length(palette),1)]),pt.cex=3,lty=0,lwd=1,cex = 1.5,bty = "n")
    if (!length(popData)==1){
      legend(-200,-10,c("Population weighted:",paste(c(paste(posLab,": ",sep=""),paste(negLab,": ",sep="")),pp,"%",sep="")),text.col=c("black",palette[c(length(palette),1)]),pt.cex=3,lty=0,lwd=1,cex = 1.5,bty = "n")
    }
  }
  dev.off()

}



#' Plot global LPJmL array inside RStudio
#'
#' Plot of a global LPJmL array inside RStudio
#'    Data is plotted in range: c(-2^pow2max,-2^-pow2min,0,2^-pow2min,2^pow2max)
#'    where the positive values are colored green to blue,
#'    0-range is white,
#'    and the negative ones red to yellow
#'
#' @param data array with data to plot in LPJmL specific array c(67420)
#' @param title character string title for plot
#' @param pow2max upper (positive) end of data range to plot (2^pow2max)
#' @param pow2min smallest positive number to be distinguished from 0 (2^-pow2min)
#' @param legendtitle character string legend title
#' @param legYes show legend (boolean)
#'
#' @return None
#
#' @examples
#' plotGlobalWtoscreen(data=irrigation2006,title = paste("irrigation amount 2006 in mm/yr",sep=""),
#'                     pow2max=15,pow2min=0,"legendtitle",legYes=TRUE)
#'
#' @export
plotGlobalWtoscreen <- function(data,title,pow2max,pow2min,legendtitle,legYes){
  legendticks <- c(-(2^seq(pow2max,-pow2min,-1)),2^seq(-pow2min,pow2max,1))
  brks <- seq(-pow2max,pow2max,length.out = length(legendticks))
  data[data<legendticks[1]] <- legendticks[1]
  data[data>legendticks[length(legendticks)]] <- legendticks[length(legendticks)]
  palette <- c(rev(colorRampPalette(RColorBrewer::brewer.pal(9,"YlOrRd"))(pow2max+pow2min)),"white",colorRampPalette(RColorBrewer::brewer.pal(9,"GnBu"))(pow2max+pow2min))
  ra <- raster::raster(ncols=720, nrows=360)
  range <- range(data)
  ra[raster::cellFromXY(ra,cbind(lon,lat))] <-  data
  extent <- raster::extent(c(-180, 180, -60, 90))
  modLegendTicks=seq(0,length(legendticks)-1,1)
  par(mar=c(0,0,0,3),oma=c(0,0,0,0),bty="n")
  raster::plot(ra,ext=extent,breaks=legendticks,col=palette,main=title,legend=FALSE,axes=FALSE)
  if (legYes){
    legendtitle=""
    fields::image.plot(legend.only=TRUE,zlim=c(-pow2max,pow2max),col = palette, breaks=brks,lab.breaks=legendticks,legend.shrink = 0.7,
                       legend.args=list(legendtitle,side=4, font=2, line=2.5))
  }
  maps::map('world',add=TRUE,res=0.4, lwd=0.25,ylim=c(-60,90))
}

#' Plot global LPJmL array with only positive values
#'
#' Creates a PNG/eps with a plot of a global LPJmL array
#'    Data is plotted in range: c(0,2^pow2max)
#'    positive values are colored accoding to chosen palette,
#'    0-range is white,
#'
#' @param data array with data to plot in LPJmL specific array c(67420)
#' @param file character string for location/file to save plot to
#' @param title character string title for plot
#' @param pow2max upper (positive) end of data range to plot (2^pow2max)
#' @param col color palette name from brewer.pal (character string)
#' @param legendtitle character string legend title
#' @param legYes show legend (boolean)
#' @param eps write eps file instead of PNG (boolean)
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' plotGlobal_pos(data=irrigation2006,file=paste("~/","mwateramount_2005_06.png",sep=""),
#'             title = paste("irrigation amount 2006 in mm/yr",sep=""),pow2max=15,col="GnBu",
#'             legendtitle="legendtitle",legYes=TRUE,eps=FALSE)
#' }
#' @export
plotGlobal_pos <- function(data,file,title,pow2max,col,legendtitle,legYes,eps){
  legendticks <- c(0,2^seq(0,(pow2max+1),1))
  brks <- seq(0,(pow2max+1),length.out = length(legendticks))
  data[data>legendticks[length(legendticks)]] <- legendticks[length(legendticks)]
  palette <- c("white",colorRampPalette(brewer.pal(9,col))(length(legendticks)-2))
  if (eps){
    file=strsplit(file,".",fixed=T)[[1]]
    file=paste(c(file[1:(length(file)-1)],"eps"),collapse=".")
    ps.options(family = c("Helvetica"), pointsize = 18)
    postscript(file,horizontal = FALSE, onefile = FALSE, width=22, height=8.5,paper="special")
  }else{
    png(file, width=7.25, height=3.5, units="in", res=300, pointsize=6,type="cairo")
  }
  ra <- raster(ncols=720, nrows=360)
  range <- range(data)
  ra[cellFromXY(ra,cbind(lon,lat))] <-  data
  extent <- extent(c(-180, 180, -60, 90))
  if (legYes){
    par(bty="n",oma=c(0,0,0,0),mar=c(0,0,0,3),xpd=T)
  }else{
    par(bty="n",oma=c(0,0,0,0),mar=c(0,0,0,0))
  }
  plot(ra,ext=extent,breaks=legendticks,col=palette,main=title,legend=F,axes=F)
  if (legYes){
    image.plot(legend.only=T,zlim=c(0,pow2max+1),col = palette,useRaster=F,breaks=brks,legend.shrink = 0.8,
               #axis.args=list(at=brks,labels=legendticks),
               legend.args=list(legendtitle,side=3, font=2, line=1),axis.args=list(at=brks[1:(length(brks)-1)],labels=legendticks[1:(length(legendticks)-1)]))
  }
  map('world',add=T,res=0.4, lwd=0.25)
  dev.off()
}

#' Plot global LPJmL array with positive and negative values
#'
#' Creates a PNG/eps with a plot of a global LPJmL array
#'    Data is plotted in range: c(-2^negpow2max,2^pow2max)
#'    positive and negative values are colored accoding to chosen palettes,
#'    0-range is white,
#'
#' @param data array with data to plot in LPJmL specific array c(67420)
#' @param file character string for location/file to save plot to
#' @param title character string title for plot
#' @param negpow2max lower (negative) end of data range to plot (-2^negpow2max)
#' @param pow2max upper (positive) end of data range to plot (2^pow2max)
#' @param poscol color palette name from brewer.pal (character string) for positive values
#' @param negcol color palette name from brewer.pal (character string) for negative values
#' @param legendtitle character string legend title
#' @param legYes show legend (boolean)
#' @param eps write eps file instead of PNG (boolean)
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' plotGlobal_pos_neg(data=EFR_deficits2006,file=paste("~/","EFRdeficits_06.png",sep=""),
#'             title = "",negpow2max=10, pow2max=15,negcol="GnBu",poscol="Reds"
#'             legendtitle="legendtitle",legYes=TRUE,eps=FALSE)
#' }
#' @export
plotGlobal_pos_neg <- function(data,file,title,pow2max,negpow2max,negcol, poscol,legendtitle,legYes,eps){
  legendticks <- c(-(2^seq(negpow2max+1,0,-1)),2^seq(0,pow2max+1,1))
  brks <- seq(-(negpow2max+1),(pow2max+1),length.out = length(legendticks))
  data[data<legendticks[1]] <- legendticks[1]
  data[data>legendticks[length(legendticks)]] <- legendticks[length(legendticks)]
  if (pow2max<negpow2max){
    palette  <- c(colorRampPalette(rev(RColorBrewer::brewer.pal(9,negcol)[1:9]))(length(seq(negpow2max+1,0,-1))-1),"white",colorRampPalette(RColorBrewer::brewer.pal(9,poscol)[1:9])(length(seq((negpow2max+1),0,-1))-1)[1:(pow2max+1)])
  } else if (pow2max>negpow2max){
    palette  <- c(colorRampPalette(rev(RColorBrewer::brewer.pal(9,negcol)[1:9]))(length(seq(0,pow2max+1,1))-1)[(length(seq(0,pow2max+1,1))-(negpow2max+1)):(length(seq(0,pow2max+1,1))-1)],"white",colorRampPalette(RColorBrewer::brewer.pal(9,poscol)[1:9])(length(seq(0,pow2max+1,1))-1))
  } else {
    palette  <- c(colorRampPalette(rev(RColorBrewer::brewer.pal(9,negcol)[1:9]))(length(seq(negpow2max+1,0,-1))-1),"white",colorRampPalette(RColorBrewer::brewer.pal(9,poscol)[1:9])(length(seq(0,pow2max+1,1))-1))
  }
  if (eps){
    file=strsplit(file,".",fixed=T)[[1]]
    file=paste(c(file[1:(length(file)-1)],"eps"),collapse=".")
    ps.options(family = c("Helvetica"), pointsize = 18)
    postscript(file,horizontal = FALSE, onefile = FALSE, width=22, height=8.5,paper="special")
  }else{
    png(file, width=7.25, height=3.5, units="in", res=300, pointsize=6,type="cairo")
  }
  ra <- raster(ncols=720, nrows=360)
  range <- range(data)
  ra[cellFromXY(ra,cbind(lon,lat))] <-  data
  extent <- extent(c(-180, 180, -60, 90))
  if (legYes){
    par(bty="n",oma=c(0,0,0,0),mar=c(0,0,0,3),xpd=T)
  }else{
    par(bty="n",oma=c(0,0,0,0),mar=c(0,0,0,0))
  }
  plot(ra,ext=extent,breaks=legendticks,col=palette,main="",legend=F,axes=F)
  title(title, line = -1)
  if (legYes){
    image.plot(legend.only=T,zlim=c(-(negpow2max+1),pow2max),col = palette,useRaster=F,breaks=brks,legend.shrink = 0.8,
               axis.args=list(at=brks[2:(length(brks)-1)],labels=legendticks[2:(length(legendticks)-1)]),
               legend.args=list(legendtitle,side=3, font=2, line=1))
  }
  map('world',add=T,res=0.4, lwd=0.25)
  dev.off()
}

#' Plot global LPJmL CFT array inside RStudio
#'
#' Plot of one band of a global LPJmL cft array inside RStudio
#'            with classification into 10% classes.
#'
#' @param lushares array with cft band data to plot in LPJmL specific array c(67420)
#' @param title character string title for plot
#' @param legendtitle character string legend title
#'
#' @return None
#
#' @examples
#' \dontrun{
#' plotLUsharesToScreen(lushares=cftfracs2005[,3],title = "cft fracs for maize",legendtitle="")
#' }
#'
#' @export

plotLUsharesToScreen <- function(lushares,title,legendtitle){
  par(mar=c(4, 0, 4, 15) + 0.1,oma=c(0,0,0,2))
  par(cex=1.2,cex.main=1.2)
  cols  <- c(RColorBrewer::brewer.pal(10,"RdYlBu"),"white")
  range <- c(0,1)
  brk   <- c(0,0.001,seq(0,1,0.1)[2:length(seq(0,1,0.1))])
  par(cex=1.2,cex.main=1.2)
  lu.ras <- raster::raster(ncols=720, nrows=360)
  lu.ras[raster::cellFromXY(lu.ras,cbind(lon,lat))] <-  1-lushares
  raster::plot(lu.ras,ylim=c(-60,90),xlim=c(-180,180),zlim=range,breaks=brk,col=(cols),legend=F,xaxt="n",yaxt="n",main=title,axes=F,box=F)
  maps::map('world',add=TRUE,res=0.4, lwd=0.25,ylim=c(-60,90))  #Legend
  par(xpd=T)
  legend(190,70,title=legendtitle,cex=1.6,
         rev(c("0","0-10%","11-20%","21-30%","31-40%","41-50%","51-60%","61-70%","71-80%","81-90%","91-100%")),
         fill=(cols), horiz=F,border=NULL,bty="o",box.col="white",bg="white",ncol=1)
  text(190,70,pos=4,"landuse intensity",cex=1.6)
  par(xpd=F)
}

#' Plot global LPJmL CFT array
#'
#' Plot of one band of a global LPJmL cft array
#'             classification into 10% classes.
#'
#' @param file character string for location/file to save plot to
#' @param lushares array with cft band data to plot in LPJmL specific array c(67420)
#' @param title character string title for plot
#' @param legendtitle character string legend title
#' @param eps write eps file instead of PNG (boolean)
#'
#' @return None
#
#' @examples
#' \dontrun{
#' plotLUshares(file="~/cftfracs2005_maize.png",lushares=cftfracs2005[,3],
#'              title = "cft fracs for maize",legendtitle="",eps=FALSE)
#' }
#'
#' @export
plotLUshares <- function(file,lushares,title,legendtitle,eps){
  if (eps){
    file=strsplit(file,".",fixed=T)[[1]]
    file=paste(c(file[1:(length(file)-1)],"eps"),collapse=".")
    ps.options(family = c("Helvetica"), pointsize = 18)
    postscript(file,horizontal = FALSE, onefile = FALSE, width=20, height=10,paper="special")
  }else{
    png(file, width=7.2, height=3.6, units="in", res=300, pointsize=6,type="cairo")
  }
  par(mar=c(4, 0, 4, 15) + 0.1,oma=c(0,0,0,2))
  par(cex=1.2,cex.main=1.2)
  cols  <- c(RColorBrewer::brewer.pal(10,"RdYlBu"),"white")
  range <- c(0,1)
  brk   <- c(0,0.001,seq(0,1,0.1)[2:length(seq(0,1,0.1))])
  par(cex=1.2,cex.main=1.2)
  lu.ras <- raster::raster(ncols=720, nrows=360)
  lu.ras[raster::cellFromXY(lu.ras,cbind(lon,lat))] <-  1-lushares
  raster::plot(lu.ras,ylim=c(-60,90),xlim=c(-180,180),zlim=range,breaks=brk,col=(cols),legend=F,xaxt="n",yaxt="n",main=title,axes=F,box=F)
  maps::map('world',add=TRUE,res=0.4, lwd=0.25,ylim=c(-60,90))  #Legend
  par(xpd=T)
  legend(190,70,title=legendtitle,cex=1.6,
         rev(c("0","0-10%","11-20%","21-30%","31-40%","41-50%","51-60%","61-70%","71-80%","81-90%","91-100%")),
         fill=(cols), horiz=F,border=NULL,bty="o",box.col="white",bg="white",ncol=1)
  text(190,70,pos=4,"landuse intensity",cex=1.6)
  par(xpd=F)
  dev.off()
}

#' Plot global LPJmL CFT array
#'
#' Plot of one band of a global LPJmL cft array
#'             classification into 10% classes, except for first class: 0-1%, 1-10%,
#'             colors according to chosen palette
#'
#' @param file character string for location/file to save plot to
#' @param lushares array with cft band data to plot in LPJmL specific array c(67420)
#' @param title character string title for plot
#' @param col color palette name from brewer.pal (character string)
#' @param legendtitle character string legend title
#' @param eps write eps file instead of PNG (boolean)
#'
#' @return None
#
#' @examples
#' \dontrun{
#' plotLUshares2(file="~/cftfracs2005_maize.png",lushares=cftfracs2005[,3],
#'              title = "cft fracs for maize",col="Reds",legendtitle="",eps=FALSE)
#' }
#'
#' @export

plotLUshares2<- function(file,lushares,title, col, legendtitle, eps){
  if (eps){
    file=strsplit(file,".",fixed=T)[[1]]
    file=paste(c(file[1:(length(file)-1)],"eps"),collapse=".")
    ps.options(family = c("Helvetica"), pointsize = 18)
    postscript(file,horizontal = FALSE, onefile = FALSE, width=20, height=10,paper="special")
  }else{
    png(file, width=7.2, height=3.6, units="in", res=300, pointsize=6,type="cairo")
  }
  par(cex=1.2,cex.main=1.2)
  cols  <- c("white",RColorBrewer::brewer.pal(9,col)[2],colorRampPalette(RColorBrewer::brewer.pal(9,col)[3:9])(10))
  range <- c(0,1)
  brk   <- c(0,0.0001,0.01,seq(0,1,0.1)[2:length(seq(0,1,0.1))])
  lu.ras <- raster::raster(ncols=720, nrows=360)
  lu.ras[raster::cellFromXY(lu.ras,cbind(lon,lat))] <-  lushares
  extent <- raster::extent(c(-180, 180, -60, 90))
  legend.breaks<- c(0,0.05,seq(0.05,1,0.095)[2:length(seq(0.05,1,0.095))]) # damit auch 0 bis 1 in der Legende sichtbar ist
  legend.cols <-c(RColorBrewer::brewer.pal(9,col)[2],colorRampPalette(RColorBrewer::brewer.pal(9,col)[3:9])(10))
  par(bty="n",oma=c(0,0,0,0),mar=c(0,0,0,3),xpd=T)
  raster::plot(lu.ras,ext=extent,breaks=brk,col=(cols),legend=F,main=title,axes=F,box=F)
  maps::map('world',add=TRUE,res=0.4, lwd=0.25,ylim=c(-60,90))  #Legend
  fields::image.plot(legend.only=TRUE,zlim=range,col = legend.cols,useRaster=FALSE,breaks=legend.breaks,lab.breaks=c("0","1 %","10 %","20 %","30 %","40 %","50 %","60 %","70 %","80 %","90 %","100 %"),legend.shrink = 0.8,
                     legend.args=list(legendtitle,side=3, font=2, line=1))
  dev.off()
}

#' Plot bioenergy harvest over time
#'
#' Plot bioenergy harvest over time
#'
#' @param file character string for location/file to save plot to
#' @param beHarvest array yearly beharvest data -- array c(years,type,clim,rf:irr)
#' @param title character string title for plot
#' @param eps write eps file instead of PNG (boolean)
#'
#' @return None
#
#' @examples
#' \dontrun{
#' plotBEharvest(file="beHarvest.png",beHarvest=beHarvestYearly,
#'              title = "",eps=FALSE)
#' }
#'
#' @export
plotBEharvest <- function(file,beHarv,beHarv_cal,title,expFormat="png"){
  if (expFormat=="eps"){
    file=strsplit(file,".",fixed=TRUE)[[1]]
    file=paste(c(file[1:(length(file)-1)],"eps"),collapse=".")
    ps.options(family = c("Helvetica"), pointsize = 18)
    postscript(file,horizontal = FALSE, onefile = FALSE, width=18, height=18,paper="special")
  }else if (expFormat=="pdf"){
    file=strsplit(file,".",fixed=TRUE)[[1]]
    file=paste(c(file[1:(length(file)-1)],"pdf"),collapse=".")
    pdf(file,width=14.4,height=7.2,family = c("Helvetica"),pointsize = 12,paper='special',version = "1.5")
  }else{
    png(file, width=7.2, height=3.6, units="in", res=500, pointsize=6,type="cairo")
  }
  par(fig=c(0,0.9,0,1),oma=par("oma")+c(0,0.1,0,0))
  plot(0,type="n", xlim=c(2006,2099), ylim=c(0,11.5),xlab="Year", ylab="harvest [GtC]",main=title,cex.axis=1.5,cex.lab=1.5,xaxs = "i", yaxs = "i")
  grid(ny=NULL,nx=NULL)
  cols=rep("white",12)
  ind=c(5,11,1,12,6,8,18,10,16,13,17,14,15)
  cols[ind]=alpha(c("gray30","orange","orangered","red3","red4","lawngreen","yellowgreen","limegreen","green4","turquoise1","steelblue1","royalblue","royalblue4"),0.7)#RColorBrewer::brewer.pal(10,"Paired")[c(7,8,5,6,3,4,9,1,2,10)])
  ltys=c("solid", "dashed", "dotted", "longdash")#, "dotdash")
  clims=c("HadGEM","MIROC5","GFDL","IPSL")
  scens=c("baseline 0","noefr 15","noefr 30","noefr 45","noefr 60","efr 30","efr 45","efr 60","efr 90","efrwm 30","efrwm 45","efrwm 60","efrwm 90")
  #plot sums
  ytop=11.2
  xpos=seq(2011,2046,length.out = 6)
  rect(xleft = 2006,ybottom = 3.9,xright = xpos[length(xpos)]+2,ytop = ytop+0.3,col = "white")
  text(x=xpos,y=ytop,labels=c("Sum [GtC]",c(clims,"mean")),cex=0.9)
  text(x=xpos,y=ytop-1*0.5,labels=c("ISIMIP2b demand",round(c(colSums(apply(beHarv_cal[,ind[1],1:4,],c(1,2),sum)),mean(colSums(apply(beHarv_cal[,ind[1],1:4,],c(1,2),sum)))),0)),cex=0.9)
  for (i in 1:(length(beHarv[1,,1,1])-5)){
    text(x=xpos,y=ytop-(i+1)*0.5,labels=c(scens[i],round(c(colSums(apply(beHarv[,ind[i],1:4,],c(1,2),sum)),mean(colSums(apply(beHarv[,ind[i],1:4,],c(1,2),sum)))),0)),cex=0.9)
  }
  #plot lines
  for (t in ind[1:(length(beHarv[1,,1,1])-5)]){
    for (c in 1:4){
      lines(x=2006:2099,y=rowSums(beHarv[,t,c,]),col=cols[t],lty=ltys[c])
    }
  }
  colb=alpha("black",0.7)
  for (c in 1:4){
    lines(x=2006:2099,y=rowSums(beHarv_cal[,5,c,]),col=colb,lty=ltys[c])
  }
  legend(2085,5.4,title="scenario",legend = c("ISIMIP2b demand",scens[1:(length(beHarv[1,,1,1])-5)]),col=c(colb,cols[ind]), lty="solid",cex=0.8)
  legend(2067,4,title="climate",legend = clims,col="black", lty=ltys,cex=0.8)
  par(fig=c(0.9,1,0.0,1), mar=c(5.1, 0, 4.1, 0),new=TRUE,xpd=T)
  plot(0,type="n", xlim=c(0,1), ylim=c(0,100),xlab="", ylab="",main="",cex.axis=1.5,cex.lab=1.5,axes=F)
  # plot new prod. increase axis and labels
  ys=c(mean(apply(beHarv_cal[,5,1:4,],c(2),sum)),rowMeans(apply(beHarv[,ind,1:4,],c(2,3),sum)))
  yp=round((ys-ys[2])/(ys[1]-ys[2])*100)
  #lines(x=rep(0.2,2),y=yp[1:2],col="black",lwd=0.7)
  text(x=0.65,y=yp,labels=paste(yp,"%"),adj=1,cex=1)
  text(x=0.85,y=(yp[1]+yp[2])/2,labels="total productivity increase",srt=90,cex=1)
  #plot means
  b=yp[1]
  yp=yp[2:length(yp)]
  for (i in 1:(length(beHarv[1,,1,1])-5)){
    xs=c(0.05,0.25)
    #if (i==11 || i==9){xs=c(0,0.15)}
    #else if(i==3 || i==4){xs=c(0.15,0.3)}
    lines(x=xs,y=rep(yp[i],2),col=cols[ind[i]],lwd=1.5,xaxt="n")
  }
  lines(x=c(0.05,0.25),y=rep(b,2),col=colb,lwd=1.5,xaxt="n")
  #lines(x=c(0,0.2),y=rep(mean(rowSums(beHarv_cal[94,5,,])),2),col=colb,lwd=1.5)
  dev.off()
}

#' Plot global relative differences
#'
#' Plot of relative differences derived from two LPJmL arrays
#'             classification into 10% classes
#'
#' @param file character string for location/file to save plot to
#' @param diff_data array with relative differences in LPJmL specific array c(67420)
#' @param min lower end of data range (between -1.5 and 0, with one digit allowed to the left of the decimal point); one additional break is added to display values lower than min
#' @param max upper end of data range (between 0 and 1.5, with one digit allowed to the left of the decimal point); one additional break is added to display values higher than max
#' @param title character string title for plot
#' @param poscol color palette name from brewer.pal (character string) for poitive values
#' @param negcol color palette name from brewer.pal (character string) for negative values
#' @param legendtitle character string legend title
#'
#' @return None
#
#' @examples
#' \dontrun{
#' plot_relDiff(file="~/difference_in_soil_carbon_2005_2006.png",diff_data=diff_soilc_2005_2006, min=-0.5, max=0.7, negcol="Reds", poscol="Blues",
#'              title = "relative difference in soil carbon between 2005 and 2006",legendtitle="diff. in %",eps=FALSE)
#' }
#'
#' @export
plot_relDiff <- function(file,diff_data, min, max, negcol, poscol, title,legendtitle){
  png(file, width=7.2, height=3.6, units="in", res=300, pointsize=6,type="cairo")
  par(cex=1.2,cex.main=1.2)
  if (max<abs(min)&&max!=0){
    cols  <- c(colorRampPalette(rev(RColorBrewer::brewer.pal(9,negcol)[3:9]))(length(seq(min-0.1,0,0.1))-1),"white",colorRampPalette(RColorBrewer::brewer.pal(9,poscol)[3:9])(length(seq(min-0.1,0,0.1))-1)[1:((max+0.1)*10)])
    legend.cols<-c(colorRampPalette(rev(RColorBrewer::brewer.pal(9,negcol)[3:9]))(length(seq(min-0.1,0,0.1))-1),colorRampPalette(RColorBrewer::brewer.pal(9,poscol)[3:9])(length(seq(min-0.1,0,0.1))-1)[1:((max+0.1)*10)])
  } else if (max>abs(min)&&min!=0){
    cols  <- c(colorRampPalette(rev(RColorBrewer::brewer.pal(9,negcol)[3:9]))(length(seq(0,max+0.1,0.1))-1)[((length(seq(0,max+0.1,0.1))-1)-(abs(min-0.1)*10)+1):(length(seq(0,max+0.1,0.1))-1)],"white",colorRampPalette(RColorBrewer::brewer.pal(9,poscol)[3:9])(length(seq(0,max+0.1,0.1))-1))
    legend.cols<-c(colorRampPalette(rev(RColorBrewer::brewer.pal(9,negcol)[3:9]))(length(seq(0,max+0.1,0.1))-1)[((length(seq(0,max+0.1,0.1))-1)-(abs(min-0.1)*10)+1):(length(seq(0,max+0.1,0.1))-1)],colorRampPalette(RColorBrewer::brewer.pal(9,poscol)[3:9])(length(seq(0,max+0.1,0.1))-1))
  } else if (max==0){
    cols  <- c(colorRampPalette(rev(RColorBrewer::brewer.pal(9,negcol)[3:9]))(length(seq(min-0.1,0,0.1))-1),"white",RColorBrewer::brewer.pal(3,poscol)[3])
    legend.cols<-c(colorRampPalette(rev(RColorBrewer::brewer.pal(9,negcol)[3:9]))(length(seq(min-0.1,0,0.1))-1),RColorBrewer::brewer.pal(3,poscol)[3])
  } else if (min==0){
    cols  <- c(RColorBrewer::brewer.pal(3,negcol)[3],"white",colorRampPalette(RColorBrewer::brewer.pal(9,poscol)[3:9])(length(seq(0,max+0.1,0.1))-1))
    legend.cols<-c(RColorBrewer::brewer.pal(3,negcol)[3],colorRampPalette(RColorBrewer::brewer.pal(9,poscol)[3:9])(length(seq(0,max+0.1,0.1))-1))
  } else {
    cols  <- c(colorRampPalette(rev(RColorBrewer::brewer.pal(9,negcol)[3:9]))(length(seq(min-0.1,0,0.1))-1),"white",colorRampPalette(RColorBrewer::brewer.pal(9,poscol)[3:9])(length(seq(0,max+0.1,0.1))-1))
    legend.cols<-c(colorRampPalette(rev(RColorBrewer::brewer.pal(9,negcol)[3:9]))(length(seq(min-0.1,0,0.1))-1),colorRampPalette(RColorBrewer::brewer.pal(9,poscol)[3:9])(length(seq(min-0.1,0,0.1))-1))
  }
  range <- c(min-0.1,max+0.1)
  brk   <- c(seq(min-0.1,0,0.1)[1:(length(seq(min-0.1,0,0.1))-1)],-0.0001,0.0001,seq(0,max+0.1,0.1)[2:length(seq(0,max+0.1,0.1))])
  diff_data[diff_data<brk[1]] <- brk[1] #smaller values than min also displayed
  diff_data[diff_data>brk[length(brk)]] <- brk[length(brk)] #higher values than max also displayed
  lu.ras <- raster::raster(ncols=720, nrows=360)
  lu.ras[raster::cellFromXY(lu.ras,cbind(lon,lat))] <-  diff_data
  extent <- raster::extent(c(-180, 180, -60, 90))
  if(max==0){
    legendlabel<-c(as.character(c(paste(round(brk*100,0),"%",sep=" ")))[2:(length(seq(min-0.1,0,0.1))-1)],0)
  }
  else if (min==0){
    legendlabel<-c(0, as.character(c(paste(round(brk*100),"%",sep=" ")))[(length(brk)-length(seq(0,max+0.1,0.1))+2):(length(brk)-1)])
  } else {
    legendlabel<-c(as.character(c(paste(round(brk*100,0),"%",sep=" ")))[2:(length(seq(min-0.1,0,0.1))-1)],0, as.character(c(paste(round(brk*100),"%",sep=" ")))[(length(brk)-length(seq(0,max+0.1,0.1))+2):(length(brk)-1)])
  }
  legend.breaks<- c(seq(min-0.1,-0.1,0.1),0,seq(0.1,max+0.1,0.1))
  par(bty="n",oma=c(0,0,0,0),mar=c(0,0,0,3),xpd=T)
  raster::plot(lu.ras,ext=extent,zlim=range,breaks=brk,col=(cols),legend=F,xaxt="n",yaxt="n",main=title,axes=F,box=F)
  maps::map('world',add=TRUE,res=0.4, lwd=0.25,ylim=c(-60,90))  #Legend
  fields::image.plot(legend.only=TRUE,zlim=range,col = legend.cols,useRaster=FALSE,breaks=legend.breaks,legend.shrink = 0.8,
                     legend.args=list(legendtitle,side=3, font=2, line=1),axis.args=list(at=legend.breaks[2:(length(legend.breaks)-1)],labels=legendlabel))
  dev.off()
}

#' Copy of the pie function with labels further away from pie
#'
#' Copy of the pie function with labels further away from pie
#'
#' @export
pie2 <- function (x, labels = names(x), edges = 200, radius = 0.8, clockwise = FALSE,
          init.angle = if (clockwise) 90 else 0, density = NULL, angle = 45,
          col = NULL, border = NULL, lty = NULL, main = NULL, ...)
{
  if (!is.numeric(x) || any(is.na(x) | x < 0))
    stop("'x' values must be positive.")
  if (is.null(labels))
    labels <- as.character(seq_along(x))
  else labels <- as.graphicsAnnot(labels)
  x <- c(0, cumsum(x)/sum(x))
  dx <- diff(x)
  nx <- length(dx)
  plot.new()
  pin <- par("pin")
  xlim <- ylim <- c(-1, 1)
  if (pin[1L] > pin[2L])
    xlim <- (pin[1L]/pin[2L]) * xlim
  else ylim <- (pin[2L]/pin[1L]) * ylim
  dev.hold()
  on.exit(dev.flush())
  plot.window(xlim, ylim, "", asp = 1)
  if (is.null(col))
    col <- if (is.null(density))
      c("white", "lightblue", "mistyrose", "lightcyan",
        "lavender", "cornsilk")
  else par("fg")
  if (!is.null(col))
    col <- rep_len(col, nx)
  if (!is.null(border))
    border <- rep_len(border, nx)
  if (!is.null(lty))
    lty <- rep_len(lty, nx)
  angle <- rep(angle, nx)
  if (!is.null(density))
    density <- rep_len(density, nx)
  twopi <- if (clockwise)
    -2 * pi
  else 2 * pi
  t2xy <- function(t) {
    t2p <- twopi * t + init.angle * pi/180
    list(x = radius * cos(t2p), y = radius * sin(t2p))
  }
  for (i in 1L:nx) {
    n <- max(2, floor(edges * dx[i]))
    P <- t2xy(seq.int(x[i], x[i + 1], length.out = n))
    polygon(c(P$x, 0), c(P$y, 0), density = density[i], angle = angle[i],
            border = border[i], col = col[i], lty = lty[i])
    P <- t2xy(mean(x[i + 0:1]))
    lab <- as.character(labels[i])
    if (!is.na(lab) && nzchar(lab)) {
      lines(c(1, 1.05) * P$x, c(1, 1.05) * P$y)
      text(1.1 * P$x, 1.1 * P$y, labels[i], xpd = TRUE,
           adj = ifelse(P$x < 0, 1, 0), ...)
    }
  }
  title(main = main, ...)
  invisible(NULL)
}
