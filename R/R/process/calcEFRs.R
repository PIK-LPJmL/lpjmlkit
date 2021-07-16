#' Calculate environmental flow requirements (EFRs)
#'
#' Calculate environmental flow requirements (EFRs), based on 30 years of monthly discharge input
#'
#' @param discharge 30 years monthly discharge array dim(discharge)=c(ncells,12,30)
#' @param method EFR method to be used ("VMF","Q90Q50","PBpaper"), "PBpaper" is the modified version of VMF used for the Steffen et al. 2015 PB paper
#'
#' @return EFRs as volumes like in discharge input, dim(EFRs)=c(ncells,12)
#'
#' @examples
#' \dontrun{
#' calcEFRs(discharge=mdischarge_preindustrial,method="VMF")
#' }
#'
#' @export
calcEFRs <- function(discharge,method="VMF"){
  ncells=dim(discharge)[1]
  efrMethodsSet = c("PBpaper","VMF","Q90Q50")
  #method valid?
  if (!method %in% efrMethodsSet){
    print(paste("EFR method not recognized, use one of: ",paste(efrMethodsSet,collapse = " ")))
    return(NA)
  }
  #make sure discharge is a ncells,12month,30year array
  if (!all(dim(discharge) == c(ncells,12,30))){
    print(paste("discharge array has wrong dimension, use c(ncells,nmonths=12,nyears=30)"))
    return(NA)
  }
  efrs=discharge[,,1]*0 #initialize efrs array

  #calculate mean monthly flow and mean annual flow
  MMF=apply(discharge,c(1,2),mean)
  quantiles=apply(discharge,c(1,2),quantile,probs=c(0.5,0.9),na.rm=T)
  MAF=rep(rowMeans(MMF),times=12)
  dim(MAF)=c(ncells,12)
  Q90=quantiles[2,,]
  Q50=quantiles[1,,]
  remove(quantiles)

  if (method==efrMethodsSet[1]){ #"PBpaper" - Steffen et al. 2015
    efrs[MMF<=0.4*MAF]=0.75*MMF[MMF<=0.4*MAF] # low flow months
    efrs[MMF>0.4*MAF & MMF<=0.8*MAF]=0.7*MMF[MMF>0.4*MAF & MMF<=0.8*MAF] # intermediate flow months
    efrs[MMF>0.8*MAF]=0.45*MMF[MMF>0.8*MAF] # high flow months
  }else if (method==efrMethodsSet[2]){ # "VMF" - Pastor et al. 2014
    efrs[MMF<=0.4*MAF]=0.6*MMF[MMF<=0.4*MAF] # low flow months
    efrs[MMF>0.4*MAF & MMF<=0.8*MAF]=0.45*MMF[MMF>0.4*MAF & MMF<=0.8*MAF] # intermediate flow months
    efrs[MMF>0.8*MAF]=0.3*MMF[MMF>0.8*MAF] # high flow months
  }else if (method==efrMethodsSet[3]){ # "Q90Q50" - Pastor et al. 2014
    efrs[MMF<=MAF]=Q90[MMF<=MAF] # low flow months
    efrs[MMF>MAF]=Q50[MMF>MAF] # high flow months
  }
  return(efrs)
}
