#' similar RegionSet
#'
#'
#' create a similar region set.
#'
#' @usage similarRegionSet<-function(GR, name,genome,vectorPerc)
#'
#' @param GR an object of class gMXR or a matrix
#' @param name character, name of the first element of gMXR object to test.
#' @param genome character, name of the second element of gMXR object to test.
#' @param vectorPerc character, label for x axes. (default = NA)
#'
#' @export similarRegionSet
#' @import ggplot2
#'
#'
  similarRegionSet<-function(GR,name,genome,vectorPerc){
  vectorNames<-vector()
  GRL<-list()
  for(i in 1:length(vectorPerc)){
    RGperc<-randomizeRegionsPerc(GR,genome = genome,frac = vectorPerc[i])
    GRL[[i]]<-RGperc
    vectorNames[i]<-paste0(name,"0",vectorPerc[i]*10)
  }
  GRL<-c(GRL,GR)
  vectorNames<-c(vectorNames,name)
  names(GRL)<-vectorNames
  return(GRL)
}
