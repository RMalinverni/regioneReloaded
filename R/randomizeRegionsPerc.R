#' randomize Regions Perc
#'
#'
#' randomize regions at percentage
#'
#' @usage randomizeRegionsPerc<-function(GR, genome, frac)
#'
#' @param GR description
#' @param genome description
#' @param frac description
#'
#' @export randomizeRegionsPerc
#'
#'
#'
#'
#'


randomizeRegionsPerc<-function(GR,
                               genome = "hg19",
                               frac=0.2,
                               ...)
  {

  nc<-round(length(GR) * frac)
  change<-sample(length(GR), nc)
  GR1<-GR[-change]
  GR2<-randomizeRegions(GR[change], genome=genome)
  GR3<-c(GR1,GR2)

  return(GR3)
}

