#' randomize Regions Perc
#'
#'
#' randomize regions at percentage
#'
#' @usage randomizeRegionsPerc(GR, genome = "hg19", frac = 0.2, ...)
#'
#' @param GR description
#' @param genome description
#' @param frac description
#'
#' @examples
#'
#' data("cw_Alien")
#'
#' nreg <- 100
#'
#' regA <-
#'   createRandomRegions(
#'   nregions = nreg,
#'   length.mean = 100,
#'   length.sd = 10,
#'   non.overlapping = TRUE,
#'   genome = AlienGenome
#'  )
#'
#'  regA_02 <- randomizeRegionsPerc(GR = regA, genome = AlienGenome, frac = 0.2)
#'
#' @seealso similarRegionSet
#'
#' @export randomizeRegionsPerc
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

