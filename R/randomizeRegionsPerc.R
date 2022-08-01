#' randomizeRegionsPerc
#'
#' @description
#'
#' Create a random region set similar to a reference region set.
#'
#' @details
#'
#' This function takes an input region set and generates a region set where a
#' fraction of the regions has been randomized.
#'
#' @usage randomizeRegionsPerc(GR, genome = "hg19", frac = 0.2, ...)
#'
#' @return a [GRanges-class] object
#'
#' @inheritParams similarRegionSet
#'
#' @param frac fraction of the original region set to randomize. (default = 0.2)
#' @param ... further arguments to be passed to other methods.
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
#' regA_02 <- randomizeRegionsPerc(GR = regA, genome = AlienGenome, frac = 0.2)
#'
#' @seealso [similarRegionSet()]
#'
#' @export randomizeRegionsPerc
#'


randomizeRegionsPerc<-function(GR,
                               genome = "hg19",
                               frac = 0.2,
                               ...)
  {

  nc<-round(length(GR) * frac)
  change<-sample(length(GR), nc)
  GR1<-GR[-change]
  GR2<-randomizeRegions(GR[change], genome=genome)
  GR3<-c(GR1,GR2)

  return(GR3)
}

