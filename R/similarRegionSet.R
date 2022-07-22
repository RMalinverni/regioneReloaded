#' similar RegionSets
#'
#'
#' Create a list of class GenomicRanges similar to a reference RegionSet
#'
#' @usage similarRegionSet(GR, name, genome, vectorPerc)
#'
#' @param GR an object of class GenomigRanges
#' @param name character, name of the first element of genoMatriXeR object to test.
#' @param genome character, name of the second element of genoMatriXeR object to test.
#' @param vectorPerc character, label for x axes. (default = NA)
#'
#' @return A list of class GenomicRanges
#'
#' @examples
#'
#' data("cw_Alien")
#'
#' A<-createRandomRegions(nregions = 20, length.mean = 1000, length.sd = 100, genome = AlienGenome)
#' similAList <- similarRegionSet(GR = A, genome = AlienGenome,vectorPerc = seq(0.1,0.9,0.2), name = "test")
#' summary (similAList)
#'
#' @seealso \code{\link{GenomicRanges}}
#'
#' @import GenomicRanges
#'
#' @examples
#' data("cw_Alien")
#'
#' regA <- createRandomRegions(
#'   nregions = 100,
#'   length.mean = 10,
#'  length.sd = 5,
#'  genome = AlienGenome
#' )
#'
#' listRegA <- similarRegionSet(GR = regA, genome = AlienGenome)
#' summary(listRegA)
#'
#' @export similarRegionSet
#'
similarRegionSet <- function(GR,
                             name = "A",
                             genome = "hg19",
                             vectorPerc = seq(.1,.9,.1))
  {

  vectorNames <- vector()
  GRL <- list()

  for (i in seq_along(vectorPerc)) {
    RGperc <-
      randomizeRegionsPerc(GR, genome = genome, frac = vectorPerc[i])
    GRL[[i]] <- RGperc
    vectorNames[i] <- paste0(name, "_0", vectorPerc[i] * 10)
  }

  GRL <- c(GRL, GR)
  vectorNames <- c(vectorNames, name)
  names(GRL) <- vectorNames

  return(GRL)
}
