#' createUniverse
#'
#' @description
#'
#' Create the universe parameter for [regioneR::resampleRegions()] using all unique regions present in Alist.
#'
#' @usage createUniverse(Alist, joinR = TRUE)
#'
#' @param Alist list of regions set in a format accepted for [regioneR](https://bioconductor.org/packages/release/bioc/html/regioneR.html)
#' @param joinR logical, if TRUE all the regions will be joined using the function [regioneR::joinRegions()].(default == TRUE)
#'
#' @return A list of [GenomicRanges][GenomicRanges-class] objects
#'
#' @examples
#'
#' data("cw_Alien")
#'
#' universe <- createUniverse(AlienRSList_narrow)
#'
#' @export

createUniverse <- function(Alist, joinR = TRUE) {
  is.integer0 <- function(x) {
    is.integer(x) && length(x) == 0L
  }

  uniList <- GenomicRanges::GRanges()

  for (u in seq_along(Alist)) {
    uniList <- c(uniList, Alist[[u]])
  }

  if (joinR == TRUE) {
    uniList <- regioneR::joinRegions(uniList)
  }

  return(uniList)
}
