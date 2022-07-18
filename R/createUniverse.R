#' Create Universe
#'
#' create the universe parameter for \code{\link{resampleRegions}} function using all unique region present in Alist
#'
#' @usage createUniverse(Alist, joinR = TRUE)
#'
#' @param Alist list of regions set in format accepted from [regioneR](https://bioconductor.org/packages/release/bioc/html/regioneR.html)
#' @param joinR logical, if TRUE all the regions will be joiner using the function [regioneR::joinRegions()].(default == TRUE)
#'
#' @return A list of Genomic Ranges objects
#'
#' @examples
#'
#' data("cw_Alien")
#'
#' universe <- createUniverse(AlienRSList_narrow)
#'
#'
#' @export
#' @keywords internal

createUniverse<-function(Alist, joinR=TRUE){

  is.integer0 <- function(x)
  {
    is.integer(x) && length(x) == 0L
  }

  uniList<-GenomicRanges::GRanges()

  for(u in 1:length(Alist)){
    uniList<-c(uniList,Alist[[u]])
  }

  if (joinR == TRUE) {
    uniList <- regioneR::joinRegions(uniList)
  }

  return(uniList)
}


