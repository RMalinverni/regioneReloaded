#' Create Universe
#'
#' create the universe parameter for \code{\link{resampleRegions}} function using all unique region present in Alist
#'
#' @usage createUniverse(Alist)
#'
#' @param Alist list of regions set in format accepted from \code{\link{regioneR}}
#'
#' @return A list of Genomic Ranges objects
#'
#' @examples
#'
#' data("cw_Alien")
#'
#' universe <- createUniverse(AlienRSList)
#'
#' @export
#' @keywords internal

createUniverse<-function(Alist){

  is.integer0 <- function(x)
  {
    is.integer(x) && length(x) == 0L
  }

  uniList<-GRanges()
  for(u in 1:length(Alist)){
    uniList<-c(uniList,Alist[[u]])
  }
  return(uniList)
}


