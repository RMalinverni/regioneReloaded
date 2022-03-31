#' Create Universe
#'
#' create the universe parameter for \code{\link{resampleRegions}} function
#'
#' @usage createUniverse(Alist)
#'
#' @param Alist list of regions set in format accepted from \code{\link{regioneR}}
#' @export
#' @keywords internal

createUniverse<-function(Alist){
  uniList<-GRanges()
  for(u in 1:length(Alist)){
    uniList<-c(uniList,Alist[[u]])
  }
  return(uniList)
}

is.integer0 <- function(x)
{
  is.integer(x) && length(x) == 0L
}
