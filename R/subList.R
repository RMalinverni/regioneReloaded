#' Sub List from region set list
#'
#' create a samples sublist starting from a list of Region Set
#'
#' @usage subList(Alist, min_sampling, fraction)
#' @return a sublist of GenomicRanges from an original GenomicRanges list
#'
#' @param Alist,  GRangesList or list of Region Set of any accepted formats by  [regioneR](https://bioconductor.org/packages/release/bioc/html/regioneR.html) package
#' @param min_sampling numeric, minimum number of regions for each element of the list,
#' if the number of regions is less than min_samplnig as subsempilng will be take the entire regionset
#' @param fraction numeric, percentage of regions selected for the subsampling
#'
#'
#'
#'


subList <- function(Alist,
                    min_sampling,
                    fraction) {

  subFUN<-function(A,min_sampling, fraction){

    if (min_sampling < length(A)) {
      subN <- round(length(A) * fraction)
      if (subN < min_sampling) {
        subN <- min_sampling
      }
      A <- A[sample(length(A), subN)]
      subAlist[[i]] <- A
    } else {
      subAlist[[i]] <- A
    }
  }

  subAlist <- lapply(Alist, subFUN, min_sampling, fraction)




  return(subAlist)
}
