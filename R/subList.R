#' Sub List from region set list
#'
#' create a samples sublist starting from a list of Region Set
#'
#' @usage subList(Alist, min_sampling, fraction)
#'
#' @param Alist,  GRangesList or list of Region Set of any accepted formats by  \code{\link{regioneR}} package
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
  subAlist <- list()

  for (i in 1:length(Alist)) {
    A <- Alist[[i]]
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

  names(subAlist) <- names(Alist)

  return(subAlist)
}
