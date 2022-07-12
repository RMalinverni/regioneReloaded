#' Multiple Permutation test
#'
#' Perform a multiple permutation test between each elements of two list of Region
#' set the result will be store in a genoMatriXeR S4 class
#'
#' @usage crosswisePermTest(Alist, Blist, sampling=FALSE, fraction=0.15,
#'                          ranFUN = "randomizeRegions",evFUN = "numOverlaps",
#'                          universe = NULL, adj_pv_method = "BH", max_pv = 0.05,
#'                          verbose = FALSE, subEx = 0, genome = "hg19",
#'                          verbose = FALSE...)
#'
#'
#' @param Alist GRangesList or list of Region Set of any accepted formats by  \code{\link{regioneR}} package
#' (\code{\link{GenomicRanges}}, \code{\link{data.frame}} etc...)
#' @param Blist GRangesList or list of Region Set of any accepted formats by  \code{\link{regioneR}} package
#' (\code{\link{GenomicRanges}}, \code{\link{data.frame}} etc...)
#' @param sampling Boolean, if is true the function will use only a sample of
#' each element of Alist to perform the test (default = FALSE)
#' @param fraction Logic, if sampling==TRUE is the fraction of the region sets
#' used to perform the test (default = 0.15)
#' @param min_sampling numeric, minimum number of regions accepted after the sampling, if the number of the sampled
#' regions is less than min_sampling will be used min_sampling value as number of regions
#' @param ranFUN (default = "randomizeRegions") choose the randomization strategy used for the test see  \code{\link{regioneR}}
#' @param evFUN  (default = "numOverlaps) choose the evaluation strategy used for the test see  \code{\link{regioneR}}
#' @param universe (default = NULL) used only when \code{\link{resampleRegions}} function is selected
#' @param adj_pv_method Charachter, the method used for the calculation of the adjusted p-value,
#' to choose between the options of \code{\link{p.adjust}}. (default = "BH")
#' @param max_pv Numeric, the z-scores associate a p-values higher of this parameter will be transform in subEx. (default =0.05)
#' @param subEx Numeric, (default = 0) substitute this value to a z-score when the p-value is higher than max_pv
#' @param genome Charachter or GenomicRanges, (defalut = "hg19") genome used to compute the randomization
#' @param verbose Boolean, if verbose test
#'
#' @details ...
#' @return
#'
#' A object of class \code{genoMatriXeR} containing three slots
#'
#' \itemize{
#' \item \bold{\code{@parameters}}
#' \item \bold{\code{@multioverlaps}}
#' \item \bold{\code{@matrix}}
#'
#' }
#'
#'
#' @seealso    \code{\link{genoMatriXeR_classes}} \code{\link{regioneR}} \code{\link{regioneR}}, \code{\link{permTest}}, \code{\link{overlapPermTest}} ,
#'
#' @examples
#'
#' \dontrun{
#'
#' data(cw_Alien)
#'
#' CW_Alien <- crosswisePermTest(Alist = AlienRSList, Blist = AlienRSList)
#'
#' summary(CW_Alien)
#'
#' }
#'
#'
#' @import regioneR
#' @import knitr
#' @export crosswisePermTest


crosswisePermTest <-
  function(Alist,
           Blist = NULL,
           sampling = FALSE,
           fraction = 0.15,
           min_sampling = 5000,
           ranFUN = "randomizeRegions",
           evFUN = "numOverlaps",
           ntimes = 100,
           universe = NULL,
           adj_pv_method = "BH",
           max_pv = 0.05,
           subEx = 0,
           genome = "hg19",
           verbose = FALSE,
           ...){

# control parameters

    if (!hasArg(Alist))
      stop("Alist is missing")
    if (!is.logical(sampling))
      stop("sampling must be logical")
    if (!is.numeric(fraction))
      stop("fraction must be numeric")
    if (!is.numeric(min_sampling))
      stop("min_sampling must be numeric")
    if(!is.character(ranFUN))
      stop("runFun must be charachter")
    if(!is.character(evFUN))
      stop("evFun must be charachter")
    if (!is.numeric(ntimes))
      stop("ntimes must be numeric")
    if (!is.numeric(min_sampling))
      stop("min_sampling must be numeric")


# create @parameters slot

    paramList <- list(
      Alist = deparse(substitute(Alist)),
      Blist = deparse(substitute(Blist)),
      sampling = deparse(substitute(sampling)),
      fraction = deparse(substitute(fraction)),
      min_sampling = deparse(substitute(fraction)),
      ranFUN = ranFUN,
      evFUN= evFUN,
      ntimes = ntimes,
      universe = deparse(substitute(universe)),
      adj_pv_method = adj_pv_method,
      max_pv = deparse(substitute(max_pv)),
      nc = NULL,
      matOrder = NULL
    )

    if (is.null(Blist)) {
      Blist <- Alist
    }

    list.tabs <- list()
    list.pt <- list()


    rFUN <- eval(parse(text = ranFUN))


    if (sampling == TRUE) {
      Alist <-
        subList(Alist, min_sampling = min_sampling, fraction = fraction)
    }

    if ((ranFUN == "resampleRegions") & (is.null(universe))) {
    warning(
      "resampleRegions function need that 'universe' is not NULL, universe was created using all the regions present in Alist"
    )
    }

# create @multiOverlaps slot

    list.tabs<-lapply(Alist, FUN=multiPermTest, ... ,Blist=Blist,
                      ranFUN=ranFUN, evFUN=evFUN,universe=universe,
                      genome=genome,rFUN=rFUN,ntimes=ntimes,adj_pv_method=adj_pv_method)

    names(list.tabs) <- names(Alist)

# create S4 object (matrix slot is = NULL)

    GMXRobj <- gMXR(
      parameters = paramList ,
      multiOverlaps = list.tabs ,
      matrix = list(NULL)
    )

    return(GMXRobj)
  }
