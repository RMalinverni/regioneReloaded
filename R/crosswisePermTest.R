#' crosswisePermTest
#'
#' @description
#'
#' Perform multiple permutation tests between each element in two lists of region sets.
#'
#' @details
#'
#' This function performs multiple permutation tests for all pairwise combinations
#' of the elements in two lists of region sets. Essentially, it uses the [regioneR::permTest()]
#' function and its associated randomization and evaluation functions. It creates and returns a
#' [genoMatriXeR-class] object with the result of the permutation tests stored in the `multiOverlaps` slot.
#' In addition, all the parameters used for the test are stored in the `parameters` slot.
#'
#' @usage crosswisePermTest(Alist, Blist = NULL, sampling = FALSE, fraction = 0.15,
#'  min_sampling = 5000, ranFUN = "randomizeRegions", evFUN = "numOverlaps",
#'  ntimes = 100, universe = NULL, adj_pv_method = "BH",
#'  genome = "hg19", ...)
#'
#'
#' @param Alist,Blist [GRangesList][GenomicRanges::GRangesList] or list of region sets in any accepted formats by [regioneR](https://bioconductor.org/packages/release/bioc/html/regioneR.html) package
#' ([GRanges][GenomicRanges::GRanges], [data.frame] etc.).
#' @param sampling logical, if TRUE the function will use only a sample of each element of Alist to perform the test as specified in `fraction.` (default = FALSE)
#' @param fraction logical, if `sampling=TRUE`, defines the fraction of the region sets used to perform the test. (default = 0.15)
#' @param min_sampling numeric, minimum number of regions accepted after sampling is performed with the specified `fraction`. If the number of sampled
#' regions is less than `min_sampling`, the number specified by `min_sampling` will be used as number of regions sampled instead. (default = 5000)
#' @param ranFUN character, the randomization strategy used for the test, see [regioneR](https://bioconductor.org/packages/release/bioc/html/regioneR.html). (default = "randomizeRegions")
#' @param evFUN  character, the evaluation strategy used for the test, see  [regioneR](https://bioconductor.org/packages/release/bioc/html/regioneR.html). (default = "numOverlaps)
#' @param ntimes numeric, number of permutations used in the test. (default = 100)
#' @param universe  region set to use as universe, used only when [regioneR::resampleRegions()] function is selected. (default = NULL)
#' @param adj_pv_method character, the method used for the calculation of the adjusted p-value, to choose between the options of [p.adjust()]. (default = "BH")
#' @param genome character or [GRanges][GenomicRanges::GRanges], genome used to compute the randomization. (default = "hg19")
#' @param ... further arguments to be passed to other methods.
#'
#' @return
#'
#' A object of class [genoMatriXeR][genoMatriXeR-class] containing three slots
#'
#' \itemize{
#' \item \bold{\code{@parameters}}
#' \item \bold{\code{@multioverlaps}}
#' \item \bold{\code{@matrix}}
#'
#' }
#'
#' @seealso [genoMatriXeR][genoMatriXeR-class], [`regioneR`](https://bioconductor.org/packages/release/bioc/html/regioneR.html), [regioneR::permTest()], [regioneR::overlapPermTest()]
#'
#' @examples
#'
#' fakeGenome <- regioneR::toGRanges("chrF", 1, 1000)
#' regA <- regioneR::createRandomRegions(nregions = 10, length.mean = 10,
#' length.sd = 2, genome = fakeGenome)
#' regB <- regioneR::createRandomRegions(nregions = 10, length.mean = 10,
#' length.sd = 2, genome = fakeGenome)
#' regAs <- similarRegionSet(GR = regA, genome = fakeGenome, name = "A",
#' vectorPerc = seq(0.1, 0.3, by = 0.1))
#' regBs <- similarRegionSet(GR = regB, genome = fakeGenome, name = "B",
#' vectorPerc = seq(0.1, 0.3, by = 0.1))
#' ABList <- c(regAs, regBs)
#' cw_ptAB <- crosswisePermTest(ABList, genome = fakeGenome, ntimes = 10)
#' print(cw_ptAB)
#'
#' @import regioneR
#' @importFrom methods hasArg
#' @importFrom methods new
#'
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
           genome = "hg19",
           ...) {

    # control parameters

    stopifnot("Alist is missing" = methods::hasArg(Alist))
    stopifnot("fraction must be numeric" = is.numeric(fraction))
    stopifnot("sampling must be logical" = is.logical(sampling))
    stopifnot("min_sampling must be numeric" = is.numeric(min_sampling))
    stopifnot("ranFun must be charachter" = is.character(ranFUN))
    stopifnot("evFun must be charachter" = is.character(evFUN))
    stopifnot("ntimes must be numeric" = is.numeric(ntimes))

    # create @parameters slot

    paramList <- list(
      Alist = deparse(substitute(Alist)),
      Blist = deparse(substitute(Blist)),
      sampling = deparse(substitute(sampling)),
      fraction = deparse(substitute(fraction)),
      min_sampling = deparse(substitute(fraction)),
      ranFUN = ranFUN,
      evFUN = evFUN,
      ntimes = ntimes,
      universe = NULL,
      adj_pv_method = adj_pv_method,
      nc = NULL,
      matOrder = NULL,
      errors = NULL
    )

    if (is.character(genome)){
      genome <- getGenome(genome)
    }

    if (is.null(names(Alist))){
      names(Alist) <- paste0("RS1_",1:length(Alist))
    }


    Alist <- as.list(Alist)

    Alist <- lapply(Alist, function(GR,genome){
      newGR <- GR[regioneR::overlapRegions(GR, genome, only.boolean = TRUE)]
    } ,genome)


    if (is.null(Blist)) {
      Blist <- Alist
    }else{
      Blist <- as.list(Blist)
    }

    Blist <- as.list(Blist)

    if (is.null(names(Blist))){
      names(Blist) <- paste0("RS2_",1:length(Blist))
    }

    rFUN <- eval(parse(text = ranFUN))

    if (sampling == TRUE) {
      Alist <-
        subList(Alist, min_sampling = min_sampling, fraction = fraction)
    }

    if ((ranFUN == "resampleRegions") & (is.null(universe))) {
      warning(
        "resampleRegions function need that 'universe' is not NULL, universe was created using all the regions present in Alist"
      )
      universe <- createUniverse(Alist)
    }
    
    # List all arguments to be passed on to mapply including the ellipsis
    mapargs <- list(Blist = Blist,
                    ranFUN = ranFUN, 
                    evFUN = evFUN, 
                    uni = universe,
                    genome = genome, 
                    rFUN = rFUN, 
                    ntimes = ntimes, 
                    adj_pv_method = adj_pv_method,
                    ...)

<<<<<<< HEAD
    # create multiOverlaps slot
=======
    # create @multiOverlaps slot
>>>>>>> david_dev

    list.tabs <- mapply(FUN = function(A, nameA, i, ...){
                            show(paste0("Performing permutation tests for ", nameA, " (", i, " of ", length(Alist), ")"))
                            tryCatch(
                              res <- multiPermTest(A, ...), error = function(e) {
<<<<<<< HEAD
                                message("There was an issue when performing the permutation test for: ", nameA)
=======
                                message(paste0("There was an error when performing the permutation test for: ", nameA))
>>>>>>> david_dev
                                return(list(NULL, e))
                              }
                            )
                        },
                        Alist,
                        names(Alist),
                        seq_along(Alist),
<<<<<<< HEAD
                        MoreArgs = list(Blist = Blist,
                        ranFUN = ranFUN, evFUN = evFUN, uni = universe,
                        genome = genome, rFUN = rFUN, ntimes = ntimes, adj_pv_method = adj_pv_method),
=======
                        MoreArgs = mapargs,
>>>>>>> david_dev
                        SIMPLIFY = FALSE
    )

    names(list.tabs) <- names(Alist)

    # Save error list
    list.errors <- lapply(list.tabs, FUN = function(x) {
      if (is.null(x[[1]])) {
        return(x[[2]])
      }
    })

    list.errors <- list.errors[!unlist(lapply(list.errors, FUN = is.null))]

    if(length(list.errors) > 0) {
      paramList$errors <- data.frame(call = unlist(lapply(list.errors, FUN = function(x) deparse(x[["call"]]))),
                                     errorMessage = unlist(lapply(list.errors, FUN = function(x) x[["message"]])))
<<<<<<< HEAD
      warning("There was an issue in one or more of the permutation test iterations (note that the evaluation for these test has been set to NULL)",
=======
      warning("There was an error in one or more of the permutation test iterations (note that the evaluation for these test has been set to NULL)",
>>>>>>> david_dev
              call. = FALSE)
    }

    # Clean list.tabs of errors
    list.tabs <- lapply(list.tabs, FUN = function(x) {
      if (is.data.frame(x)) {
        return(x)
      } else {
        return(NULL)
      }
    })

    # create S4 object (matrix slot is = NULL)

    GMXRobj <- gMXR(
      parameters = paramList,
      multiOverlaps = list.tabs,
      matrix = list(NULL)
    )

    return(GMXRobj)
  }
