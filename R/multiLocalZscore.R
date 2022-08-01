#' multiLocalZscore
#'
#' @description
#'
#' Perform multiple permutation tests between a region set and each element in
#' a list of region sets using shifted positions to calculate a local z-score.
#'
#' @details
#'
#' This function performs multiple permutation tests between a single region set
#' and each element in a list of region sets. For every pairwise combination, the
#' evaluation step is repeated each time shifting the position of all the regions in the query region set
#' by a fixed step inside a defined window (using [regioneR::localZScore()].
#' This produces a "local z-score" profile that can be indicative of the nature
#' of the association between region sets. For example, an association can occur
#' "centrally" if the z-score value drops sharply when sifting the region set.
#' On the other hand, two region sets may have a peak of local z-score away from
#' the central position if they happen to occur often at a regular distance,
#' showing a "lateral" association.
#'
#' @usage multiLocalZscore( A, Blist, ranFUN = "randomizeRegions", evFUN = "numOverlaps", sampling = FALSE,
#' min_sampling = 5000, fraction = 0.15, universe = NULL, window = 1000, step = 100, adj_pv_method = "BH",
#' max_pv = 0.05, genome = "hg19", ...)
#'
#' @inheritParams crosswisePermTest
#'
#' @param A
#' @param Blist [GRangesList] or list of region sets in any accepted formats by [regioneR](https://bioconductor.org/packages/release/bioc/html/regioneR.html) package
#' ([GenomicRanges], [data.frame] etc...).
#' @param window numeric, window (number of base pairs) in which the local z-score will be calculated. (default = 1000)
#' @param step numeric, step (number of base pairs) by which will be estimated the local Z-score. (default = 100)
#' @param ...  further arguments to be passed to other methods.
#'
#' @return
#'
#' A object of class [multiLocalZScore][multiLocalZScore-class] containing three slots
#'
#' \itemize{
#' \item \bold{\code{@parameters}}
#' \item \bold{\code{@multiLocalZscores}}
#' \item \bold{\code{@matrix}}
#'
#' }
#'
#'
#' @seealso  [regioneR::localZScore()]
#'
#' @examples
#'
#' fakeGenome<- regioneR::toGRanges("chrF",1,1000)
#' regA <- regioneR::createRandomRegions(nregions = 10, length.mean = 10,length.sd = 2,genome = fakeGenome)
#' regB <- regioneR::createRandomRegions(nregions = 10,length.mean = 10,length.sd = 2,genome = fakeGenome)
#' regAs <-similarRegionSet(GR = regA,genome = fakeGenome, name = "A",vectorPerc = seq(0.1,0.3,by =0.1))
#' regBs <-similarRegionSet(GR = regB,genome = fakeGenome, name = "B", vectorPerc = seq(0.1,0.3,by =0.1))
#' ABList <- c(regAs,regBs)
#'
#' mlz_ptAB <- multiLocalZscore(A = regA, Blist = ABList, genome = fakeGenome, ntimes = 10)
#' summary(mlz_ptAB)
#'
#'
#' @importFrom methods new
#' @importFrom methods show
#' @importFrom stats sd
#' @importFrom stats p.adjust
#'
#' @export multiLocalZscore



multiLocalZscore <- function(A,
                             Blist = NULL,
                             ranFUN = "randomizeRegions",
                             evFUN = "numOverlaps",
                             sampling = FALSE,
                             min_sampling = 5000,
                             fraction = 0.15,
                             universe = NULL,
                             window = 1000,
                             step = 100,
                             adj_pv_method = "BH",
                             max_pv = 0.05,
                             genome = "hg19",
                             ...) {

  paramList <- list(
    A = deparse(substitute(A)),
    Blist = deparse(substitute(Blist)),
    sampling = deparse(substitute(sampling)),
    fraction = deparse(substitute(fraction)),
    min_sampling = deparse(substitute(fraction)),
    ranFUN = ranFUN,
    evFUN = evFUN,
    universe = deparse(substitute(universe)),
    window = window,
    step = step,
    adj_pv_method = adj_pv_method,
    max_pv = deparse(substitute(max_pv))
  )

  ranFUN <- eval(parse(text = ranFUN))
  evFUN <- eval(parse(text = evFUN))

  A <- toGRanges(A)

  if (sampling == TRUE) {
    if (length(A) >= min_sampling) {
      if (length(A) * fraction > min_sampling) {
        A <- A[sample(length(A), round(length(A) * fraction))]

      } else{
        A <- A[sample(length(A), min_sampling)]
      }
    }
  }


  if (paramList$ranFUN == "resampleRegions" & is.null(universe)) {
    if (is.null(universe)) {
      methods::show(
        "resampleRegions function need that universe parameters in not NULL universe will created using all the regions present in Blist"
      )
      universe <- createUniverse(Blist) # check well this option
    }
  }


  funct.list <-
    regioneR::createFunctionsList(FUN = evFUN,
                        param.name = "B",
                        values = Blist)


  pt <- regioneR::permTest(
    A = A,
    evaluate.function = funct.list,
    randomize.function = ranFUN,
    genome = genome ,
    universe = universe ,
    ...
  )

  lZs <-
    lapply(
      pt,
      regioneR::localZScore,
      ... ,
      A = A ,
      count.once = TRUE,
      window = window,
      step = step
    )
  names(lZs) <- names(pt)


  Nreg <- length(A)
  p_values <- do.call(c, pt)
  pval <- p_values[grep(".pval", names(p_values))]

  means_pemuted <-
    lapply(p_values[grep(".permuted", names(p_values))], mean)

  sd_pemuted <-
    lapply(p_values[grep(".permuted", names(p_values))], stats::sd)

  z_score <- p_values[grep(".zscore", names(p_values))]
  observed <- p_values[grep(".observed", names(p_values))]
  localZs <- do.call(c, lZs)
  shiftedZs <- localZs[grep("shifted.z.scores", names(localZs))]
  shifts <- localZs[grep(".shifts", names(localZs))][[1]]


  if (is.null(names(Blist))) {
    names(Blist) <- seq_along(Blist)
  }

  tab <- data.frame(
    name = names(Blist),
    p_value = unlist(pval),
    z_score = unlist(z_score),
    mean_perm_test = unlist(means_pemuted),
    sd_perm_test = unlist(sd_pemuted),
    n_overlaps = unlist(observed)
  )

  tab$norm_zscore <- tab$z_score / sqrt(Nreg)
  maxzscores <- (Nreg - tab$mean_perm_test) / tab$sd_perm_test
  tab$ranged_zscore <- tab$z_score / maxzscores
  tab$adj.p_value <-
  round(stats::p.adjust(tab$p_value, method = adj_pv_method), digits = 4)

  names(shiftedZs) <- names(Blist)

  paramList$Nregions <- Nreg


  mLZSobj <- mLZS(
    parameters = paramList ,
    multiLocalZscores = list(
      resumeTab = tab,
      max_zscores = maxzscores,
      shifts = shifts,
      shifed_ZSs = shiftedZs
    ),
    matrix = list(NULL)
  )


  return (mLZSobj)
}



