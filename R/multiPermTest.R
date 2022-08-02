#' multiPermTest
#'
#' @details
#'
#' Perform a multiple permutation test
#'
#' @keywords internal function
#' @usage multiPermTest (A, Blist, ranFUN, evFUN, universe, genome, rFUN, ntimes, adj_pv_method, ...)
#'
#' @return a table obtained from parsing of [regioneR][permTest()] object
#'
#' @inheritParams crosswisePermTest
#' @param A Genomic Ranges or any accepted formats by  [regioneR](https://bioconductor.org/packages/release/bioc/html/regioneR.html) package
#' (\code{\link{GenomicRanges}}, \code{\link{data.frame}} etc...)
#'
#' @importFrom methods show
#' @importFrom stats sd
#' @importFrom stats p.adjust
#'
#' @keywords internal function

multiPermTest <-
  function(A,
           Blist,
           ranFUN,
           evFUN,
           universe,
           genome,
           rFUN,
           ntimes,
           adj_pv_method,
           ...) {

    #print(deparse(substitute(A)))
    methods::show(paste0("number of regions: ", length(A)))

    new.names <- names(Blist)
    func.list <-
      regioneR::createFunctionsList(FUN = evFUN,
                          param.name = "B",
                          values = Blist)
    ptm <- proc.time()

    pt <- regioneR::permTest(
      A = A,
      evaluate.function = func.list,
      randomize.function = rFUN,
      genome = genome ,
      ntimes = ntimes,
      universe = universe,
      ...
    )

    time <- proc.time() - ptm
    time <- time[3] / 60

    tab <- data.frame()

    for (j in seq_along(pt)) {
      if (pt[[j]]$zscore == 0 |
          is.na(pt[[j]]$zscore) |
          is.nan((pt[[j]]$zscore))) {
        zscore.norm <- 0
        #zscore.std <- 0
      } else{
        zscore.norm <- pt[[j]]$zscore / sqrt(length(A))
      }

      vec <- data.frame(
        order.id = j,
        name = new.names[j],
        n_regionA = length(A),
        n_regionB = length(Blist[[j]]),
        z_score = pt[[j]]$zscore,
        p_value = pt[[j]]$pval,
        n_overlaps = pt[[j]]$observed,
        mean_perm_test = mean(pt[[j]]$permuted),
        sd_perm_test = stats::sd(pt[[j]]$permuted)
      )
      tab <- rbind(vec, tab)
    }

    tab$norm_zscore <- tab$z_score / sqrt(tab$n_regionA)
    #max_zscore <-
      (tab$n_regionA - tab$mean_perm_test) / tab$sd_perm_test
    #tab$std_zscore <- tab$z_score / max_zscore
    tab$adj.p_value <-
      round(stats::p.adjust(tab$p_value, method = adj_pv_method), digits = 4)

    return(tab)
  }
