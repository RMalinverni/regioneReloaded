#' Multiple Permutation Test
#'
#' Perform a multiple permutation test
#' @keywords internal function
#' @usage multiPermTest (A, Blist, ranFUN, evFUN, universe, genome, rFUN, verbose = FALSE, ntimes, adj_pv_method, ...)
#'
#' @param A Genomic Ranges or any accepted formats by  [regioneR](https://bioconductor.org/packages/release/bioc/html/regioneR.html) package
#' (\code{\link{GenomicRanges}}, \code{\link{data.frame}} etc...)
#' @param Blist GRangesList or list of Region Set of any accepted formats by  [regioneR](https://bioconductor.org/packages/release/bioc/html/regioneR.html) package
#' (\code{\link{GenomicRanges}}, \code{\link{data.frame}} etc...)
#' @param ranFUN (default = "randomizeRegions") choose the randomization strategy used for the test see  [regioneR](https://bioconductor.org/packages/release/bioc/html/regioneR.html)
#' @param evFUN  (default = "numOverlaps) choose the evaluation strategy used for the test see  [regioneR](https://bioconductor.org/packages/release/bioc/html/regioneR.html)
#' @param universe (default = NULL) used only when \code{\link{resampleRegions}} function is selected
#' @param adj_pv_method Charachter, the method used for the calculation of the adjusted p-value,
#' to choose between the options of \code{\link{p.adjust}}. (default = "BH")
#' @param genome Charachter or GenomicRanges, (defalut = "hg19") genome used to compute the randomization
#' @param verbose Boolean, if verbose test
#'
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
           verbose = FALSE,
           ntimes,
           adj_pv_method,
           ...) {

    #print(deparse(substitute(A)))
    print(paste0("number of regions: ", length(A)))

    new.names <- names(Blist)
    func.list <-
      regioneR::createFunctionsList(FUN = evFUN,
                          param.name = "B",
                          values = Blist)
    ptm <- proc.time()

    # if (ranFUN == "resampleRegions") {
    #   if (is.null(universe)) {
    #
    #     uniList <- data.frame()
    #     for (u in seq_along(Alist)) {
    #       df <- regioneR::toDataframe(Alist[[u]])[, 1:3]
    #       uniList <- rbind(uniList, df)
    #     }
    #     universe <- uniList
    #   }
    # }

    # controllare se passano le variabili

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
    if (verbose == TRUE) {
      print(paste0(" run in ", time, "  minute"))
    }

    tab <- data.frame()

    for (j in seq_along(pt)) {
      if (pt[[j]]$zscore == 0 |
          is.na(pt[[j]]$zscore) |
          is.nan((pt[[j]]$zscore))) {
        zscore.norm <- 0
        zscore.std <- 0
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
        sd_perm_test = sd(pt[[j]]$permuted)
      )
      tab <- rbind(vec, tab)
    }

    tab$norm_zscore <- tab$z_score / sqrt(tab$n_regionA)
    max_zscore <-
      (tab$n_regionA - tab$mean_perm_test) / tab$sd_perm_test
    tab$std_zscore <- tab$z_score / max_zscore
    tab$adj.p_value <-
      round(p.adjust(tab$p_value, method = adj_pv_method), digits = 4)

    if (verbose == TRUE) {
      print(tab)
    }


    return(tab)
  }
