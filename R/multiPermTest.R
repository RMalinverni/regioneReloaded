#' multiPermTest
#'
#' @details
#'
#' Perform a multiple permutation test
#'
#' @keywords internal function
#' @usage multiPermTest (A, Blist, ranFUN, evFUN, uni, genome, rFUN, ntimes, adj_pv_method, ...)
#'
#' @return a data frame object computed starting from results of [regioneR::permTest()] function
#'
#' @inheritParams crosswisePermTest
#' @param A Genomic Ranges or any accepted formats by  [regioneR](https://bioconductor.org/packages/release/bioc/html/regioneR.html) package
#' ([GRanges][GenomicRanges::GRanges], [data.frame] etc.).
#' @param uni region set to use as universe, used only when [regioneR::resampleRegions()] function is selected. (default = NULL)
#' @seealso [regioneR::permTest()]
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
           uni,
           genome,
           rFUN,
           ntimes,
           adj_pv_method,
           ...) {

        #methods::show(paste0("number of regions: ", length(A)))
    new.names <- names(Blist)
    func.list <-
      regioneR::createFunctionsList(FUN = evFUN,
                                    param.name = "B",
                                    values = Blist)

    if(ranFUN == "resampleRegions"){


      pt <- regioneR::permTest(
        A = A,
        evaluate.function = func.list,
        randomize.function = rFUN,
        genome = genome ,
        ntimes = ntimes,
        universe = uni,
        ...
      )

    }else{

      pt <- regioneR::permTest(
        A = A,
        evaluate.function = func.list,
        randomize.function = rFUN,
        genome = genome ,
        ntimes = ntimes,
        ...
      )
    }

    tab <- do.call("rbind",lapply(seq_along(pt),
                                  FUN = function(j, pt){

                                    if (pt[[j]]$zscore == 0 |
                                        is.na(pt[[j]]$zscore) |
                                        is.nan((pt[[j]]$zscore))) {
                                      zscore.norm <- 0
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

                                  }, pt))

    tab$norm_zscore <- tab$z_score / sqrt(tab$n_regionA)
    tab$adj.p_value <-
      round(stats::p.adjust(tab$p_value, method = adj_pv_method), digits = 4)

    return(tab)
  }
