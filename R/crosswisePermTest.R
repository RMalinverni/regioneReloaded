#' Multiple Permutation test
#'
#' Perform a multiple permutation test between each elements of 2 list of Region
#' set the result will be store in a gMXR S4 class
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
#' A object of class \code{gMXR}
#' @seealso    \code{\link{regioneR}}, \code{\link{permTest}}, \code{\link{overlapPermTest}}
#'
#' @examples  ...
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
           universe = NULL,
           adj_pv_method = "BH",
           max_pv = 0.05,
           subEx = 0,
           genome = "hg19",
           verbose = FALSE,
           ...){

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
    #if (!is.numeric(ntimes))
    #  stop("ntimes must be numeric")
    if (!is.numeric(min_sampling))
      stop("min_sampling must be numeric")



    paramList <- list(
      Alist = deparse(substitute(Alist)),
      Blist = deparse(substitute(Blist)),
      sampling = deparse(substitute(sampling)),
      fraction = deparse(substitute(fraction)),
      min_sampling = deparse(substitute(fraction)),
      ranFUN = ranFUN,
      evFUN= evFUN,
      universe = deparse(substitute(universe)),
      adj_pv_method = adj_pv_method,
      max_pv = deparse(substitute(max_pv)),
      nc = NULL,
      matOrder = NULL
    )

    #ranFUN <- eval(parse(text = ranFUN))
    #evFUN <- eval(parse(text = evFUN))

   if (is.null(Blist)) {
      Blist <- Alist
   }


    list.tabs <- list()
    list.pt <- list()
    if (sampling == TRUE) {
      Alist <-
        subList (Alist, min_sampling = min_sampling, fraction = fraction)
    }


    FUN <- eval(parse(text = ranFUN))


    for (i in 1:length(Alist)) {
      print(names(Alist[i]))
      A <- Alist[[i]]
      print(paste0("number of regions: ", length(A)))
      new.names <- names(Blist)
      func.list <-
        createFunctionsList(FUN = evFUN,
                            param.name = "B",
                            values = Blist)
      ptm <- proc.time()

      if (ranFUN == "resampleRegions") {
        if (is.null(universe)) {
          warning(
            "resampleRegions function need that 'universe' is not NULL, universe was created using all the regions present in Alist"
          )
          uniList <- data.frame()
          for (u in 1:length(Alist)) {
            df <- toDataframe(Alist[[u]])[, 1:3]
            uniList <- rbind(uniList, df)
          }
          universe <- uniList
        }
      }

      pt <- permTest(
        A = A,
        evaluate.function = func.list,
        #aggiungere TryCatch
        randomize.function = FUN,
        genome = genome ,
        universe = universe,
        ...
      )

      paramList$ntimes <- pt[[1]]$ntimes

      time <- proc.time() - ptm
      time <- time[3] / 60
      if (verbose == TRUE) {
        print(paste0(" run in ", time, "  minute"))
      }
      tab <- data.frame()

      for (j in 1:length(pt)) {
        if (pt[[j]]$zscore == 0 |
            is.na(pt[[j]]$zscore) |
            is.nan((pt[[j]]$zscore))) {
          # modifica per non andare in errore
          zscore.norm <- 0
          zscore.std <- 0
        } else{
          zscore.norm <- pt[[j]]$zscore / sqrt(length(A))
        }

        vec <- data.frame(
          order.id = j,
          name = new.names[j],
          n_regionA = length(Alist[[i]]),
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
      max_zscore <- (tab$n_regionA - tab$mean_perm_test) / tab$sd_perm_test
      tab$std_zscore <- tab$z_score / max_zscore
      tab$adj.p_value <-
        round(p.adjust(tab$p_value, method = adj_pv_method), digits = 4)


      if (verbose == TRUE) {
        print(tab)
      }  # remember to activate only if verbose....
      list.tabs[[i]] <- tab
      names(list.tabs)[i] <- names(Alist)[i]
    }

    GMXRobj <- gMXR(
        parameters = paramList ,
        multiOverlaps = list.tabs ,
        matrix = list(NULL)
      )

    return(GMXRobj)
  }



