#' getMultiEvaluation
#'
#' @description
#' Get `multiEvaluation` slot from [genoMatriXeR][genoMatriXeR-class] or [multiLocalZScore][multiLocalZScore-class] class.
#'
#' @usage getMultiEvaluation( rR, namesRS = NULL)
#' @param rR A genoMatriXeR or multiLocalZScore object.
#' @param namesRS a vector of names. (default = NA)
#'
#' @returns
#' If rR is a [genoMatriXeR][genoMatriXeR-class] object, a list of data frames resuming the associations results.
#' If rR is a [multiLocalZScore][multiLocalZScore-class] object, a list of two elements: "resumeTable" that is a data frame
#' summarizing the associations and "shifts", a list of shifts computed from [multiLocalZscore()] function for the elements
#' indicated in the nameRS vector.
#'
#' @seealso [genoMatriXeR][genoMatriXeR-class], [multiLocalZScore][multiLocalZScore-class]
#'
#' @examples
#'
#' data("cw_Alien")
#'
#' mevs <- getMultiEvaluation(cw_Alien_ReG, names = "regA")
#'
#' mevs
#'
#' @importFrom methods is
#' @importFrom methods hasArg
#'
#' @export getMultiEvaluation
#'

getMultiEvaluation <- function(rR,namesRS = NULL){

  stopifnot("rR is missing" = methods::hasArg(rR))
  stopifnot("rR must be an object of class genoMatriXeR or multiLocalZScore" = {
    methods::is(rR , "genoMatriXeR") | methods::is(rR , "multiLocalZScore")
  })

  if(methods::is(rR , "genoMatriXeR" )){

    if (!is.null(namesRS)) {
      if (!all(namesRS %in% names(rR@multiOverlaps))) {
        warning("One or more of namesRS is not a name of a region set in rR")
      }
    } else {
      namesRS <- names(rR@multiOverlaps)
    }
    res <- rR@multiOverlaps[namesRS]
    res <- res[!sapply(res, is.null)]
  }

  if(methods::is(rR , "multiLocalZScore" )){

    if (!is.null(namesRS)) {
      if (!all(namesRS %in% names(rR@multiLocalZscores$shifed_ZSs))) {
        warning("One or more of namesRS is not a name of a region set in rR")
      }
    } else {
      namesRS <- names(rR@multiLocalZscores$shifed_ZSs)
    }
    resTable<-rR@multiLocalZscores$resumeTab
    rownames(resTable)<- NULL
    resTable <- resTable[,-8]
    res <- list(resumeTable = resTable,
                shifts = rR@multiLocalZscores$shifed_ZSs[namesRS])
    res[["shifts"]] <- res[["shifts"]][!sapply(res[["shifts"]], is.null)]
    }

  return(res)
}



