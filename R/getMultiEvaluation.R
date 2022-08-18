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

  if (!methods::hasArg(rR)) {
    stop("rR is missing")
  }

  if(!(methods::is(rR , "genoMatriXeR") | methods::is(rR , "multiLocalZScore"))){
    stop(" class of rR object need to be genoMatriXeR or multiLocalZScore")
  }

  if(methods::is(rR , "genoMatriXeR" )){

    if (!is.null(namesRS)) {
      if (!all(namesRS %in% names(gmxrMultiOverlaps(rR)))) {
        warning("One or more of namesRS is not a name of a region set in rR")
      }
    } else {
      namesRS <- names(gmxrMultiOverlaps(rR))
    }
    res <- gmxrMultiOverlaps(rR)[namesRS]
    res <- res[!sapply(res, is.null)]
  }

  if(methods::is(rR , "multiLocalZScore" )){

    if (!is.null(namesRS)) {
      if (!all(namesRS %in% names(mlzsMultiLocalZscores(rR)$shifed_ZSs))) {
        warning("One or more of namesRS is not a name of a region set in rR")
      }
    } else {
      namesRS <- names(mlzsMultiLocalZscores(rR)$shifed_ZSs)
    }
    resTable<-mlzsMultiLocalZscores(rR)$resumeTab
    rownames(resTable)<- NULL
    resTable <- resTable[,-8]
    res <- list(resumeTable = resTable,
                shifts = mlzsMultiLocalZscores(rR)$shifed_ZSs[namesRS])
    res[["shifts"]] <- res[["shifts"]][!sapply(res[["shifts"]], is.null)]
    }

  return(res)
}



