#' getMultiEvaluation
#'
#' @description
#' Get `multiEvaluation` slot from [genoMatriXeR][genoMatriXeR-class] or [multiLocalZScore][multiLocalZScore-class] class.
#'
#' @usage getMultiEvaluation( rR, namesRS = NA)
#' @param rR A genoMatriXeR or multiLocalZScore object.
#' @param namesRS a vector of names. (default = NA)
#'
#' @returns
#' If rR is a [genoMatriXeR][genoMatriXeR-class] object, a list of data frames resuming the associations results.
#' If rR is a [multiLocalZScore][multiLocalZScore-class] object, a list of two elements: "resumeTable" that is the resume of the associations analysis and "shifts",
#' a list of shifts computed from [multiLocalZScore()] function for the element indicated in the nameRS vector.
#'
#' @seealso [genoMatriXeR][genoMatriXeR-class], [multiLocalZScore][multiLocalZScore_class]
#'
#' @export getMultiEvaluation
#'

getMultiEvaluation <- function(rR,namesRS = NA){

  if (!methods::hasArg(rR)) {
    stop("rR is missing")
  }


  if(methods::is(rR , "genoMatriXeR" )){

    if (is.na(namesRS)) { namesRS <- names(rR@multiOverlaps) }

    res <- rR@multiOverlaps[namesRS]
  }

  if(methods::is(rR , "multiLocalZScore" )){

    if (is.na(namesRS)) {
      namesRS <- names(rR@multiLocalZscores$shifed_ZSs) }

    res <- list(resumeTable = rR@multiLocalZscores$resumeTab ,
                shifts = rR@multiLocalZscores$shifed_ZSs[namesRS])
  }

  return(res)
}



