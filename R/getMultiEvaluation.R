#' getMultiEvaluation
#'
#' @description
#' get multiEvaluation Object from genoMatriXeR or multiLocalZScore class
#'
#' @usage getMultiEvaluation( rR, namesRS = NA)
#' @param rR A genoMatriXeR or multiLocalZScore object.
#' @param namesRS a vector of names. (default = NA)
#'
#' @returns
#' If rR is a [genoMatriXeR][genoMatriXeR_class] object a list of data frames resuming the associations results.
#' If rR is a [multiLocalZScore][multiLocalZScore_class] object return al list of two elements "resumeTable" that is the resume of the associations analysis and "shifts",
#' a list of shifts computed from [multilocalZScore()] function for the element indicated in the nameRS vector.
#'
#' @seealso [genoMatriXeR][genoMatriXeR-class]
#'
#' @export getMultiEvaluation
#'
getMultiEvaluation <- function(rR,namesRS){

  if (!methods::hasArg(rR)) {
    stop("rR is missing")
  }

  if(methods::is(rR , "genoMatriXeR" )){

    if (is.na(namesRS)) {
      namesRS <- names(rR@multiOverlaps)
    }
    res <- rR@multiOverlaps[namesRS]
  }

  if(methods::is(rR , "multiLocalZScore" )){
    if (is.na(namesRS)) {
      namesRS <- names(rR@multiLocalZscores$shifed_ZSs)
    }
    res <- list(resumeTable = rR@multiLocalZscores$resumeTab ,
                shifts = rR@multiLocalZscores$shifed_ZSs[namesRS])
  }

  return(res)
}



