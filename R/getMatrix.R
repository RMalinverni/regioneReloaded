#' Get Matrix
#'
#' @description
#'
#' Return matrix from an genoMatriXeR or multiLocalZScore object
#'
#' @usage getMatrix(rR, vebose = TRUE)
#'
#' @param rR genoMatriXeR or multiLocalZScore object
#' @return a numeric Matrix
#' @examples
#'
#' data("cw_Alien")
#'
#' cw_Alien_RaR <- makeCrosswiseMatrix(cw_Alien_RaR)
#' GM <- getMatrix(cw_Alien_RaR)
#'
#' @importFrom methods is
#' @export getMatrix
#'

getMatrix <- function(rR) {

  if (!methods::hasArg(rR)) {
    stop("rR is missing")
  }

  if (methods::is(rR, "genoMatriXeR")) {

    if (is.null(rR@matrix[[1]])){
      stop ("no matrix computed for genoMatriXeR object")
    }
    GM <- rR@matrix$GMat
  }

  if (methods::is(rR, "multiLocalZScore")) {
    if (is.null(rR@matrix[[1]])){
      stop ("no matrix computed for multiLocalZScore object")
    }

    GM <- rR@matrix$LZM
  }

  return(GM)
}


