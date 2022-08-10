#' Get Matrix
#'
#' @description
#'
#' Returns the matrix from an [genoMatriXeR][genoMatriXeR-class] or [multiLocalZScore][multiLocalZScore-class] object.
#'
#' @usage getMatrix(rR)
#'
#' @param rR genoMatriXeR or multiLocalZScore object
#'
#' @return a numerical matrix from a
#'
#' @examples
#'
#' data("cw_Alien")
#'
#' cw_Alien_ReG <- makeCrosswiseMatrix(cw_Alien_ReG)
#' mtx <- getMatrix(cw_Alien_ReG)
#'
#' mtx
#'
#' @seealso [genoMatriXeR][genoMatriXeR-class], [multiLocalZScore][multiLocalZScore-class], [makeCrosswiseMatrix], [makeLZMatrix]
#'
#' @examples
#'
#' data("cw_Alien")
#'
#' cw_Alien_RaR <- makeCrosswiseMatrix(cw_Alien_RaR)
#' GM <- getMatrix(cw_Alien_RaR)
#'
#' GM
#'
#' @importFrom methods is
#' @importFrom methods hasArg
#'
#' @export getMatrix
#'

getMatrix <- function(rR) {

  if (!methods::hasArg(rR)) {
    stop("rR is missing")
  }

  if(!(methods::is(rR , "genoMatriXeR") | methods::is(rR , "multiLocalZScore"))){
    stop(" class of rR object need to be genoMatriXeR or multiLocalZScore")

  }

  if (methods::is(rR, "genoMatriXeR")) {

    if (is.null(rR@matrix[[1]])){
      warning("no matrix computed for genoMatriXeR object, run first makeCrosswiseMatrix()")
    }
    GM <- rR@matrix$GMat
  }

  if (methods::is(rR, "multiLocalZScore")) {
    if (is.null(rR@matrix[[1]])){
      warning("no matrix computed for multiLocalZScore object, run first makeLZMatrix")
    }

    GM <- rR@matrix$LZM
  }

  return(GM)
}


