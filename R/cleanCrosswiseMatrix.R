#' cleanCrosswiseMatrix
#'
#' @description
#'
#' Clean and scale a matrix from a genoMatriXeR object
#'
#' @usage cleanCrosswiseMatrix(GM, GM_pv, pvcut, scale, subEX)
#'
#' @return a matrix filtered for a matrix of pvalue
#'
#' @seealso [makeCrosswiseMatrix()]
#'
#' @inheritParams makeCrosswiseMatrix
#'
#' @param GM matrix,  numerical matrix of z-scores.
#' @param GM_pv matrix, numerical matrix of pvalues.
#'
#' @keywords internal

cleanCrosswiseMatrix <-
  function(GM,
           GM_pv,
           pvcut,
           scale,
           subEX) {
    GM[GM_pv > pvcut] <- subEX

    if (scale == TRUE) {
      GM <- scale(GM)
    }

    GM[is.nan(GM)] <- subEX
    if (sum(GM) == 0) {
      stop("all values of the matrix is 0")
    }

    return(GM)
  }
