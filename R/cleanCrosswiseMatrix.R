#' cleanCrosswiseMatrix
#'
#' @description
#'
#' Clean and scale a matrix from a genoMatriXeR object
#'
#' @usage cleanCrosswiseMatrix(GM, GM_pv, pvcut, scale, subEX)
#'
#' @inheritParams makeCrosswiseMatrix
#' @param GM matrix,  numerical matrix of z-scores from a genoMatriXeR object.
#' @param GM_pv matrix, numerical matrix of pvalues from genoMatriXeR object.
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
