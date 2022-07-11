#' Clean Crosswise Matrix
#'
#' clean and scale a matrix from a gMXR object
#'
#' @usage cleanCrosswiseMatrix(GM, GM_pv, pvcut,scale)
#'
#' @param GM matrix,  numerical matrix from gMXR object.
#' @param GM_pv matrix, numerical matrix that represent p-value from gMXR object.
#' @param pvcut maximum pvalue accepted, (all the z-score vales with pvalue higher than "pvcut" will be transform in 0)
#' @param logic, if the matrix GM will be scaled using the function \code{link{scale}}. (default = FALSE)
#'
#' @keywords internal

cleanCrosswiseMatrix <-
  function(GM,
           GM_pv,
           pvcut,
           scale,
           subEx) {

    GM[GM_pv > pvcut] <- subEx

    if (scale == TRUE) {
      GM <- scale(GM)
    }

    GM[is.nan(GM)] <- subEx
    if (sum(GM) == 0) {
      stop("all values of the matrix is 0")
    }

    return(GM)
  }
