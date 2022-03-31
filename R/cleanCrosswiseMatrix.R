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
#' @export
#' @keywords internal

cleanCrosswiseMatrix <-
  function(GM,
           GM_pv,
           pvcut,
           scale) {

    GM[GM_pv > pvcut] <- 0

    if (scale == TRUE) {
      GM2 <- scale(GM2)
    }

    GM[is.nan(GM)] <- 0
    if (sum(GM) == 0) {
      stop("all values of the matrix is 0")
    }

    return(GM)
  }
