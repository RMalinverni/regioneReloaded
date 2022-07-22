#' crosswiseMatrix
#'
#' @description
#'
#' Create a matrix from a genoMatriXeR object.
#'
#' @usage crosswiseMatrix(mPT, zs.type='norm_zscore',...)
#'
#' @inheritParams makeCrosswiseMatrix
#'
#' @return a numeric matrix
#'
#' @examples
#'
#' data("cw_Alien")
#'
#' matrix_CW_Alien <- crosswiseMatrix(cw_Alien_ReG)
#'
#' @export

crosswiseMatrix <- function(mPT,
                            zs.type = 'norm_zscore',
                            ...)
  {

  A.obj <- mPT@multiOverlaps

  mat <- vector()
  for (i in seq_along(A.obj)) {

    mat <- cbind(mat, as.numeric(A.obj[[i]][, zs.type]))

  }

  colnames(mat) <- names(A.obj)
  rownames(mat) <- A.obj[[1]][, 2]
  mat <- as.matrix(mat)

  return(mat)

}
