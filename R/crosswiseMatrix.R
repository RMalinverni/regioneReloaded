#' Matrix from crosswisePermTest Object
#'
#' create a matrix from a gMXR object
#'
#' @usage crosswiseMatrix(mPT, zs.type='norm_zscore',...)
#'
#' @param mPt,  object of class = "gMXR"
#' @param zs.type Character, choose if create the matrix using every column from MultiOverlap slot of gMXR object default: 'norm_zscore'
#'
#' @export
#' @keywords  internal function

crosswiseMatrix <- function(mPT, zs.type = 'norm_zscore', ...) {
  A.obj <- mPT@multiOverlaps
  mat <- vector()
  for (i in 1:length(A.obj)) {
    mat <- cbind(mat, as.numeric(A.obj[[i]][, zs.type]))
  }
  colnames(mat) <- names(A.obj)
  rownames(mat) <- A.obj[[1]][, 2]
  mat <- as.matrix(mat)
  return(mat)

}
