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
#' @keywords internal

crosswiseMatrix <- function(mPT,
                            zs.type = "norm_zscore",
                            ...) {

  A.obj <- getMultiEvaluation(mPT)


  mat <- do.call("cbind",lapply(seq_along(A.obj),
                                FUN =  function(i,A.obj, zs.type){
                                  as.numeric(A.obj[[i]][, zs.type])
                                },
                                A.obj, zs.type))

  colnames(mat) <- names(A.obj)
  rownames(mat) <- A.obj[[1]][, 2]
  mat <- as.matrix(mat)

  return(mat)
}
