#' Make Local Z-Score Matrix
#'
#' @description
#'
#' Create a local z-score matrix from a [multiLocalZScore][multiLocalZScore-class]
#' object and save it in its `matrix` slot.
#'
#' @usage makeLZMatrix(mlZA, normalize = TRUE, clusterize = TRUE, centralize = NA, hc.method = NULL, dist.method = "euclidean",
#'                            scale = FALSE, ...)
#'
#' @inheritParams makeCrosswiseMatrix
#'
#' @param mlZA an object of class [multiLocalZScore][multiLocalZScore-class] or a numerical matrix.
#' @param normalize logical, if TRUE the z-score values in the matrix will be normalized. (default = FALSE)
#' @param centralize numeric, only z-score values in a number of steps (defined by `centralize`) around the center of the local association will be used for clustering. If NA, all the values in the matrix will be used for clustering. (default = NA)
#' @param ...  further arguments to be passed to other methods.
#'
#' @return
#'
#' A object of class [multiLocalZScore][multiLocalZScore-class] containing three slots, with a populated `matrix` slot.
#'
#' \itemize{
#' \item \bold{\code{@parameters}}
#' \item \bold{\code{@multiLocalZscores}}
#' \item \bold{\code{@matrix}}
#'
#' }
#'
#'
#' @seealso  \code{\link{localZScore}}
#' @examples
#'
#' data("cw_Alien")
#'
#'
#'
#' @importFrom methods is
#' @export makeLZMatrix
#'
makeLZMatrix <- function(mlZA,
                          normalize = TRUE,
                          clusterize = TRUE,
                          centralize = NA,
                          hc.method = NULL,
                          dist.method = "euclidean",
                          scale = FALSE,
                          ...) {

  stopifnot("mlZA is missing" = methods::hasArg(mlZA))
  stopifnot("mlZA must be an object of class multiLocalZScore" = methods::is(mlZA,"multiLocalZScore"))

  if (normalize == TRUE) {

    mat <- do.call("rbind",lapply(seq_along(mlzsMultiLocalZscores(mlZA)$shifed_ZSs),
                                  FUN = function(i, mlZA){
                                    mlzsMultiLocalZscores(mlZA)$shifed_ZSs[[i]] / sqrt(mlzsMultiLocalZscores(mlZA)$max_zscores[i])},
                                  mlZA))

  }else{


    mat <- do.call("rbind",lapply(seq_along(mlzsMultiLocalZscores(mlZA)$shifed_ZSs),
                                  FUN = function(i, mlZA){
                                    mlzsMultiLocalZscores(mlZA)$shifed_ZSs[[i]]},
                                  mlZA))
  }

  rownames(mat) <- names(mlzsMultiLocalZscores(mlZA)$shifed_ZSs)
  colnames(mat) <- mlzsMultiLocalZscores(mlZA)$shifts


  if (clusterize == TRUE) {
    st <- 1
    en <- ncol(mat)
    if (!is.na(centralize)) {
      center <- round((ncol(mat) / 2)) + 1
      st <- (center - centralize)
      en <- (center + centralize)
    }
    fit <-
      chooseHclustMet(mat[, seq(st,en), drop = FALSE],
                      scale = scale,
                      vecMet = hc.method,
                      distHC = dist.method)
    ind <- fit$labels[fit$order]
    mat <- mat[ind, , drop = FALSE]

  }

  mat_corX <- cor(x = t(mat), method = "pearson")
  mat_corX[is.na(mat_corX)]<-0

  fit2 <-
    chooseHclustMet(mat_corX,
                    scale = scale,
                    vecMet = hc.method,
                    distHC = dist.method)
  ind <- fit2$labels[fit2$order]
  mat_corX <- mat_corX[ind, ind, drop = FALSE]

  matL <-
    list(
      LZM = mat,
      LZM_cor = mat_corX,
      FitRow = fit,
      FitCorr = fit2
    )

  mlzsMatrix(mlZA) <- matL

  return(mlZA)
}
