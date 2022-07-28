  #' Make Local Z-Score Matrix
#'
#'
#' create a local z-score matrix from an [multiLocalZScore][multiLocalZScore-class] object and save it in the matrix slot
#'
#' @usage makeLZMatrix( mlZA, normalize = TRUE, clusterize = TRUE, centralize = NA, hc.method = NULL, dist.method = "euclidean",
#'                            scale = FALSE, ...)
#'
#' @param mlZA an object of class [multiLocalZScore][multiLocalZScore-class] or a matrix
#' @param normalize logic, if TRUE the matrix will be normalize. (default = FALSE)
#' @param clusterize logic, if TRUE the matrix will be clusterize using a method selected with the variable \code{hc.method} (default = TRUE)
#' @param hc.method character/vector, select the \code{hclust} method to use for clusterize the matrix,
#' if hc.method == NULL, the methods used for clusterize the matrix will be selected using the function \code{\link{chooseHclustMet}} (default = "NA")
#' @param dist.method character, metric used to calculate the distance matrix see \code{hclust} (default = "euclidean")
#' @param centralize numeric, number of "steps" around the center of the local association in which will be apply the clusterization algorithm, if NA all the values in the matrix will be utilize. (default = NA)
#' @param scale logic, if TRUE the matrix will be scaled (default = FALSE)
#' @param ...  further arguments to be passed to other methods.
#'
#'
#' @return
#'
#' A object of class [mLZS][multiLocalZScore-class] containing three slots
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


  if (!methods::is(mlZA,"multiLocalZScore")) {
    stop("the object mlZA need to be an multiLocalZScore object")
  }

  mat <- vector(length = length(mlZA@multiLocalZscores$shifts))

  if (normalize == TRUE) {

    for (i in seq_along(mlZA@multiLocalZscores$shifed_ZSs)) {
      mat <-
        rbind(
          mat,
          mlZA@multiLocalZscores$shifed_ZSs[[i]] / sqrt(mlZA@multiLocalZscores$max_zscores[i])
        )
    }
  } else{
    for (i in seq_along(mlZA@multiLocalZscores$shifed_ZSs)) {
      mat <- rbind(mat, mlZA@multiLocalZscores$shifed_ZSs[[i]])
    }
  }

  mat <- mat[-1,]

  if (is.vector(mat)) {
    mat <- t(as.data.frame(mat))
  }
  rownames(mat) <- names(mlZA@multiLocalZscores$shifed_ZSs)
  colnames(mat) <- mlZA@multiLocalZscores$shifts

  # I need to add a matLim integration

  if (clusterize == TRUE) {
    st <- 1
    en <- ncol(mat)
    if (!is.na(centralize)) {
      center <- round((ncol(mat) / 2)) + 1
      st <- (center - centralize)
      en <- (center + centralize)
    }
    fit <-
      chooseHclustMet(mat[, seq(st,en)],
                      scale = scale,
                      vecMet = hc.method,
                      distHC = dist.method)
    ind <- fit$labels[fit$order]
    mat <- mat[ind,]

  }

  mat_corX <- cor(x = t(mat), method = "pearson")
  mat_corX[is.na(mat_corX)]<-0

  fit2 <-
    chooseHclustMet(mat_corX,
                    scale = scale,
                    vecMet = hc.method,
                    distHC = dist.method)
  ind <- fit2$labels[fit2$order]
  mat_corX <- mat_corX[ind, ind]

  mlZA@matrix <-
    list(
      LZM = mat,
      LZM_cor = mat_corX,
      FitRow = fit,
      FitCorr = fit2
    )

  return(mlZA)
}



