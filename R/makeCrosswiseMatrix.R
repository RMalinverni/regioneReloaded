#' makeCrosswiseMatrix
#'
#' @description
#'
#' Populate the matrix slot in a [genoMatriXeR][genoMatriXeR-class] object.
#'
#' @details
#'
#' This function will create a series of matrices of z-scores, adj.pvalues and
#' pearson correlation values from all the pairwise permutation tests stored in
#' the `multiOverlaps` slot of a [genoMatriXeR][genoMatriXeR-class] as
#' calculated with [multiPermTest()]. This matrices will then be stored in the
#' `matrix` slot of the [genoMatriXeR][genoMatriXeR-class] object.
#'
#' @usage makeCrosswiseMatrix(mPT, clusterize = TRUE, hc.method = NULL, dist.method = "euclidean",
#' transform = FALSE, scale = FALSE, zs.type = 'norm_zscore', symm_matrix = TRUE,
#' selectRow = NULL, selectCol = NULL, pvcut = 1, subEX = 0, GM_diag = NULL, ...)
#'
#' @param mPT an object of class [genoMatriXeR][genoMatriXeR-class].
#' @param clusterize logical, if TRUE the matrix will be clustered using the method specified by \code{hc.method} (default = TRUE)
#' @param hc.method character, select the \code{hclust} method to use for clustering the matrix, if NULL, the clustering method will be automatically selected by the function [chooseHclustMet()]. (default = NULL)
#' @param dist.method character, the distance measure to be used from those available in [dist()] . (default = "euclidean")
#' @param transform logical, if TRUE the matrix will be transformed using the function [t()]. (default = FALSE)
#' @param scale logical, if TRUE the matrix will be scaled. (default = FALSE)
#' @param zs.type character, z-score type to use to generate the matrix, either raw z-score ("zscore") or normalized z-score ("norm_zscore"). (default = "norm_zscore")
#' @param symm_matrix logical, if TRUE the matrix will be treated as symmetrical (same clustering for rows and columns). (default = TRUE)
#' @param selectRow,selectCol vector, the matrix will be reduced selecting the rows and/or columns in this vector. (default = NULL)
#' @param pvcut numeric, the z-score value is substituted by `subEX` (0 by default) for all the associations with an adj.pvalue (as calculated in [crosswisePermTest()]) higher than `pvcut`. (default = 0.05)
#' @param subEX numeric, value used to substitute the z-score values when the associated pvalue is higher than `pvcut`. (default = 0)
#' @param GM_diag logic, if FALSE the values of the diagonal will be set as 0. (default = TRUE)
#' @param ... further arguments to be passed to other methods.
#'
#' @return
#'
#' An object of class [genoMatriXeR][genoMatriXeR-class] containing three slots, with a populated `matrix` slot.
#'
#' \itemize{
#' \item \bold{\code{@parameters}}
#' \item \bold{\code{@multioverlaps}}
#' \item \bold{\code{@matrix}}
#'
#' }
#'
#' @seealso [crosswisePermTest()], [chooseHclustMet()], [plotCrosswiseMatrix()]
#'
#' @examples
#'
#' data("cw_Alien")
#'
#' cw_Alien_ReG <- makeCrosswiseMatrix(cw_Alien_ReG)
#'
#' summary(cw_Alien_ReG)
#'
#' @importFrom stats cor
#' @importFrom methods is
#' @export makeCrosswiseMatrix


makeCrosswiseMatrix <-

  function(mPT,
           clusterize = TRUE,
           hc.method = NULL,
           dist.method = "euclidean",
           transform = FALSE,
           scale = FALSE,
           zs.type = 'norm_zscore',
           symm_matrix = TRUE,
           selectRow = NULL,
           selectCol = NULL,
           pvcut = 1,
           subEX = 0,
           GM_diag = TRUE,
           ...) {

    if (methods::is(mPT,"genoMatriXeR")) {
      mat <- crosswiseMatrix(mPT, zs.type = zs.type)
      mat_pv <- crosswiseMatrix(mPT, zs.type = "adj.p_value")


    } else {
      stop(' mPT need to be a "genoMatriXeR" class object ')
    }


    if (!is.null(selectRow)) {
      indRow <- grep(paste(selectRow, collapse = "|"), rownames(mat))
      mat <- mat[indRow, ]
      mat_pv <- mat_pv[indRow, ]
    }

    if (!is.null(selectCol)) {
      indCol <- grep(paste(selectCol, collapse = "|"), colnames(mat))
      mat <- mat[, indCol]
      mat_pv <- mat_pv[, indCol]
    }


    if (transform == TRUE) {
      mat <- t(mat)
      mat_pv <- t(mat_pv)
    }

    mat[is.na(mat)] <- 0
    mat[!is.finite(mat)] <- 0

    if (symm_matrix == TRUE & (ncol (mat) != nrow(mat))) {
      symm_matrix <- FALSE
      warning(
        "impossible to create symmetrical matrix, number of columns is different from number of rows"
      )

    }


    if (clusterize == TRUE) {
      fit <-
        chooseHclustMet(mat,
                        scale = scale,
                        vecMet = hc.method,
                        distHC = dist.method)
      ind <- fit$labels[fit$order]

      if (symm_matrix == TRUE) {
        mat <- mat[ind, ind]
        mat_pv <- mat_pv[ind, ind]
        fit2 <- NULL

      } else{
        fit2 <-
          chooseHclustMet(t(mat),
                          scale = FALSE,
                          vecMet = hc.method,
                          distHC = dist.method)
        ind2 <- fit2$labels[fit2$order]
        mat <- mat[ind, ind2]
        mat_pv <- mat_pv[ind, ind2]

      }

    } else {
      fit <- NULL
      fit2 <- NULL
    }

    mat_corX <- stats::cor(x = mat, method = "pearson")
    mat_corY <- stats::cor(x = t(mat), method = "pearson")
    mat <- cleanCrosswiseMatrix(
      GM = mat,
      GM_pv = mat_pv ,
      pvcut = pvcut,
      scale = scale,
      subEX = subEX
    )

    if (symm_matrix == TRUE & GM_diag == FALSE) {
      diag(mat) <- 0
    }

    mat1 <-
      list(
        GMat = mat,
        GMat_pv = mat_pv,
        GMat_corX = mat_corX ,
        GMat_corY = mat_corY,
        FitRow = fit,
        FitCol = fit2
      )
    mPT@matrix <- mat1

    return(mPT)

  }
