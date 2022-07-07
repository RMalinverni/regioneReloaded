#' Make Crosswise Matrix
#'
#'
#' Accept as input a gMXR object an create in the slot Matrix all the fields.
#'
#' @usage makeCrosswiseMatrix( mPt, clusterize = TRUE, hc.method = NA,dist.method = "euclidean",transform = FALSE,
#'                            scale = FALSE, zs.type = 'norm_zscore', symm_matrix = TRUE, selectVec = NULL,
#'                            maxNZS = 100, pvcut = 0.05, ...)
#'
#' @param mPt an object of class gMXR or a matrix
#' @param clusterize logic, if TRUE the matrix will be clusterize using a method selected with the variable \code{hc.method} (default = TRUE)
#' @param hc.method character, select the \code{hclust} method to use for clusterize the matrix,
#' if hc.method == NA, the methods used for clusterize the matrix will be selected using the function \code{\link{chooseHclustMet}} (default = "NA")
#' @param dist.method character, metric used to calculate the distance matrix see \code{hclust} (default = "euclidean")
#' @param transform logic, if TRUE the matrix will be transform using the function \code{("t")} (default = FALSE)
#' @param scale logic, if TRUE the matrix will be scaled (default = FALSE)
#' @param zs.type character, choose if create the matrix using every column from MultiOverlap slot of gMXR object default: 'norm_zscore'
#' @param symm_matrix logic, if TRUE the matrix will be treated as symmetrical (same clusterization for row and column). (default = TRUE)
#' @param selectVec vector, teh matrix will be reduced using only the row/column content. (default = NULL)
#' @param pvcut maximum adj.pvalue accepted, all the associations with a adj.pvalue (defined in \code{\link{crosswisePermTest}}) higher than pvcut were transformed in 0. (default = 0.05)
#' @param ... urther arguments to be passed to other methods.
#'
#' @import stats
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
           selectVec = NULL,
           pvcut = 1,
           subEx=0,
           ...) {

  if ( class( mPT ) == "genoMatriXeR" ){

    mat <- crosswiseMatrix( mPT, zs.type = zs.type )
    mat_pv <- crosswiseMatrix( mPT, zs.type = "adj.p_value" )

  } else { stop(' mPT need to be a "genoMatriXeR" class object ') }


  if (!is.null(selectVec)){

    if (is.vector(selectVec)){

      indCol<-grep(paste(selectVec,collapse = "|"), colnames(mat))
      indRow<-grep(paste(selectVec,collapse = "|"), rownames(mat))
      mat <- mat[indRow,indCol]
      mat_pv<- mat_pv[indRow,indCol]

      }else{ stop(' selectVec need to be a vectors of column and/or row names')}
  }

  #mat <-  normGenMat(mat)
  if( transform == TRUE ){

    mat <- t( mat )
    mat_pv <- t( mat_pv )
  }

  mat[ is.na(mat) ] <- 0
  mat[ !is.finite(mat) ] <- 0

  if ( symm_matrix == TRUE & ( ncol (mat ) != nrow( mat ) ) ){

    symm_matrix <- FALSE
    warning( "impossible to create symmetrical matrix, number of matrix columns is different from number of rows" )

  }

  if ( clusterize == TRUE ){

    fit <- chooseHclustMet(mat, scale = scale,vecMet = hc.method,  distHC = dist.method)
    ind <- fit$labels[fit$order]

    if ( symm_matrix == TRUE ){

      mat<- mat[ ind, ind ]
      mat_pv <- mat_pv[ind,ind]
      fit2 <- NULL

    }else{

      fit2 <- chooseHclustMet(t(mat), scale = FALSE,vecMet = hc.method,  distHC = dist.method )
      ind2 <- fit2$labels[fit2$order]
      mat <- mat[ ind,ind2 ]
      mat_pv <- mat_pv[ind,ind2]

    }

  } else {
    fit = NULL
    fit2 = NULL
  }

  mat_corX <- cor(x = mat, method = "pearson")
  mat_corY <- cor(x = t(mat), method = "pearson")
  mat <- cleanCrosswiseMatrix(GM = mat,GM_pv = mat_pv ,
                              pvcut = pvcut,scale = scale,
                              subEX =subEX)
  mat1 <- list( GMat = mat, GMat_pv = mat_pv, GMat_corX = mat_corX , GMat_corY = mat_corY, FitRow = fit, FitCol = fit2)
  mPT@matrix <- mat1

  return( mPT )

}
