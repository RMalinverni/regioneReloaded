#' Make Crosswise Matrix
#'
#'
#' Accept as input a gMXR object an create in the slot Matrix all the fields.
#'
#' @usage makeCrosswiseMatrix( mPT, clusterize = TRUE, hc.method = NULL, dist.method = "euclidean",
#' transform = FALSE, scale = FALSE, zs.type = 'norm_zscore', symm_matrix = TRUE,
#' selectVec = NULL, pvcut = 1, GM_diag = NULL, subEX=0, ...)
#'
#' @param mPT an object of class gMXR or a matrix
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
#' @param subEX value used to substitute the z-score when they don't pass the pvalue test
#' @param GM_diag value if not NULL GE_diag value will replace the association value of a diagonal of the association matrix. (default = NULL)
#' @param ... further arguments to be passed to other methods.
#'
#' @return
#'
#' A object of class \code{genoMatriXeR} containing three slots
#'
#' \itemize{
#' \item \bold{\code{@parameters}}
#' \item \bold{\code{@multioverlaps}}
#' \item \bold{\code{@matrix}}
#'
#' }
#'
#' @examples
#'
#' data(cw_Alien)
#'
#' cw_Alien_ReG <- makeCrosswiseMatrix(cw_Alien_ReG)
#'
#' summary(cw_Alien_ReG)
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
           GM_diag = NULL,
           subEX=0,
           ...) {

  if ( class( mPT ) == "genoMatriXeR" ){

    mat <- crosswiseMatrix( mPT, zs.type = zs.type )
    mat_pv <- crosswiseMatrix( mPT, zs.type = "adj.p_value" )

  } else { stop(' mPT need to be a "genoMatriXeR" class object ') }


  if (!is.null(selectVec)){

    if (is.vector(selectVec)){

      if (length(selectVec) <= 2){

        if (length(selectVec) == 1){
          sVrow <- selectVec
          sVcol <- selectVec
        }

        if (length(selectVec) == 2){
          sVrow <- selectVec[1]
          sVcol <- selectVec[2]
        }

      indCol<-grep(paste(sVcol, collapse = "|"), colnames(mat))
      indRow<-grep(paste(sVrow, collapse = "|"), rownames(mat))
      mat <- mat[indRow,indCol]
      mat_pv<- mat_pv[indRow,indCol]

      }else{ warning( "selectVec need to be a vector of 1 or 2 elements, selectVec will not be use for this analysis")}

    }else{ warning( "selectVec need to be a vector of 1 or 2 elements, selectVec will not be use for this analysis")}
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

  if (symm_matrix == TRUE & !is.null(GM_diag)){
    diag(mat) <- GM_diag
  }

  mat1 <- list( GMat = mat, GMat_pv = mat_pv, GMat_corX = mat_corX ,
                GMat_corY = mat_corY, FitRow = fit, FitCol = fit2)
  mPT@matrix <- mat1

  return( mPT )

}
