#' Plot Crosswise Matrix
#'
#'
#' Plot matrix of associations/correlations stored in a genoMatriXeR object.
#'
#' @usage plotCrosswiseMatrix(mPT,  lineColor = NA, interpolate = FALSE, colMatrix = "default",
#' matrix_type = "crosswise", cor = "row", main = "")
#'
#' @param mPT an object of class gMXR or a matrix
#' @param lineColor logic if TRUE a grid matrix will be draw (default: FALSE)
#' @param interpolate logic the image will be interpolate using the function (\code{\link{geom_raster}}
#' @param colMatrix character or vector of colors, if "default" will be used a default selection see..
#' @param matrix_type character ("crosswise" or "correlation") is the kind of matrix that will be plotted.
#' @param cor character ("row" or "col"). If matrix variable is "correlation", defined if the function \code{"cor"} will be executed on rows or columns of the matrix
#' @param maxVal numeric, maximun abs(value) reached by the plot. (default = 2)
#' @param main charachter, graph title
#' @param ord_mat numeric list, list with two vectors X and Y as element, that
#' represent the order of rows and column of the matrix (default = NULL)
#'
#' @return A plot is created on the current graphics device.
#'
#' @seealso \code{\link{crosswisePermTest}} \code{\link{makeCrosswiseMatrix}}
#'
#' @examples
#' data("cw_Alien")
#'
#' cw_Alien_ReG <- makeCrosswiseMatrix( cw_Alien_ReG)
#'
#' plotCrosswiseMatrix(cw_Alien_ReG, matrix_type = "crosswise")
#'
#' plotCrosswiseMatrix(cw_Alien_ReG, matrix_type = "correlation")
#'
#' @import reshape2
#' @import ggplot2
#' @import RColorBrewer
#' @export plotCrosswiseMatrix
#'
#'
#'

plotCrosswiseMatrix <- function(mPT,
                       lineColor = NA,
                       interpolate = FALSE,
                       colMatrix = "default",
                       matrix_type = "crosswise",
                       cor = "row",
                       maxVal = NA,
                       main = "",
                       ord_mat=NULL) {

  if (class(mPT) == "genoMatriXeR") {

    if  (matrix_type == "crosswise") {
      GM <- mPT@matrix$GMat
      title <- "Association Matrix"

      if (is.na(maxVal)){
          maxVal<-quantile(abs(GM),.95)
      }

      if (maxVal=="max"){
        maxVal<-max(c(abs(max(GM)),abs(min(GM))))
      }

    }

    if  (matrix_type == "correlation") {
      if (cor == "row") {
        GM <- mPT@matrix$GMat_corX
      }
      if (cor == "col") {
        GM <- mPT@matrix$GMat_corY
      }
      title <- "Correlation Matrix"
      maxVal<-1
    }

  }


  if (is.matrix(mPT)){
    GM <- mPT
    if (is.na(maxVal)){
      maxVal<-max(c(abs(max(GM)),abs(min(GM))))
    }
  }

  if ( colMatrix == "default" ){

    colMatrix<-rev( c( rev(brewer.pal( 9, "PuBuGn" ) ),brewer.pal( 9, "YlOrRd" ) ) )

    }


  if (!is.null(ord_mat)){
    if (is.list(ord_mat)){
      if(length(ord_mat)==2){
        GM<-GM[ord_mat[[1]],ord_mat[[2]]]
      }
    }
  }


  DF <- reshape2::melt(GM, varnames = c("X", "Y"))

  if (interpolate == FALSE) {
    ggplot2::ggplot(DF, ggplot2::aes_string(x = "X", y = "Y")) +
      ggplot2::geom_tile(ggplot2::aes_string(fill = "value"), color = lineColor) +
      ggplot2::scale_fill_gradientn(
        colours = rev(colMatrix),
        limits = c(-maxVal, maxVal),
        oob = scales::squish
      )  +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(
          angle = 90,
          size = 6,
          hjust = 0.95,
          vjust = 0.2
        ),
        axis.text.y = ggplot2::element_text(size = 6)
      ) +
      ggplot2::labs(subtitle = title, title=main, caption = mPT@parameters$ranFUN) +
      ggplot2::coord_equal()
  } else{
    ggplot2::ggplot(DF, ggplot2::aes_string(x = "X", y = "Y")) +
      ggplot2::geom_raster(ggplot2::aes_string(fill = "value"), interpolate = TRUE) +
      ggplot2::scale_fill_gradientn(
        colours = rev(colMatrix),
        limits = c(-maxVal, maxVal),
        oob = scales::squish
      )  +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(
          angle = 90,
          size = 6,
          hjust = 0.95,
          vjust = 0.2
        ),
        axis.text.y = ggplot2::element_text(size = 6)
      ) +
      ggplot2::labs(subtitle =  title, title=main,caption = mPT@parameters$ranFUN) +
       ggplot2::coord_equal()
  }
}


