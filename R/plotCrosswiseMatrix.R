#' Plot Crosswise Matrix
#'
#'
#' Plot matrix of associations/correlations stored in a gMXR object.
#'
#' @usage plotCrosswiseMatrix(mPt,  lineColor = NA, interpolate = FALSE, colMatrix = "default", matrix.type = "crosswise", cor = "row", main = "")
#'
#' @param mPt an object of class gMXR or a matrix
#' @param lineColor logic if TRUE a grid matrix will be draw (default: FALSE)
#' @param interpolate logic the image will be interpolate using the function (\code{\link{geom_raster}}
#' @param colMatrix character or vector of colors, if "default" will be used a default selection see..
#' @param matrix.type character ("crosswise" or "correlation") is the kind of matrix that will be plotted.
#' @param cor character ("row" or "col"). If matrix variable is "correlation", defined if the function \code{"cor"} will be executed on rows or columns of the matrix
#' @param maxVal numeric, maximun abs(value) reached by the plot. (default = 2)
#' @param main charachter, graph title
#'
#' @import reshape2
#' @import ggplot2
#' @import RColorBrewer
#' @export plotCrosswiseMatrix
#'

plotCrosswiseMatrix <- function(mPt,
                       lineColor = NA,
                       interpolate = FALSE,
                       colMatrix = "default",
                       matrix.type = "crosswise",
                       cor = "row",
                       maxVal = 2,
                       main = "") {

  if (class(mPt) == "genoMatriXeR") {

    if  (matrix.type == "crosswise") {
      GM <- mPt@matrix$GMat
      title <- "Association Matrix"
      if (is.na(maxVal)){
          maxVal<-max(c(abs(max(GM)),abs(min(GM))))
      }
    }

    if  (matrix.type == "correlation") {
      if (cor == "row") {
        GM <- mPt@matrix$GMat_corX
      }
      if (cor == "col") {
        GM <- mPt@matrix$GMat_corY
      }
      title <- "Correlation Matrix"
      maxVal<-1
    }

  }

  if (is.matrix(mPt)){
    GM <- mPt
    if (is.na(maxVal)){
      maxVal<-max(c(abs(max(GM)),abs(min(GM))))
    }
  }

  if ( colMatrix == "default" ){

    colMatrix<-rev( c( rev(brewer.pal( 9, "PuBuGn" ) ),brewer.pal( 9, "YlOrRd" ) ) )

    }

  DF <- melt(GM, varnames = c("X", "Y"))

  if (interpolate == FALSE) {

    ggplot(DF, aes(x = X, y = Y)) +

      #geom_raster(aes(fill = value), interpolate = FALSE, color  = "white") +
      geom_tile(aes(fill = value), color = lineColor) +
      scale_fill_gradientn(
        colours = rev(colMatrix),
        limits = c(-maxVal, maxVal),
        oob = scales::squish
      )  +
      theme(
        axis.text.x = element_text(
          angle = 90,
          size = 6,
          hjust = 0.95,
          vjust = 0.2
        ),
        axis.text.y = element_text(size = 6)
      ) +
      labs(subtitle = title, title=main, caption = mPt@parameters$ranFUN) +
      coord_equal()

  } else{

    ggplot(DF, aes(x = X, y = Y)) +

      geom_raster(aes(fill = value), interpolate = TRUE) +
      #geom_tile(aes(fill = value), color = "withe") +
      scale_fill_gradientn(
        colours = rev(colMatrix),
        limits = c(-maxVal, maxVal),
        oob = scales::squish
      )  +
      theme(
        axis.text.x = element_text(
          angle = 90,
          size = 6,
          hjust = 0.95,
          vjust = 0.2
        ),
        axis.text.y = element_text(size = 6)
      ) +
      labs(subtitle =  title, title=main,caption = mPt@parameters$ranFUN) +
       coord_equal()

  }
}


