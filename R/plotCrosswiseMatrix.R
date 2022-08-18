#' plotCrosswiseMatrix
#'
#' @description
#'
#' Plot matrix of associations/correlations stored in a [genoMatriXeR][genoMatriXeR-class] object.
#'
#' @details
#'
#' This functions creates a graphical representation of the matrix of associations
#' stored in a [genoMatriXeR][genoMatriXeR-class] object. The values plotted and
#' clustering options can be controlled when creating the matrix with the function
#' [makeCrosswiseMatrix].
#'
#' @usage plotCrosswiseMatrix(mPT, lineColor = NA, interpolate = FALSE, colMatrix =
#' "default", matrix_type = "association", cor = "row",
#' maxVal = NA, main = "", ord_mat = NULL)

#'
#' @param mPT an object of class [genoMatriXeR][genoMatriXeR-class] or a numerical matrix.
#' @param lineColor logical, color for the line grid delineating the tiles of the matrix plot. If NA, no line will be drawn. (default = NA)
#' @param interpolate logical, if TRUE the image will be interpolated using the function [geom_raster()]. (default = FALSE)
#' @param colMatrix character or vector of colors, if "default" will be used a default selection see..
#' @param matrix_type character, type of matrix to be plotted, either "association" or "correlation". (default = "association")
#' @param cor character, if `matrix_type` is "correlation", choose if the function [cor()] will be executed on each "row" or "col" of the matrix. (default = "row")
#' @param maxVal numeric, maximum absolute value displayed by the plot. If "max", the maximum values in the matrix are used. If NA, the 0.95 quantile of all absolute values is used. (default = NA)
#' @param main character, title of the plot. (default = "")
#' @param ord_mat numeric, list with two numeric vectors that represent the ordering of rows and column of the matrix to be used in the plot.
#' If NULL, the order of the matrix is preserved as is. (default = NULL)
#'
#' @return Returns a ggplot object.
#'
#' @seealso [crosswisePermTest] [makeCrosswiseMatrix]
#'
#' @examples
#'
#' data("cw_Alien")
#'
#' cw_Alien_ReG <- makeCrosswiseMatrix( cw_Alien_ReG)
#'
#' plotCrosswiseMatrix(cw_Alien_ReG, matrix_type = "association")
#'
#' plotCrosswiseMatrix(cw_Alien_ReG, matrix_type = "correlation")
#'
#'
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom methods hasArg
#' @importFrom methods is
#' @importFrom RColorBrewer brewer.pal
#'
#' @export plotCrosswiseMatrix
#'

plotCrosswiseMatrix <- function(mPT,
                       lineColor = NA,
                       interpolate = FALSE,
                       colMatrix = "default",
                       matrix_type = "association",
                       cor = "row",
                       maxVal = NA,
                       main = "",
                       ord_mat=NULL) {

  # Check mPT object
  stopifnot("mPT is missing" = methods::hasArg(mPT))
  stopifnot("mPT needs to be a genoMatriXeR object or a numeric matrix" = {
    methods::is(mPT, "genoMatriXeR") | methods::is(mPT, "matrix")
  })
  stopifnot("The matrix slot of mPT is empty, run first makeCrosswiseMatrix()" = !is.null(mPT@matrix[[1]]))

  # Check arguments
  stopifnot("Invalid matrix_type, choose 'association' or 'correlation'" = matrix_type %in% c("association", "correlation"))
  stopifnot("Invalid cor value, choose 'row' or 'col'" = cor %in% c("row", "col"))
  stopifnot("maxVal has to be a numerical value, 'max' or NA" = {
    is.na(maxVal) | methods::is(maxVal, "numeric") | maxVal == "max"
  })

  if (methods::is(mPT,"genoMatriXeR")) {

    if  (matrix_type == "association") {
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

  if (length(colMatrix) == 1) {
    if (colMatrix == "default") {
      colMatrix <-
        rev(c(rev(RColorBrewer::brewer.pal(9, "PuBuGn")), RColorBrewer::brewer.pal(9, "YlOrRd")))

    }
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


