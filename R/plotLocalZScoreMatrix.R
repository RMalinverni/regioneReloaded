#' Plot Local Z-Score Matrix
#'
#'
#' Plot Local Z-Score Matrix of associations/correlations stored in a multiLocaZScore object.
#'
#' @usage plotLocalZScoreMatrix <- function(mLz, lineColor = NA, interpolate = FALSE, colMatrix = "default", matrix.type = "association" , maxVal = 2, main = "",  revert = FALSE)
#'
#' @param mLz an object of class multiLocaZScore or a matrix
#' @param lineColor logic if TRUE a grid matrix will be draw (default: FALSE)
#' @param interpolate logic the image will be interpolate using the function (\code{\link{geom_raster}}
#' @param colMatrix character or vector of colors, if "default" will be used a default selection see..
#' @param matrix.type character ("association" or "correlation") is the kind of matrix that will be plotted (default = "association")
#' @param maxVal numeric, maximum abs(value) reached by the plot. (default = 2)
#' @param main character, plot title
#' @param size_lab numeric, size for the plot lab
#' @param revert logic, revert the order of the plotted elements
#' @param highlight character vector indicating the regionset names to highlight by adding labels pointing to the 0 position (default = NULL)
#' @param highlight_size numeric, size of the highlight labels
#'
#' @return A plot is created on the current graphics device.
#'
#' @seealso \code{\link{multiLocalZScore}} \code{\link{makeLZMatrix}}
#'
#' @examples
#'
#' data("cw_Alien")
#'
#' plotLocalZscoreMatrix(mlz_Alien_ReG)
#'
#'
#' @import reshape2
#' @import ggplot2
#' @import ggrepel
#' @export plotLocalZScoreMatrix
#'


plotLocalZScoreMatrix <- function(mLz,
                                  lineColor = NA,
                                  interpolate = FALSE,
                                  colMatrix = "default",
                                  matrix.type = "association",
                                  cor = "row",
                                  maxVal = 2,
                                  main = "",
                                  size_lab= 6,
                                  revert = FALSE,
                                  highlight = NULL,
                                  highlight_size = 2.5) {


  if (class(mLz) != "multiLocalZScore") {
    stop("the object mlZA need to be an multiLocalZScore object")
  }

  if (matrix.type == "association") {
    GM <- t(mLz@matrix$LZM)
    title <- "Association Matrix"

    if (is.na(maxVal)){
      maxVal<-quantile(abs(GM),.95)
    }

    if (maxVal=="max"){
      maxVal<-max(c(abs(max(GM)),abs(min(GM))))
    }


    if(revert == TRUE){
      GM <-GM[rev(rownames(GM)),]
    }
  }

  if (matrix.type == "correlation") {
    GM <- mLz@matrix$LZM_cor

    title <- "Correlation Matrix"
    maxVal <- 1
  }

  if (colMatrix == "default") {
    colMatrix <-
      rev(c(rev(brewer.pal(9, "PuBuGn")), brewer.pal(9, "YlOrRd")))

  }

  DF <- melt(GM, varnames = c("X", "Y"))

  if (!is.null(highlight)) {
    DF_label <- DF[DF$Y %in% highlight & DF$X == 0,]
  }

  ggplot(DF, aes(x = X, y = Y)) +

    #geom_raster(aes(fill = value), interpolate = FALSE, color  = "white") +
    geom_tile(aes(fill = value), color = lineColor) +
    scale_fill_gradientn(
      colours = rev(colMatrix),
      limits = c(-maxVal, maxVal),
      oob = scales::squish
    )  +
    geom_label_repel(data = DF_label, aes(label = Y), max.overlaps = Inf, size = highlight_size,
                     min.segment.length = 0, xlim = c(0.4 * max(DF$X), NA),
                     segment.curvature = -0.1,
                     segment.ncp = 3,
                     segment.angle = 20) +
    theme(
      axis.text.x = element_text(
        angle = 90,
        size = 6,
        hjust = 0.95,
        vjust = 0.2
      ),
      axis.text.y = element_text(size = size_lab)
    ) +
    labs(
      subtitle = title,
      title = main,
      caption = mLz@parameters$ranFUN
    )


}




