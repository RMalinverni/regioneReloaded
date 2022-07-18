#' Plot Local Z-Score Matrix
#'
#'
#' Plot Local Z-Score Matrix of associations/correlations stored in a multiLocaZScore object.
#'
#' @usage plotLocalZScoreMatrix (mLZ, lineColor = NA, colMatrix = "default",
#' matrix.type = "association" , maxVal = 2, main = "", labSize= 6, revert = FALSE,
#' highlight = NULL, highlight_size = 2.5)
#'
#' @param mLZ an object of class multiLocaZScore or a matrix
#' @param lineColor logic if TRUE a grid matrix will be draw (default: FALSE)
#' @param colMatrix character or vector of colors, if "default" will be used a default selection see..
#' @param matrix.type character ("association" or "correlation") is the kind of matrix that will be plotted (default = "association")
#' @param maxVal numeric, maximum abs(value) reached by the plot. (default = 2)
#' @param main character, plot title
#' @param labSize numeric, size for the plot lab
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
#' # plotLocalZScoreMatrix(mLZ_Alien_ReG)
#'
#'
#' @import reshape2
#' @import ggplot2
#' @import ggrepel
#' @import stats
#' @importFrom RColorBrewer brewer.pal
#' @importFrom methods hasArg
#' @export plotLocalZScoreMatrix
#'


plotLocalZScoreMatrix <- function(mLZ,
                                  lineColor = NA,
                                  colMatrix = "default",
                                  matrix.type = "association",
                                  maxVal = 2,
                                  main = "",
                                  labSize= 6,
                                  revert = FALSE,
                                  highlight = NULL,
                                  highlight_size = 2.5) {

  if (!methods::hasArg(mLZ)) {
    stop("mLZ is missing")
  } else if (class(mLZ) != "multiLocalZScore") {
    stop("the object mLZA needs to be an multiLocalZScore object")
  }

  if (matrix.type == "association") {
    GM <- t(mLZ@matrix$LZM)
    title <- "Association Matrix"

    if (is.na(maxVal)){
      maxVal<- stats::quantile(abs(GM),.95)
    }

    if (maxVal=="max"){
      maxVal<-max(c(abs(max(GM)),abs(min(GM))))
    }


    if(revert == TRUE){
      GM <-GM[rev(rownames(GM)),]
    }
  }

  if (matrix.type == "correlation") {
    GM <- mLZ@matrix$LZM_cor

    title <- "Correlation Matrix"
    maxVal <- 1
  }

  if (colMatrix == "default") {
    colMatrix <-
      rev(c(rev(RColorBrewer::brewer.pal(9, "PuBuGn")), RColorBrewer::brewer.pal(9, "YlOrRd")))

  }

  DF <- reshape2::melt(GM, varnames = c("X", "Y"))

  p <- ggplot2::ggplot(DF, ggplot2::aes_string(x = "X", y = "Y")) +

    #geom_raster(aes(fill = value), interpolate = FALSE, color  = "white") +
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
      axis.text.y = ggplot2::element_text(size = labSize)
    ) +
    ggplot2::labs(
      subtitle = title,
      title = main,
      caption = mLZ@parameters$ranFUN
    )

  if (!is.null(highlight)) {
    DF_label <- DF[DF$Y %in% highlight & DF$X == 0,]
    p <- p + ggrepel::geom_label_repel(data = DF_label, ggplot2::aes_string(label = "Y"), max.overlaps = Inf, size = highlight_size,
                              min.segment.length = 0, xlim = c(0.4 * max(DF$X), NA),
                              segment.curvature = -0.1,
                              segment.ncp = 3,
                              segment.angle = 20)
  }
  return(p)
}




