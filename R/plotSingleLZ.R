#' Plot Single Local Z-Score Test
#'
#'
#' Plot the result of a single local Z-Score test from an mLZ object.
#'
#' @usage plotSingleLZ<-function(mLZ, RS2,  colvec = NA, xlab = " N. of overlaps", limH = NA, main = NA)
#'
#' @param mLZ an object of class multilocalZscore
#' @param RS character, name of regionSet to test.
#' @param xlab character, label for x axes. (default = NA)
#' @param main character, title for the plot. (default = NA)
#'
#' @export plotSingleLZ
#' @import ggplot2
#' @import RColorBrewer


plotSingleLZ <-
  function(mLZ,
           RS,
           xlab = "",
           normZS = TRUE,
           limH = NA,
           main = NA,
           colpal = NULL) {

    if (is.null(colpal)) {
      colpal <- brewer.pal(n = 5, "Set2")
      pal <- colorRampPalette(colpal)
    } else {
      pal <- colorRampPalette(colpal)
    }

    mendel_theme<-theme(panel.background = element_rect(fill = "#F1ECC3",
                                                        colour = "#F1ECC3",
                                                        size = 0.5, linetype = "solid"),
                        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                        colour = "#FDFAF6"),
                        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                                        colour = "#FDFAF6"))

    if (mLZ@parameters$evFUN == "numOverlaps") {
      mLZ@parameters$evFUN <- "N. of overlaps"
    }

    RS <- as.list(RS)
    df<-do.call("rbind", lapply(X=RS, FUN = DFfromLZ, mLZ=mLZ))

    if (normZS) {
      p <- ggplot(df, aes(x = shift, y = normLocalZscore, group = name, fill = name, color = name))
      ylabel <- "Normalized Z-score"
    } else {
      p <- ggplot(df, aes(x = shift, y = lzscore, group = name, fill = name, color = name))
      ylabel <- "Z-score"
    }

    p <- p +
      geom_hline(yintercept=0,  color ="#515E63", size=0.6) +
      geom_vline(xintercept = 0, color ="#515E63", size = 0.4, linetype = "dotted") +
      geom_density(alpha = 0.2, stat = "identity") +
      # geom_line() +
      scale_color_manual(values = pal(length(RS))) +
      scale_fill_manual(values = pal(length(RS))) +
      ylab(ylabel) +
      xlab("bp")

    if (!is.na(limH)) {
      p <- p + ylim(-limH, limH)
    }
    return(p)
  }
