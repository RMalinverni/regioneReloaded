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
           limH = NA,
           main = NA) {

    colvec<-c("#57837B","#F1ECC3","#C9D8B6","#515E63","#C05555")
    colpal <- brewer.pal(n = 8, "Set2")
    pal <- colorRampPalette(colpal)

    mendel_theme<-theme(panel.background = element_rect(fill = colvec[2],
                                                        colour = colvec[2],
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

    p <- ggplot(df, aes(x = shift, y = normLocalZsore, group_by=name, fill = name, color = name)) +
      # hline at y = 0
      geom_hline(yintercept=0,  color =colvec[4], size=0.6) +
      # vline at x = 0
      geom_vline(xintercept = 0, color =colvec[4], size = 0.4, linetype = "dotted") +
      geom_area(alpha = 0.35) +
      scale_color_manual(values = pal(length(RS))) +
      scale_fill_manual(values = pal(length(RS))) +
      ylab("normalized z-score") +
      xlab("bp")
      # labs(subtitle = paste0("Local z-score:  ", nameRS, " vs ", RS2),
      #      caption = paste0("Original z-score :  ", zs, " - Original adj.pvalue :  ", pv)
      # ) +

    if (!is.na(limH)) {
      p <- p + ylim(-limH, limH)
    }
    return(p)
  }
