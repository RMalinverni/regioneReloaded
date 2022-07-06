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


plotSingleLZ <-
  function(mLZ,
           RS,
           xlab = "",
           limH = NA,
           main = NA) {

    mendel_theme <-
      theme(
        panel.background = element_rect(
          fill = colvec[2],
          colour = colvec[2],
          size = 0.5,
          linetype = "solid"
        ),
        panel.grid.major = element_line(
          size = 0.5,
          linetype = 'solid',
          colour = "#FDFAF6"
        ),
        panel.grid.minor = element_line(
          size = 0.25,
          linetype = 'solid',
          colour = "#FDFAF6"
        )
      )

    colvec <-
      c("#57837B", "#F1ECC3", "#C9D8B6", "#515E63", "#C05555")

    if (mLZ@parameters$evFUN == "numOverlaps") {
      mLZ@parameters$evFUN <- "N. of overlaps"
    }


    RS <- as.list(RS)
    df<-do.call("rbind", lapply(X=RS, FUN = DFfromLZ, mLZ=mLZ))



    p <- ggplot(df, aes(x = shift, y = normLocalZsore, group_by=name)) +
      geom_line(size = 2, col = colvec[5]) +
      geom_area(aes(fill = colvec[2])) +
      geom_vline(
        aes(xintercept = 0),
        color = colvec[4],
        linetype = "dashed",
        size = 0.4
      ) +
      ylab("normalized z-score") +
      xlab("bp") +
      labs(
        title = nameRS ,
        subtitle = paste0("Local z-score:  ", nameRS, " vs ", RS2),
        caption = paste0("Original z-score :  ", zs, " - Original adj.pvalue :  ", pv)
      ) +
      mendel_theme +
      scale_fill_discrete(name = nameRS, labels = RS2)

    if (!is.na(limH)) {
      p <- p + ylim(-limH, limH)
    }
    return(p)
  }
