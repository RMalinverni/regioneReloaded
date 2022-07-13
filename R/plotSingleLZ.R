#' Plot Single Local Z-Score Test
#'
#'
#' Plot the result of a single local Z-Score test from an mLZ object.
#'
#' @usage plotSingleLZ <- function(mLZ, RS, xlab = "", normZS = TRUE, ylim = NA, main = NA, colpal = NULL, labValues = TRUE, label_size = 2.5)
#'
#' @param mLZ an object of class multiLocalZscore.
#' @param RS character, vector of regionSet names to for which to plot the local Z-score results.
#' @param xlab character, label for the x axis. (default = NA)
#' @param main character, title for the plot. (default = NA)
#' @param normZS logical, indicates whether the normalized Z-score values should be plotted. If FALSE, the raw
#' Z-score is used (default = TRUE).
#' @param ylim numeric, vector with minum and maximum Y values of the plot. If NULL, the plot limits are set by default so all data points can be plotted. (default = NULL)
#' @param labValues logical, if TRUE each local Z-score profile is labelled at position 0 with the name of the regionset
#' and its Z-score value at the central position. (default = TRUE)
#' @param label_size numerical, size of the labels from labValues in the plot. (default = 2.5)
#' @param colpal character vector of custom colors to use as palette source for the plot. If NULL, predetermined
#' colors from RColorBrewer Set2 palette are used.
#' @return A plot is created on the current graphics device.
#'
#' @seealso \code{\link{multiLocalZScore}} \code{\link{makeLZMatrix}}
#'
#' @examples
#'
#' data("cw_Alien")
#'
#' plotSingleLZ(mlz_Alien_Reg, RS = "regAB")
#'
#'
#' @export plotSingleLZ
#' @import ggplot2
#' @import RColorBrewer
#' @import ggrepel

plotSingleLZ <-
  function(mLZ,
           RS,
           xlab = "",
           normZS = TRUE,
           ylim = NULL,
           main = NA,
           colpal = NULL,
           labValues = TRUE,
           label_size = 2.5) {

    if(!hasArg(mLZ)) {
      stop("mLZ is missing")
    } else if (class(mLZ) != "multiLocalZScore") {
      stop("mLZ needs to be a multiLocalZScore object")
    } else if (!hasArg(RS)) {
      stop("RS is missing")
    } else if (!(all(RS %in% names(mLZ@multiLocalZscores$shifed_ZSs)))) {
      stop("One or more elements in RS do not match region set names in mLZ")
    }

    RS <- as.list(RS)
    df<-do.call("rbind", lapply(X=RS, FUN = DFfromLZ, mLZ=mLZ))

    ref <- mLZ@parameters$A
    evfun <- mLZ@parameters$evFUN
    ranfun <- mLZ@parameters$ranFUN

    if (is.null(colpal)) { # Palette
      colpal <- brewer.pal(n = 5, "Set2")
      pal <- colorRampPalette(colpal)
    } else {
      pal <- colorRampPalette(colpal)
    }

    if (mLZ@parameters$evFUN == "numOverlaps") {
      mLZ@parameters$evFUN <- "N. of overlaps"
    }

    if (normZS) { # Raw or norm ZS
      df$score <- df$normLocalZscore
      ylabel <- "Normalized Z-score"
    } else {
      df$score <- df$lzscore
      ylabel <- "Z-score"
    }

    # Plot
    p <- ggplot(df, aes(x = shift, y = score, group = name, fill = name, color = name)) +
      geom_hline(yintercept=0,  color ="#515E63", size=0.6) +
      geom_vline(xintercept = 0, color ="#515E63", size = 0.4, linetype = "dotted") +
      geom_density(alpha = 0.2, stat = "identity") +
      # geom_line() +
      scale_color_manual(values = pal(length(RS))) +
      scale_fill_manual(values = pal(length(RS))) +
      labs(title = ref,
           subtitle = paste("ranFUN: ", ranfun, "\nevFUN: ", evfun),
           y = ylabel,
           x = "bp") +
      theme(legend.title = element_blank())

    # Labels
    if(labValues) {
      df_label <- df[df$shift == 0,]
      df_label$text <- paste(df_label$name, "\nZS: ", round(df_label$score, digits = 2), sep = "")
      p <- p +
        geom_label_repel(data = df_label, inherit.aes = FALSE,
                         aes(label = text, x = shift, y = score, color = name),
                         fill = "#FDFAF6", size = label_size,
                         xlim = c(0.2 * max(df$shift), NA),
                         show.legend = FALSE)
    }

    # Ylims
    if (!is.null(ylim)) {
      p <- p + coord_cartesian(ylim = ylim)
    }
    p <- p + mendel_theme()
    return(p)
  }
