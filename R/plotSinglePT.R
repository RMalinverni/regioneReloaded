#' Plot Single Permutation Test
#'
#'
#' Plot the result of a single permutation test from an genomicRangersProject object.
#'
#' @usage plotSinglePT<-function(mPT, RS1, RS2, xlab =NA, main=NA,add_theme = FALSE,colvec = NULL)
#'
#' @param mPt an object of class genoMatriXeR or a matrix
#' @param RS1 character, name of the first element of gMXR object to test.
#' @param RS2 character, name of the second element of gMXR object to test.
#' @param xlab character, label for x axes. (default = NA)
#' @param main character, title for the plot. (default = NA)
#' @param add_theme logic, if TRUE will add a default \code{\link{ggplot2}} theme. (default = FALSE)
#' @param colvec vector, vector of colors used in the plot, if NULL it will choose a default vector. (default = NULL)
#' @examples
#'
#' data("cw_Alien")
#' plotSinglePT(cw_Alien_ReG, RS1 = "regA","regA05")
#' plotSinglePT(cw_Alien_ReG, RS1 = "regA","regC")
#'
#' @export plotSinglePT
#' @import ggplot2


plotSinglePT <-
  function(mPT,
           RS1,
           RS2,
           xlab = NA,
           main = NA,
           add_theme = FALSE,
           colvec = NULL) {

    if (class(mPT) != "genoMatriXeR") {
      stop('mPT needs to be a "gMXR" class object')
    }

    if (is.null(colvec)) {
      colvec <- c("#57837B", "#F1ECC3", "#C9D8B6", "#515E63", "#C05555")
    }

    if (is.na(xlab) & mPT@parameters$evFUN == "numOverlaps") {
      xlab = "N of overlaps"
    }

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

    tab <- mPT@multiOverlaps[[RS1]]
    n <- grep(paste0("^", RS2, "$"), tab$name)
    mean.1 <- tab$mean_perm_test[n]
    sd.1 <- tab$sd_perm_test[n]
    max_curve <-
      max(density(rnorm(1:1000, mean = mean.1, sd = sd.1))$y)
    zstart <- mean.1 - 4 * sd.1
    zend   <-   mean.1  + 4 * sd.1
    zs1 <- mean.1 + 1 * sd.1
    zs1neg <- mean.1 - 1 * sd.1
    zs2 <- mean.1 + 2 * sd.1
    zs2neg <- mean.1 - 2 * sd.1
    zs3 <- mean.1 + 3 * sd.1
    zs3neg <- mean.1 - 3 * sd.1
    vec_slices <- c(zs1, zs1neg, zs2, zs2neg, zs3, zs3neg)

    nov <- tab$n_overlaps[n]
    splot <- zstart
    eplot <- zend
    if (zstart > 0) {
      splot <- 0
    }
    if (nov > zend) {
      eplot <- nov
    }
    if (is.na(main)) {
      main <- deparse(substitute(mPT))
    }
    p <- ggplot(data.frame(x = c(splot, eplot)), aes(x)) +
      labs(
        title = main,
        subtitle = paste0("PermTest:  ", RS1, " vs ", RS2),
        caption = paste0("Number of Permutations:  ", mPT@parameters$ntimes)
      ) +
      xlab(paste0(xlab, " on ", tab$n_regionA[n], " regions")) +
      ylab("Freq") +
      stat_function(
        fun = dnorm,
        geom = "area",
        fill = alpha(colvec[1], alpha = 0.5),
        args = list(mean = mean.1, sd = sd.1)
      ) +
      stat_function(
        fun = dnorm,
        geom = "area",
        xlim = c(vec_slices[1], zend),
        fill = alpha(colvec[1], alpha = 0.5),
        args = list(mean = mean.1, sd = sd.1)
      ) +
      stat_function(
        fun = dnorm,
        geom = "area",
        xlim = c(vec_slices[3], zend),
        fill = alpha(colvec[1], alpha = 0.5),
        args = list(mean = mean.1, sd = sd.1)
      ) +
      stat_function(
        fun = dnorm,
        geom = "area",
        xlim = c(vec_slices[5], zend),
        fill = alpha(colvec[1], alpha = 0.5),
        args = list(mean = mean.1, sd = sd.1)
      ) +
      stat_function(
        fun = dnorm,
        geom = "area",
        xlim = c(zstart, vec_slices[2]),
        fill = alpha(colvec[1], alpha = 0.5),
        args = list(mean = mean.1, sd = sd.1)
      ) +
      stat_function(
        fun = dnorm,
        geom = "area",
        xlim = c(zstart, vec_slices[4]),
        fill = alpha(colvec[1], alpha = 0.5),
        args = list(mean = mean.1, sd = sd.1)
      ) +
      stat_function(
        fun = dnorm,
        geom = "area",
        xlim = c(zstart, vec_slices[6]),
        fill = alpha(colvec[1], alpha = 0.5),
        args = list(mean = mean.1, sd = sd.1)
      ) +

      # hline at y = 0
      geom_hline(yintercept = 0,
                 color = colvec[4],
                 size = 0.6) +

      # vline at x = 0
      geom_vline(
        xintercept = 0,
        color = colvec[4],
        size = 0.4,
        linetype = "dotted"
      ) +

      # Random overlaps
      geom_vline(
        xintercept = c(mean.1),
        color = colvec[4],
        linetype = "dashed",
        size = 0.4
      ) +

      # Observed overlaps
      geom_vline(
        aes(xintercept = nov),
        color = colvec[5],
        linetype = "dashed",
        size = 0.4
      ) +

      # Arrow between random and observed
      geom_segment(
        aes(
          x = mean.1,
          y = max_curve / 2,
          xend = nov,
          yend = max_curve / 2
        ),
        linetype = "dashed",
        color = colvec[4],
        arrow = arrow(length = unit(0.5, "cm"))
      ) +

      # Text labels
      # Box with ZS and adjpv
      annotate(
        "label",
        x = mean.1 + ((nov - mean.1) / 2),
        y = max_curve * 0.36,
        label = paste(
          "z-score",
          tab$z_score[n],
          "adj.p-value",
          tab$adj.p_value[n],
          sep = "\n"
        ),
        size = 3,
        fill = "#FDFAF6"
      ) +
      # Box with nZS and sZS
      annotate(
        "label",
        x = eplot * 0.9,
        y = max_curve * 0.9,
        size = 3,
        fill = "#FDFAF6",
        label = paste(
          paste0("Normal ZScore: ", round(tab$norm_zscore[n], digits = 2)),
          paste0("Standard ZScore: ", round(tab$std_zscore[n], digits = 2)),
          sep = "\n"
        )
      ) +
      # Ranfun used
      annotate(
        "text",
        x = eplot * 0.5,
        y = max_curve * 0.99,
        label = mPT@parameters$ranFUN
      ) +
      # Observed and mean random overlaps
      annotate(
        "label",
        x = nov,
        y = max_curve * 0.03,
        size = 3,
        label = nov,
        col = colvec[5],
        hjust = 0.5,
        fill = "#FDFAF6"
      ) +
      annotate(
        "label",
        x = mean.1,
        y = -max_curve * 0.03,
        size = 3,
        label = round(mean.1, digits = 2),
        col = colvec[4],
        hjust = 0.5,
        fill = "#FDFAF6"
      ) +

      if (add_theme == TRUE) {
        p <- p + mendel_theme
      }

    return(p)
  }
