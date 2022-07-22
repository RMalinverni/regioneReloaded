#' plotSinglePT
#'
#' @description
#'
#' Plot the result of a single pairwise permutation test from a genoMatriXeR object.
#'
#' @details
#'
#' This function generates a plot representing the result of a single
#' permutation test stored in a genoMatriXeR object. This includes a plot of the
#' density distribution of the randomized evaluations and a vertical line
#' showing the observed evaluation in the original region set. The values of the
#' mean randomized evaluations and the value of the observed evaluation are
#' shown, in addition to the calculated Z-score, normalized Z-score and adjusted
#' p-value.
#'
#' @usage plotSinglePT(mPT, RS1, RS2, xlab = NA, main = NA)
#'
#' @param mPT an object of class genoMatriXeR
#' @param RS1,RS2 character, names of region sets in genoMatriXeR object for which to represent the pairwise permutation test results.
#' @param xlab character, label for x axis (default = NA)
#' @param main title for the plot, if NA the name of the genoMatriXeR object is used (default = NA)
#'
#' @return Returns a ggplot object.
#'
#' @seealso \code{\link{crosswisePermTest}} \code{\link{makeCrosswiseMatrix}}
#'
#' @examples
#'
#' data("cw_Alien")
#' plotSinglePT(cw_Alien_ReG, RS1 = "regA", RS2 = "regA_05")
#' plotSinglePT(cw_Alien_ReG, RS1 = "regA", RS2 = "regC")
#'
#' @import ggplot2
#' @importFrom grid unit
#' @importFrom grid arrow
#' @importFrom methods hasArg
#' @importFrom scales alpha
#' @export plotSinglePT

plotSinglePT <-
  function(mPT,
           RS1,
           RS2,
           xlab = NA,
           main = NA) {
    if (!methods::hasArg(mPT)) {
      stop("mPT is missing")
    } else if (class(mPT) != "genoMatriXeR") {
      stop('mPT needs to be a "genoMatriXeR" class object')
    }

    if (!(methods::hasArg(RS1) & methods::hasArg(RS2))) {
      stop("RS1 and RS2 are required")
    } else if (!all(c(RS1, RS2) %in% names(mPT@multiOverlaps))) {
      stop("RS1 or RS2 do not match region set names in the mPT genoMatriXeR object")
    }

    if (is.na(xlab) & mPT@parameters$evFUN == "numOverlaps") {
      xlab = "N of overlaps"
    }

    colvec <- c("#57837B", "#F1ECC3", "#C9D8B6", "#515E63", "#C05555")

    tab <- mPT@multiOverlaps[[RS1]]
    n <- grep(paste0("^", RS2, "$"), tab$name)
    mean.1 <- tab$mean_perm_test[n]
    sd.1 <- tab$sd_perm_test[n]
    max_curve <-
      max(stats::density(stats::rnorm(seq_len(1000), mean = mean.1, sd = sd.1))$y)
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
    p <- ggplot2::ggplot(data.frame(x = c(splot, eplot)), ggplot2::aes_string("x")) +
      ggplot2::labs(
        title = main,
        subtitle = paste0("PermTest:  ", RS1, " vs ", RS2),
        caption = paste0("Number of Permutations:  ", mPT@parameters$ntimes)
      ) +
      ggplot2::xlab(paste0(xlab, " on ", tab$n_regionA[n], " regions")) +
      ggplot2::ylab("Freq") +
      ggplot2::stat_function(
        fun = dnorm,
        geom = "area",
        fill = scales::alpha(colvec[1], alpha = 0.5),
        args = list(mean = mean.1, sd = sd.1)
      ) +
      ggplot2::stat_function(
        fun = dnorm,
        geom = "area",
        xlim = c(vec_slices[1], zend),
        fill = scales::alpha(colvec[1], alpha = 0.5),
        args = list(mean = mean.1, sd = sd.1)
      ) +
      ggplot2::stat_function(
        fun = dnorm,
        geom = "area",
        xlim = c(vec_slices[3], zend),
        fill = scales::alpha(colvec[1], alpha = 0.5),
        args = list(mean = mean.1, sd = sd.1)
      ) +
      ggplot2::stat_function(
        fun = dnorm,
        geom = "area",
        xlim = c(vec_slices[5], zend),
        fill = scales::alpha(colvec[1], alpha = 0.5),
        args = list(mean = mean.1, sd = sd.1)
      ) +
      ggplot2::stat_function(
        fun = dnorm,
        geom = "area",
        xlim = c(zstart, vec_slices[2]),
        fill = scales::alpha(colvec[1], alpha = 0.5),
        args = list(mean = mean.1, sd = sd.1)
      ) +
      ggplot2::stat_function(
        fun = dnorm,
        geom = "area",
        xlim = c(zstart, vec_slices[4]),
        fill = scales::alpha(colvec[1], alpha = 0.5),
        args = list(mean = mean.1, sd = sd.1)
      ) +
      ggplot2::stat_function(
        fun = dnorm,
        geom = "area",
        xlim = c(zstart, vec_slices[6]),
        fill = scales::alpha(colvec[1], alpha = 0.5),
        args = list(mean = mean.1, sd = sd.1)
      ) +

      # hline at y = 0
      ggplot2::geom_hline(yintercept = 0,
                 color = colvec[4],
                 size = 0.6) +

      # vline at x = 0
      ggplot2::geom_vline(
        xintercept = 0,
        color = colvec[4],
        size = 0.4,
        linetype = "dotted"
      ) +

      # Random overlaps
      ggplot2::geom_vline(
        xintercept = c(mean.1),
        color = colvec[4],
        linetype = "dashed",
        size = 0.4
      ) +

      # Observed overlaps
      ggplot2::geom_vline(
        ggplot2::aes_string(xintercept = "nov"),
        color = colvec[5],
        linetype = "dashed",
        size = 0.4
      ) +

      # Arrow between random and observed
      ggplot2::geom_segment(
        ggplot2::aes(
          x = mean.1,
          y = max_curve / 2,
          xend = nov,
          yend = max_curve / 2
        ),
        linetype = "dashed",
        color = colvec[4],
        arrow = grid::arrow(length = grid::unit(0.5, "cm"))
      ) +

      # Text labels
      # Box with ZS and adjpv
      ggplot2::annotate(
        "label",
        x = mean.1 + ((nov - mean.1) / 2),
        y = max_curve * 0.36,
        label = paste(
          "Z-score",
          tab$z_score[n],
          "adj.p-value",
          tab$adj.p_value[n],
          sep = "\n"
        ),
        size = 3,
        fill = "#FDFAF6"
      ) +
      # Box with nZS and sZS
      ggplot2::annotate(
        "label",
        x = eplot * 0.9,
        y = max_curve * 0.9,
        size = 3,
        fill = "#FDFAF6",
        label = paste(
          paste0("Normal Z-score: ", round(tab$norm_zscore[n], digits = 2)),
          paste0("Standard Z-score: ", round(tab$std_zscore[n], digits = 2)),
          sep = "\n"
        ),
        hjust = 1
      ) +
      # Ranfun used
      ggplot2::annotate(
        "text",
        x = eplot * 0.5,
        y = max_curve * 0.99,
        label = mPT@parameters$ranFUN
      ) +
      # Observed and mean random overlaps
      ggplot2::annotate(
        "label",
        x = nov,
        y = max_curve * 0.03,
        size = 3,
        label = nov,
        col = colvec[5],
        hjust = 0.5,
        fill = "#FDFAF6"
      ) +
      ggplot2::annotate(
        "label",
        x = mean.1,
        y = -max_curve * 0.03,
        size = 3,
        label = round(mean.1, digits = 2),
        col = colvec[4],
        hjust = 0.5,
        fill = "#FDFAF6"
      ) +
      mendel_theme()
    return(p)
  }
