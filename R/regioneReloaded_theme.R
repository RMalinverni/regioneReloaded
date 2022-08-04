#' Themes used in the regioneReloaded package
#'
#' Ggplot2 themes created for the package.
#'
#' @usage
#'
#' mendel_theme(
#'   base_size = 11,
#'   base_family = ""
#' )
#'
#' @param base_size	 base font size, given in pts.
#' @param base_family	 base font family
#'
#' @import ggplot2
#'
#' @keywords internal
#'
#' @noRd

mendel_theme <- function(base_size = 11, base_family = ""){
  ggplot2::theme(
    panel.background = ggplot2::element_rect(
      fill = "#F7F4DB",
      colour = "#F7F4DB",
      size = 0.5,
      linetype = "solid"
    ),
    panel.grid.major = ggplot2::element_line(
      size = 0.5,
      linetype = 'solid',
      colour = "#FDFAF6"
    ),
    panel.grid.minor = ggplot2::element_line(
      size = 0.25,
      linetype = 'solid',
      colour = "#FDFAF6"
    )
  )
}

#' plotPal
#'
#' @description
#'
#' Internal function to create a palette generating function for plots
#'
#' @inheritParams plotSingleLZ
#'
#' @importFrom grDevices colorRampPalette
#' @keywords internal
#'
#' @return A palette function
#' @noRd

plotPal <- function(colPal = NULL) {
  if (is.null(colPal)) {
    # colPal <- c("#434C6D", "#553B23", "#EF7E39", "#E23A79", "#6EA89E")
    colPal <- c("#BD3241", "#802160", "#3E3C66", "#495C52", "#E67E45")
    pal <- grDevices::colorRampPalette(colPal)
  } else {
    pal <- grDevices::colorRampPalette(colPal)
  }
  return(pal)
}
