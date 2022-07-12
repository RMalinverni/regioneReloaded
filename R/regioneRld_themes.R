#' Themes used in the regioneReloaded package
#'
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
#'
#' @keywords internal function
#'
#'

colvec <- c("#57837B", "#F1ECC3", "#C9D8B6", "#515E63", "#C05555")

mendel_theme <- function(base_size = 11, base_family = ""){
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
}
