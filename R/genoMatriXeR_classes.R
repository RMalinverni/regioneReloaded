#' genoMatriXeR Class
#'
#' @description
#'
#' An S4 class for "genoMatriXeR" object.
#'
#' @slot parameters List of parameters used to create the object.
#' @slot multiOverlaps Results of multiple pairwise permutation tests generated with [crosswisePermTest()].
#' @slot matrix List of numerical matrices containing z-score, pvalues and correlation values generated with [makeCrosswiseMatrix()]
#' @export
#'

gMXR <- setClass("genoMatriXeR",
                 slots = c(
                 parameters = "list",
                 multiOverlaps = "list",
                 matrix = "list"
                 )
              )








