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

#' multiLocalZScore Class
#'
#' @description
#'
#' An S4 class for "multiLocalZScore" object.
#'
#' @slot parameters List of parameters used to create the object
#' @slot multiLocalZscores Results of multiple pairwise permutation tests on shifted region sets generated with [multiLocalZscore()].
#' @slot matrix List of numerical matrices containing local z-scores and correlation values generated with [makeLZMatrix()].
#' @export
#'

mLZS <- setClass("multiLocalZScore",
                 slots = c(
                   parameters = "list",
                   multiLocalZscores = "list",
                   matrix = "list"
                 )
)







