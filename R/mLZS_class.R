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
