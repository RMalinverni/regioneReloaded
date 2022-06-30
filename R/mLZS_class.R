#' An S4 class for "multiLocalZScore" object.
#'
#' @slot parameters List of parameters used to create the object
#' @slot multiLocalZscores ...
#' @slot matrix ...
#'
mLZS <- setClass("multiLocalZScore",
                 slots = c(
                   parameters = "list",
                   multiLocalZscores = "list",
                   matrix = "list"
                 )
)
