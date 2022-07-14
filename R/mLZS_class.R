#' multiLocalZScore Class
#'
#' An S4 class for "multiLocalZScore" object.
#'
#' @slot parameters List of parameters used to create the object
#' @slot multiLocalZscores ...
#' @slot matrix ...
#' @export
#'
#'
mLZS <- setClass("multiLocalZScore",
                 slots = c(
                   parameters = "list",
                   multiLocalZscores = "list",
                   matrix = "list"
                 )
)
