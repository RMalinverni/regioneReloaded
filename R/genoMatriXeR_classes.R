#' An S4 class for "genoMatriXeR" object.
#'
#' @slot parameters List of parameters used to create the object
#' @slot multiOverlaps ...
#' @slot matrix ...
#' @export
#'
#'
#'
gMXR <- setClass("genoMatriXeR",
                 slots = c(
                 parameters = "list",
                 multiOverlaps = "list",
                 matrix = "list"
                 )
              )








