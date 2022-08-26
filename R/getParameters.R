#' getParameters
#'
#' @description
#'
#' Get parameters from a genoMatriXeR or multiLocalZScore class object.
#'
#' @usage getParameters(rR, show_errors = FALSE)
#'
#' @param rR A genoMatriXeR or multiLocalZScore class object.
#' @param show_errors logical, if TRUE the function returns a list with two dataframes:
#' one containing the parameter values and one with any error messages that have been
#' generated during the permutation test iterations when running [crosswisePermTest].
#'
#' @return A dataframe with parameters and values, or a list with two dataframes
#' with parameters and errors information.
#'
#' @seealso [genoMatriXeR][genoMatriXeR-class], [multiLocalZScore][multiLocalZScore-class]
#'
#' @examples
#'
#' data("cw_Alien")
#'
#' prm <- getParameters(cw_Alien_ReG)
#'
#' prm
#'
#'
#' @importFrom methods is
#' @importFrom methods hasArg
#'
#' @export getParameters
#'

getParameters <- function(rR, show_errors = FALSE){

  stopifnot("rR is missing" = methods::hasArg(rR))
  stopifnot("rR must be an object of class genoMatriXeR or multiLocalZScore" = {
    methods::is(rR , "genoMatriXeR") | methods::is(rR , "multiLocalZScore")
  })

  if (methods::is(rR , "genoMatriXeR")) {
    param <- gmxrParam(rR)
    errors <- param$errors
    param <- param[names(param) != "errors"]
    res <- data.frame(parameter = names(param), value = as.character(param))

    if (show_errors) {
      res <- list(parameters = res, errors = errors)
    }

  }

  if(methods::is(rR , "multiLocalZScore")){
    param <- mlzsParam(rR)
    res <- data.frame(parameter = names(param), value = as.character(param))

    if (show_errors) {
      message("show_errors is only relevant for genoMatriXeR objects")
    }
  }

  return(res)
}
