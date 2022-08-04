#' getParameters
#'
#' @description
#'
#' Get parameters from a genoMatriXeR or multiLocalZScore class object.
#'
#' @usage getParameters(rR)
#'
#' @param rR A genoMatriXeR or multiLocalZScore class object.
#'
#' @return A dataframe with parameters and values.
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

getParameters <- function(rR){

  if (!methods::hasArg(rR)) {
    stop("rR is missing")
  }

  if(methods::is(rR , "genoMatriXeR") | methods::is(rR , "multiLocalZScore")){

    param <- rR@parameters

  }else {
    stop("getParameters function needs an object of class genoMatriXeR or multiLocalZScore")
  }

 res <- data.frame(parameter= names(param), value = as.character(param))
 return(res)
}
