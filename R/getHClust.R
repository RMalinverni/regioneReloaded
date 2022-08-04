#' getHClust
#'
#' @description
#' get Object of class [hclust] from  [genoMatriXeR][genoMatriXeR_class] or [multiLocalZScore][multiLocalZScore_class]
#'
#' @usage getHClust( rR, hctype = "rows")
#'
#' @param rR A genoMatriXeR or multiLocalZScore object.
#' @param hctype character. Can be "rows" or "cols". (default= "cols")
#'
#'
#' @returns
#'
#' an object of class [hclust]
#'
#' @seealso [genoMatriXeR][genoMatriXeR-class], [multiLocalZScore][multiLocalZScore_class], [hclust]
#'
#' @examples
#'
#' data("cw_Alien")
#'
#' hc <- getHClust(cw_Alien_ReG)
#'
#' plot(hc)
#'
#' @importFrom methods is
#' @importFrom methods hasArg
#'
#'
#' @export getHClust
#'

getHClust<-function(rR, hctype = "rows"){

  if (!methods::hasArg(rR)) {
    stop("rR is missing")
  }

  if(!(methods::is(rR , "genoMatriXeR") | methods::is(rR , "multiLocalZScore"))){
    stop(" class of rR object need to be genoMatriXeR or multiLocalZScore")
  }

  if(!(hctype == "rows" | hctype == "cols")){
    stop(" hctype need to be rows or cols ")
  }


  if(methods::is(rR , "genoMatriXeR" )){

    if (hctype == "rows"){

      mod <- rR@matrix$FitRow

    }
    if (hctype == "cols"){

      mod <- rR@matrix$FitCol

      if (is.null(mod)) { mod <- rR@matrix$FitRow}

    }

  }

  if(methods::is(rR , "multiLocalZScore")){

      mod <- rR@matrix$FitRow

      if (hctype == "cols"){

        warning(" For a multiLocalZScore objects columns clustering is not allowed,
              row clustering will be retrived instead.")
      }


  }

  return(mod)
}
