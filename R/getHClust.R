#' getHClust
#'
#' @description
#' get Object of class [hclust] from  [genoMatriXeR][genoMatriXeR-class] or [multiLocalZScore][multiLocalZScore-class]
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
#' @seealso [genoMatriXeR][genoMatriXeR-class], [multiLocalZScore][multiLocalZScore-class], [hclust]
#'
#' @examples
#'
#' data("cw_Alien")
#'
#' cw_Alien_ReG <- makeCrosswiseMatrix(cw_Alien_ReG)
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

  stopifnot("rR is missing" = methods::hasArg(rR))
  stopifnot("rR must be of class genoMatriXeR or multiLocalZScore" = {
    methods::is(rR , "genoMatriXeR") | methods::is(rR , "multiLocalZScore")
    })
  stopifnot("hctype must be rows or cols" = {
    hctype == "rows" | hctype == "cols"
  })

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
