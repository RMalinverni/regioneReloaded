#' Print method for genoMatriXeR class
#'
#' @param x A genoMatriXeR object.
#'
#' @return A printed output for genoMatriXeR objects.
#'
#' @seealso [genoMatriXeR][genoMatriXeR-class]
#'
#' @export


setMethod("print", "genoMatriXeR", function(x)
  list(
    param_slot = data.frame(
      param = names(x@parameters),
      value = unlist(x@parameters)[names(x@parameters)]
    ),
    Alist_Sample_slot = names(x@multiOverlaps),
    matrix_slot = x@matrix$GMat
  ))

#' Export parameters from genoMatriXeR objects
#'
#' @param x A genoMatriXeR object.
#'
#' @return A printed output for genoMatriXeR parameters slot.
#'
#' @seealso [genoMatriXeR][genoMatriXeR-class]
#'
#' @export "gmxrParam"
#'
setGeneric("gmxrParam", function(x) standardGeneric("gmxrParam"))

setMethod("gmxrParam", "genoMatriXeR", function(x) x@parameters)


#' Export multiOverlaps slot from genoMatriXeR objects
#'
#' @param x A genoMatriXeR object.
#'
#' @return A printed output for genoMatriXeR multiOverlaps slot.
#'
#' @seealso [genoMatriXeR][genoMatriXeR-class]
#'
#' @export "gmxrMultiOverlaps"
#'
setGeneric("gmxrMultiOverlaps", function(x) standardGeneric("gmxrMultiOverlaps"))

setMethod("gmxrMultiOverlaps", "genoMatriXeR", function(x) x@multiOverlaps)


#' Export matrix slot from genoMatriXeR objects
#'
#' @param x A genoMatriXeR object.
#'
#' @return A printed output for genoMatriXeR matrix slot.
#'
#' @seealso [genoMatriXeR][genoMatriXeR-class]
#'
#' @export "gmxrMatrix"
#'
#'
setGeneric("gmxrMatrix", function(x) standardGeneric("gmxrMatrix"))

setMethod("gmxrMatrix", "genoMatriXeR", function(x) x@matrix)

setGeneric("gmxrMatrix<-", function(x, value) standardGeneric("gmxrMatrix<-"))
setMethod("gmxrMatrix<-", "genoMatriXeR", function(x, value){
  x@matrix <- value
  validObject(x)
  x
} )


#' Export parameters from multiLocalZScore objects
#'
#' @param x A multiLocalZScore object.
#'
#' @return A printed output for multiLocalZScore parameters slot.
#'
#' @seealso [multiLocalZScore][multiLocalZScore-class]
#'
#' @export "mlzsParam"
#'
setGeneric("mlzsParam", function(x) standardGeneric("mlzsParam"))

setMethod("mlzsParam", "multiLocalZScore", function(x) x@parameters)

setGeneric("mlzsParam<-", function(x, value) standardGeneric("mlzsParam<-"))

setMethod("mlzsParam<-", "multiLocalZScore", function(x, value){
  x@parameters <- value
  validObject(x)
  x
} )


#' Export multiLocalZscores slot from multiLocalZScore objects
#'
#' @param x A multiLocalZScore object.
#'
#' @return A printed output for multiLocalZScore multiLocalZscores slot.
#'
#' @seealso [multiLocalZScore][multiLocalZScore-class]
#'
#' @export "mlzsMultiLocalZscores"
#'
setGeneric("mlzsMultiLocalZscores", function(x) standardGeneric("mlzsMultiLocalZscores"))

setMethod("mlzsMultiLocalZscores", "multiLocalZScore", function(x) x@multiLocalZscores)




#' Export matrix from multiLocalZScore objects
#'
#' @param x A multiLocalZScore object.
#'
#' @return A printed output for multiLocalZScore matrix slot.
#'
#' @seealso [multiLocalZScore][multiLocalZScore-class]
#'
#' @export "mlzsMatrix"
#'
setGeneric("mlzsMatrix", function(x) standardGeneric("mlzsMatrix"))
setGeneric("mlzsMatrix<-", function(x,value) standardGeneric("mlzsMatrix<-"))

setMethod("mlzsMatrix", "multiLocalZScore", function(x) x@matrix)
setMethod("mlzsMatrix<-", "multiLocalZScore", function(x, value){
  x@matrix <- value
  validObject(x)
  x
} )



