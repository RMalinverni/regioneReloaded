#' Print method for genoMatriXeR class
#'
#' @param x A genoMatriXeR object.
#'
#' @return A printed output for genoMatriXeR objects.
#'
#' @seealso [genoMatriXeR][genoMatriXeR-class]
#'
#' @keywords internal

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
#' @return Parameters slot of a genoMatriXeR object.
#'
#' @seealso [genoMatriXeR][genoMatriXeR-class]
#'
#' @keywords internal
#'

setGeneric("gmxrParam", function(x) standardGeneric("gmxrParam"))

setMethod("gmxrParam", "genoMatriXeR", function(x) x@parameters)



#' Export multiOverlaps slot from genoMatriXeR objects
#'
#' @param x A genoMatriXeR object.
#'
#' @return multiOverlaps slot of a genoMatriXeR.
#'
#' @seealso [genoMatriXeR][genoMatriXeR-class]
#'
#' @keywords internal

setGeneric("gmxrMultiOverlaps", function(x) standardGeneric("gmxrMultiOverlaps"))

setMethod("gmxrMultiOverlaps", "genoMatriXeR", function(x) x@multiOverlaps)


#' Export matrix slot from genoMatriXeR objects
#'
#' @param x A genoMatriXeR object.
#'
#' @return matrix slot of a genoMatriXeR object.
#'
#' @seealso [genoMatriXeR][genoMatriXeR-class]
#'
#' @keywords internal

setGeneric("gmxrMatrix", function(x) standardGeneric("gmxrMatrix"))

setMethod("gmxrMatrix", "genoMatriXeR", function(x) x@matrix)



#' Set matrix slot value of a genoMatriXeR object
#'
#' @param x A genoMatriXeR object.
#' @param value Value to assign to the matrix slot.
#'
#' @return A genoMatriXeR object.
#'
#' @importFrom methods validObject
#'
#' @keywords internal

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
#' @return parameters slot of a multiLocalZScore object.
#'
#' @seealso [multiLocalZScore][multiLocalZScore-class]
#'
#' @keywords internal

setGeneric("mlzsParam", function(x) standardGeneric("mlzsParam"))

setMethod("mlzsParam", "multiLocalZScore", function(x) x@parameters)



#' Set parameters slot value of a multiLocalZScore object
#'
#' @param x A multiLocalZScore object.
#' @param value Value to assign to the parameters slot.
#'
#' @return A multiLocalZScore object.
#'
#' @importFrom methods validObject
#'
#' @keywords internal

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
#' @return multiLocalZscores slot of a multiLocalZScore object.
#'
#' @seealso [multiLocalZScore][multiLocalZScore-class]
#'
#' @keywords internal

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
#' @keywords internal

setGeneric("mlzsMatrix", function(x) standardGeneric("mlzsMatrix"))

setMethod("mlzsMatrix", "multiLocalZScore", function(x) x@matrix)



#' Set matrix slot value of a multiLocalZScore object.
#'
#' @param x A multiLocalZScore object.
#' @param value Value to assign to the multiLocalZScore slot.
#'
#' @return A multiLocalZScore object.
#'
#' @importFrom methods validObject
#'
#' @keywords internal

setGeneric("mlzsMatrix<-", function(x,value) standardGeneric("mlzsMatrix<-"))

setMethod("mlzsMatrix<-", "multiLocalZScore", function(x, value){
  x@matrix <- value
  validObject(x)
  x
} )



