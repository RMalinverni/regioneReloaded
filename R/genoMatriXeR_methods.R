setMethod("print", "genoMatriXeR", function(x)
  list(
    param_slot = data.frame(
      param = names(x@parameters),
      value = unlist(x@parameters)[names(x@parameters)]
    ),
    Alist_Sample_slot = names(x@multiOverlaps),
    matrix_slot = x@matrix$GMat
  ))
