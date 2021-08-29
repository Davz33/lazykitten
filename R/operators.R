#############Multiple Assignments in one line of code
# Grouping the left hand side, needed for multiple assignment operator within this library
g = function(...) {
  List = as.list(substitute(list(...)))[-1L]
  class(List) = 'lbunch'
  return(List)
}

'%>%'<-dplyr::'%>%'

# Generic form
'%<-%' = function(l, r, ...) {
  UseMethod('%<-%')
}

# Binary Operator
'%<-%.lbunch' = function(l, r, ...) {
  Envir = as.environment(-1)

  if (length(r) > length(l))
    warning("RHS has more args than LHS. Only first", length(l), "used.")

  if (length(l) > length(r))  {
    warning("LHS has more args than RHS. RHS will be repeated.")
    r <- extendToMatch(r, l)
  }

  for (II in 1:length(l)) {
    do.call('<-', list(l[[II]], r[[II]]), envir = Envir)
  }
}

`%+%` = function(str1, str2) {
  base::paste0(str1, str2)
}

`%a%` = function(obj_name_asString, obj) {
  assign(obj_name_asString, obj, envir = parent.frame())
}

`%>>%` = function(data_frame, type) {
  out <- switch(type,
                "namvect" = setNames(data_frame[[2]], data_frame[[1]]))
  return(out)
}

`%=%` = function(a, b)
  identical(sort(a), sort(b))

`%*=%` = function(a, list) {
  lapply(list, function(el)
    identical(sort(a), sort(el)))
}


