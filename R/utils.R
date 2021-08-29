ifelse <- function(cond,iftrue,iffalse=NULL) switch(cond+1,iffalse,iftrue)
tofc <- function(obj) return(list(typeof=typeof(obj),class=class(obj)))
