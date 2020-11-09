pste <- function(vect_or_list,sep=" ")Reduce(function(x,y)paste(x,y,sep),as.list(vect_or_list))
ssplit <- function(stringvect,splitby=" ") strsplit(stringvect,splitby)[[1]]
hasclmn <- function(tb,string) sum(str_detect(colnames(tb),string))
#alternative to grepl
grab <- function(string,delim,Nth) unlist(lapply(strsplit(string, delim, fixed = TRUE), '[', Nth))
