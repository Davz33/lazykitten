r <- return
sl <- dplyr::select
ast <- dplyr::as_tibble


highest_in  <- function(listOrEnv,FUN=starts_with,...){
  l <- listOrEnv %>% as.list()
  sizes  <- map(l,function(x) x %>% select(FUN(...)) %>% ncol())
  sizes %>% as_vector() %>% base::which.max() %>% l[[.]] %>% return()
}

easin <- function(str,sep=" ") return(strsplit(str, sep, fixed = TRUE) %>% .[[1]])
ppend <- function(df,list,name=NULL){if(is.null(name))out <-c(list,list(df))else {out<-"[[<-"(list,name,df)}; out}

#dataframes
#merges dfs' rows (of different sizes as well) from a list into a new list whereby every element is a row
dilute <- function(dfs_list) Reduce(function(x,y)c(x,y),dfs_list)

# Grouping the left hand side, needed for multiple assignment operator within this library
g = function(...) {
  List = as.list(substitute(list(...)))[-1L]
  class(List) = 'lbunch'
  return(List)
}
ztartW<-function(char_vect,strings_list){
  matrix <- sapply(strings_list,function(prefix)startsWith(char_vect,prefix))
  char_vect[apply(matrix,1,any)]
}
#fuses two lists by names
#naming must be a character vector of length 2
lfus = function(list1,list2,naming){
  if(!(names(list1)%=%names(list2))) stop("lists names must be the same")
  if(!identical(unique(names(list1)),names(list1))) stop("duplicate names are not allowed")
  out <- list()
  for(nm in names(list1)) {
    el <- list();el[[naming[1]]]<- list1[[nm]];el[[naming[2]]]<-list2[[nm]]
    if(is.table(list1[[nm]])|is.table(list2[[nm]]))out[[nm]] <- list(el)
    else out[[nm]] <- el
  }
  return(out)
}
