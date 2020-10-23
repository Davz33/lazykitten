r <- return
sl <- dplyr::select;ast <- dplyr::as_tibble
highest_in  <- function(listOrEnv,FUN=starts_with,...){
  l <- listOrEnv %>% as.list()
  sizes  <- map(l,function(x) x %>% select(FUN(...)) %>% ncol())
  sizes %>% as_vector() %>% base::which.max() %>% l[[.]] %>% return()
}
result <- list()
easin <- function(str,sep=" ") return(strsplit(str, sep, fixed = TRUE) %>% .[[1]])
