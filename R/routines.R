nav_sub <- function(path,current_obj,stop,j=1){
  if(j==stop) return(current_obj[[path[j]]]) else Recall(path,current_obj[[path[j]]],stop,j+1)}

nav <- function(path,current_obj,stop=length(path)) {
  nav_sub(path,current_obj,stop)
}

#' #' Bind rows from dataframes in list
#' #' @description if dataframes have different column length, return them clustered by clength (in a list)
#' #' @param dfs_list list of dataframes
#' #'
#' #' @return a dataframe
#' #' @export
#' #' @importFrom tibble as_tibble
#' #' @examples bindr_flxbl(list(mtcars,mtcars))
#' bindr_flxbl <- function(dfs_list){
#'   sapply(dfs_list,ncol) -> dfs_ncols
#'   processed <- c()#already processed, to avoid redundancy
#'   out <- list();i<-0
#'   for(n in dfs_ncols){
#'     i<-i+1
#'     if(i %in% processed) next()
#'
#'     out <- tibble::as_tibble(Reduce(rbind,dfs_list[which(dfs_ncols==n)])) %>% ppend(out)
#'     processed <- c(which(dfs_ncols==n),processed)
#'     print(processed)
#'   }
#'   return(out)
#' }






