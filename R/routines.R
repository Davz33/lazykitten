nav_sub <- function(path,current_obj,j=1,stop=length(path)){
  if(j==stop) return(current_obj[[path[j]]]) else Recall(path,current_obj[[path[j]]],j+1)}

nav <- function(path,current_obj) {
  nav_sub(path,current_obj)
}






