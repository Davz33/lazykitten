nav_sub <- function(path,current_obj,stop,j=1){
  if(j==stop) return(current_obj[[path[j]]]) else Recall(path,current_obj[[path[j]]],stop,j+1)}

nav <- function(path,current_obj,stop=length(path)) {
  nav_sub(path,current_obj,stop)
}

