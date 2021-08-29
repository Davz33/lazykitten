#' Find the data frame with greatest amount of columns named a certain way
#' @description Given a list/environment of data frames and one or more attributes, it finds the one dataframe having
#' most columns/rows named as specified by the attributes. The default function used for matching is \link[dplyr]{starts_with} (dplyr v.1.0.7)
#' @importFrom tibble as_tibble
#' @param listOrEnv container of the input dataframes
#' @param ... one or more attributes to search by, the type depends on the \link{FUN} accepted input (default:character)
#' @param FUN the function to apply for matching column or row names (default: \link[dplyr]{starts_with})
#' @param row if TRUE row names are used for search and scoring
#'
#' @return a dataframe from \link{listOrEnv}
#' @export
#'
#' @examples df1<-mtcars[1:9,];df2<-mtcars[10:16,]
#' highest_in(list(df1,df2),"Merc",row=T)
#' highest_in(list(df1,df2),c("Merc","Cadillac"),row=T)
#' highest_in(list(df1,df2),c("Merc","Valiant"),row=T)

highest_in  <- function(listOrEnv,
                        ...,
                        FUN = dplyr::starts_with,
                        row = F) {
  s <- dplyr::select
  wmax <- base::which.max
  l <- as.list(listOrEnv)
  sizes  <- purrr::map(l, function(x)
    ifelse(is.data.frame(x), ifelse(row, tibble::as_tibble(
      t(x)
    ), tibble::as_tibble(x)), stop(
      "all of the elements in the input list or environment must be of class data.frame"
    ))
    %>% s(FUN(...)) %>% ncol())
  wmax(as.vector(sizes)) %>% l[[.]] %>% return()
}

#' Decompose a string into a vector
#'
#' @description Create a vector of strings from a string based on a delimiter
#'
#' @param str Input character vector
#' @param sep Delimiter to be used to subset \link{str} into chunks
#'
#' @return A vector of characters
#' @export
#'
#' @examples fruits <- easin("apple berry banana pear")
easin <-
  function(str, sep = " ") {
    return(strsplit(str, sep, fixed = TRUE) %>% .[[1]])
  }



#dataframes
#


#' Explode dataframe to list of lists
#' @description merges dataframes columns/rows from a list into a new list whereby every element is a list containing the column/row elements for each of the dataframes. All of the dataframes are be ultimately merged together in the same list.
#' @param ... list of dataframes to merge together
#' @param row if TRUE, the output list will consist of the rows of the original dataframes as sublist-elements, otherwise the original dataframes columns (default: FALSE)
#'
#' @return a list containing row/column of all the dataframes in \code{...} as sublists
#' @export
#'
#' @examples dilute(mtcars,iris)
#' dilute(mtcars,iris,row=T)
dilute <- function(..., row = F) {
  dfs_list <- list(...)
  if (sum(mapply(
    function(df)
      typeof(df) == "list" &
    "data.frame" %in% class(df),
    dfs_list
  )) != length(dfs_list))
    stop("All inputs elements must be data frames")
  asdt <- function(x)
    as.data.frame(t(x))
  if (row) {
    Reduce(function(x, y)
      c(asdt(x), asdt(y)), dfs_list)
  } else{
    return(Reduce(function(x, y)
      c(x, y), dfs_list))
  }
}



ztartW <- function(char_vect, strings_list) {
  matrix <-
    sapply(strings_list, function(prefix)
      startsWith(char_vect, prefix))
  char_vect[apply(matrix, 1, any)]
}


#' List fusion by name
#' @description #fuses two lists by names, creating a new list where each sub-list contain the elements
#' of the original lists in the same ordered and labels as per \code{labeling}.
#' @param list1 list to be fused by name
#' @param list2 list to be fused by name
#' @param labeling must be a character vector of length 2
#' @param follow specify which order should the resulting list have. If \code{list2}, the order will be the same of the second input list (default: \code{list1})
#'
#' @return a list containing all the elements of \code{list1} and \code{list2}
#' @export
#'
#' @examples la<-list(type="sunflower",available=F,quantity=1)
#' lb<-list(available=T,type="rosemary",quantity=3)
#' lfus(la,lb,c("A","B"))
lfus = function(list1, list2, labeling, follow = list1) {
  if (!(names(list1) %=% names(list2)))
    stop(
      "lists names must be identical or lists must be unnamed, use unname() or assign common names to both list ( will be override by the naming arg)"
    )
  if (!identical(unique(names(list1)), names(list1)))
    stop("duplicate names are not allowed")
  out <- list()
  if (is.null(names(list1))) {
    nnames = seq(1, length(list1), 1)
    "names<-"(list1, nnames)
    "names<-"(list2, nnames)
  }
  for (nm in names(follow)) {
    el <-
      list()
    el[[labeling[1]]] <- list1[[nm]]
    el[[labeling[2]]] <- list2[[nm]]
    if (is.table(list1[[nm]]) |
        is.table(list2[[nm]]))
      out[[nm]] <- list(el)
    else
      out[[nm]] <- el
  }
  return(out)
}
