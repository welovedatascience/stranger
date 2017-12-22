
#' @export
get_info <- function(x,...){
  UseMethod("get_info")
}

#' @export
get_info.default <- function(x,...){
  print(paste0(deparse(substitute(x)), " is of class:",paste(class(x),collapse='+')))
}


#' stranger object  information
#'
#' Retrieve some information on the content of a stranger object.
#' @return matrix - metrics attributes (corresponding weird info), one row per weird.
#' @examples
#' \dontrun{
#' library(dplyr)
#' info <-  iris %>% select(-Species) %>% crazyfy() %>% stranger() %>% get_info()
#' info
#' }
#' @export
get_info.stranger <- function(x,simplify=TRUE,...){

  out=sapply(select(x,-.id),attributes,simplify=simplify)
  if (simplify & is.array(out)) out=t(out)
  return(out)
}

#' singularized metrics information
#'
#' Retrieve some informaiton on the content of a singularize object.
#' Note: always print (cat)  information, store the content if you want to programmatically access it.
#' @return (invisible) list with two componenents: metrics and standardizations. First slot consists in metrics attributes (corresponding weird info), second slot is a vector containing the names of aggregated standardized derived metrics.
#' @examples
#' \dontrun{
#' library(dplyr)
#' info <-  iris %>% select(-Species) %>% crazyfy() %>% stranger() %>% singularize() %>% get_info()
#' }
#' @export
get_info.singular <- function(x,...){
  cat("\n*** singular object")
  out=sapply(select(x,-.id),attributes,simplify=FALSE)
  nam <- names(out)
  null <- sapply(out,function(col){
    if (is.null(col)) return(TRUE)
    if (!is.null(col$normalized)){
      if (col$normalized) return(TRUE)
    }
    return(FALSE)
  })
  norms <- nam[null]
  metrics <- nam[!null]

  out=sapply(select(x,one_of(metrics)),attributes,simplify=TRUE)
  if (is.array(out)) out=t(out)
  cat("\n\n- source metrics\n\n")
  print(t(out))
  cat("\n\n- standardizations\n")
  cat(paste(nam[null],collapse="\n"))
  #<!><TODO> (maybe) Add a class and a print method ; simpler: directly print what we want to say
  invisible(list(metrics=metrics, standardizations=nam[null]))
}
