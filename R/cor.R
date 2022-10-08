#' Cor
#'
#' Correlation for stranger objects
#'
#' @param x a stranger  object
#' @param method see base cor function
#'
#' @rdname cor
#' @export
cor <- function(x, ...) {
  UseMethod('cor', x)
}

#' @rdname cor
#' @export
cor.stranger <- function(x, method = c("pearson", "kendall", "spearman"),...) {
  stats::cor(x[,-'.id',with=FALSE],y=NULL, use="everything",method=method)
}

#' @rdname cor
#' @export
cor.default <- function(x, ...) {
  return(stats::cor(x, ...))
}

#' 
#' 
#' 
#' #'@export
#' setGeneric("cor")
#' 
#' #' Correlation for stranger objects
#' #'
#' #'cor Correlation method for stranger objects.
#' #' @method cor stranger
#' #' @export
#' setMethod("cor",signature(x="stranger"),function(x, method = c("pearson", "kendall", "spearman")){
#'   selectMethod("cor","ANY")(x[,-'.id',with=FALSE],y=NULL, use="everything",method=method)
#' })
#' 
#' 
