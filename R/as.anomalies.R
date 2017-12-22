#' Create a class anomalies from a (pre-filterd) stranger/singular
#'
#' One can use either dplyr filter verb or subseting records with base "[" to manually define anomalies
#' Then \code{as.anomalies} can be used to create anomalies object to be used for instance for visualisation purpose.
#'
#' Note that it is expected from the user to filter the object. If not done, all records are defined as anomalies.
#' This manual approach can be useful with some metrics and litterature recommandation,
#'  for instance using a cutpoint of 2 for 'abod' method.
#' @param x stranger/singular (pre-filered) object
#' @return object of class anomalies -- see also \code{\link{get_anomalies}}.
#' @export
as.anomalies <- function(x,...){
  assertthat::assert_that(inherits(x,"stranger"),msg="x must be a stranger object derived from strange or singularize")
  outliers <- x[[".id"]]
  attr(outliers,"meta") <- attr(x,"meta")
  class(outliers) <- c("anomalies",class(outliers))
  return(outliers)
}
