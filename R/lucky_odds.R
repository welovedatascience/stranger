#' Quickly apply stanger full process flow to flag candidates anomalies applying one weird method
#'
#' \code{lucky_odds} is basically a wrapper around the process: \code{\link{add_id}} --> \code{\link{crazyfy}}
#' --> \code{\link{strange}} (weird method with set of parameters) --> \code{\link{singularize}}
#'  (default parameters: all methods)
#' --> \code{\link{get_anomalies}} (flag top \emph{n.anom} anomalies) --> \code{\link{fortify}} to enrich source data.
#' By calling \code{lucky_odds}, analyst gets back source data with an additional column flagging some records.
#' Though obviously simplifying the analysis process, not all options are available and intermediate objects
#' are not available for further analysis.
#'
#' @param data  Source data (data.frame or data.table).
#' @param n.anom Number of anomaly candidate records to flags.
#' @param \dots Additional parameters to be passed to weird method (\code{analysis.method}).
#' @param analysis.drop Character - set of variables to be removed from analysis (metrics computations by weird).
#' @param analysis.keep Character - set of variables to be kept for analyis  (metrics computations by weird).
#' @param weird weird method to use for metric computation
#' @param stack Stacking metric passed to \code{\link{get_anomalies}}.
#' @param stack.method Stacking selection method passed to  \code{\link{get_anomalies}}.
#'
#'@section Selecting variables:
#'If your source data contains variables you don't want to use in metrics computations - weird method: knnw, autoencode...,
#'then you have to first select analysis variables. You can thos this use either \code{analysis.keep} OR \code{analysis.drop}.
#'Those two parameters are mutually exclusive.
# @examples
#' library(stranger)
#' data(iris)
#' anomalies <- lucky_odds(iris[,1:4])
#' table(anomalies$flag_anomaly)
lucky_odds <- function(data,n.anom=5,..., analysis.drop=NULL,analysis.keep=NULL,weird="knn",
                       stack="avg", stack.method="norm"){
  data.id <- add_id(data)
  if (!is.null(analysis.drop) & !is.null(analysis.keep)) stop("Specify analysis variables either with keep or drop.")
  if (!is.null(analysis.drop)) data.id <- data.id[,!colnames(data.id)%in%analysis.drop, with=FALSE]
  if (!is.null(analysis.keep)) data.id <- data.id[,c(analysis.keep,".id"), with=FALSE]
  data.crazy <- crazyfy(data.id)
  data.strange <- strange(data.crazy,weird=weird,...)
  data.singular <- suppressWarnings(singularize(data.strange))
  data.anomalies <- get_anomalies(data.singular,stack.use=stack, method.use=stack.method, nmin=n.anom, nmax=n.anom, verbose=FALSE)
  data <- suppressWarnings(fortify(data.anomalies, data=data))
  return(data)
}
