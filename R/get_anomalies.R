
#' Retrieve anomalies
#'
#' Based on a summary normalized/stacked metric, retrieve top anomalies.
#'
#' @param x stranger object (before of after singularize)
#' @param rank.prop: proportion of records to be considered as anomalies
#' @param nmin: constraint - minimum number of anomalies
#' @param nmax: constrait - maximum number of anomalies
#' @param stack.use One of c("max","avg","min","damavg", "pruavg")) - must have been requestedwhen invoking `singularize` (done by default).
#' @param method.use One of c("norm","rank") - must have been requestedwhen invoking `singularize` (done by default).
#' @param verbose logical: provide some information.
#' @param ... additional parameters to pass
#'to singularize (if called on a non-singularized object)
#'
#'Anoamlies selection is performed using one summary metric. This summary metrics is assumed to stacked some base metrics - may be only one!. Stacking is performed after standardisarion, being possible with two approaches: normalisation (\code{method.use} = "norm") or ranking (\code{method.use} = "rank"). See \link{\code{singularize}} function.
#'
#'Three parameters are used together to define anomalies: rank.prop is firsu used to filter on top x% anomalies then one applies on top of this criteria conditions on a minimal (\code{nmin}) and maximal (\code{nmax}) number of anomalies to be provided.
#'
#'@examples
#' data <- crazyfy(iris[,1:4])
#' (anom <- get_anomalies(strange(data)))
#' \dontrun{
#' library(dplyr)
#' ss <- iris %>% select(-Species) %>%
#'  crazyfy() %>%
#'  strange(weird="autoencode") %>%
#'  singularize(methods="norm",stacks="avg")
#'  anom2 <- ss %>% get_anomalies(nmin=2, nmax=4)
#'  ss %>% plot(type="n",score="N_anom_norm_avg",anomaly_id=anom2[1])
#' }
#' @export
get_anomalies <- function(x,rank.prop=0.05, nmin=10, nmax=300,
                          stack.use="avg",
                          method.use="norm",verbose=TRUE,...){

  stack.use=match.arg(stack.use,c("max","avg","min","damavg", "pruavg"))
  method.use = match.arg(method.use,c("norm","rank"))

  assertthat::assert_that(inherits(x,"stranger"),msg="Incorrect object passed
                          as argument for x Use an output of either strange(r) or singularize functions. ")

#
#   #print(grepl(paste0("_",method.use,"_",stack.use),colnames(x)))
#     assertthat::assert_that(any(grepl(paste0("_",method.use,"_",stack.use),colnames(x))),
#                     msg="Selected stacking+method not present in singularized data; check colnames
#                     to list available combinations or recall singularize.")
  if (!inherits(x,"singular")) singular <- singularize(x,...) else singular <- x

  # check is both stack.use and method.use are available in singular object
  assertthat::assert_that(
    stack.use %in% attr(singular,"stacks"),
    method.use %in% attr(singular,"methods"),
    msg="Combination of method and stack approaches not present in singular object.")

  meta <- attr(singular,"meta")

  # referential of sorting methods per method/stack
  sortorders <- list(
    rank=list(
      min=list(order=1,vars="max"),
      max=list(order=1, vars="min"),
      avg=list(order=1, vars="avg"),
      damavg=list(order=1, vars="damavg"),
      pruavg=list(order=1, vars="pruavg")),
    norm=list(
      min=list(order=c(-1,-1),vars=c("max","min")),
      max=list(order=c(-1,-1), vars=c("min","max")),
      avg=list(order=c(-1,-1), vars=c("avg","min")),
      damavg=list(order=c(-1,-1), vars=c("damavg","min")),
      pruavg=list(order=c(-1,-1), vars=c("pruavg","min")))
  )

  getvar <- function(m,s) colnames(singular)[grepl(paste(m,"_",s,"$",sep=""),colnames(singular))]

  ## fast DT reorder, cf: https://stackoverflow.com/questions/13685295/sort-a-data-table-fast-by-ascending-descending-order
  ordering <- sortorders[[method.use]][[stack.use]]
  vars <- sapply(ordering$vars, function(s)getvar(method.use,stack.use))
  orders <- ordering$order
  if (verbose) cat(paste0("\nOrdering: ",paste(orders,vars, sep="x",collapse=" and ")))
  singular.sorted <- setorderv(singular, vars, orders)



  if (method.use=="rank"){
    rank.index = round(rank.prop*nrow(singular),0)
    if (verbose) cat("\nBased on provided proportion", rank.prop, ", the", rank.index, "highest ranked observations are considered as anomalies before stacking.\n")
    if (stack.use=="damavg") rank.index <- log(rank.index)
    outliers <- singular.sorted[singular.sorted[[getvar(method.use, stack.use)]]<=rank.index,][['.id']]
  } else {
    # method=norm
    outliers <- singular.sorted[singular.sorted[[getvar(method.use, stack.use)]]==1,][['.id']]
  }

  noutliers <- length(outliers)

  if (length(outliers) < nmin) {
    if (verbose) cat("\n",noutliers, "outliers found by default with this stacking and this method, which is not enough to satisfy nmin=",nmin,". Following most probable outliers are added to selection.\n")
    outliers <- singular.sorted[1:nmin,][['.id']]
  }

  if (length(outliers) > nmax) {
    if (verbose) cat("\n",length(outliers), "outliers found by default with this stacking and this method, which is too many to satisfy nmax",nmax,". Less probable outliers are removed from selection.\n")
    outliers <- singular.sorted[1:nmax,][['.id']]
  }



  # binarize
  # cf. anomfunction from line 1384 (cutoff)
  # class: strangest (contains ID of anomalies + source data+ some meta information+...)
  attr(outliers,"meta") <- meta
  class(outliers) <- c("anomalies",class(outliers))
  return(outliers)


}
