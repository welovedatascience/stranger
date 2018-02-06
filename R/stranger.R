#' Computes anomaly metrics by invoking specific method(s) with associated sets of parameters
#'
#' \code{strange} invokes a \emph{weird} method -- that is a wrapper around a
#' pre-existing anomaly detection measure (distance, probability...). \code{stranger}
#' allows to invoke several \emph{weird} methods in a single call.
#'
#' @details
#' You will use \code{strange} to use one method and may be interested by \code{stranger}
#' if you wants to apply different methods in a single call.
#' When comparing \code{stranger} package with \code{caret}, \code{strange} function is the
#' equivalent of \code{train}, where as \code{stranger} corresponds to \code{caretEnsemble}
#' function and package.
#'
#' @param data crazy data, ie outcome of a call to \code{\link{crazyfy}}.
#' @param weird Weird method to be used - for the list of available methods, use \code{weirds_list}.
#' @param tuneGrid (optional) vector or data.frame of values for the parameters of the invoked method.
#' @param colname (optional) character - name to be given to the resulting anomaly metric computation (distance/probability).
#' @param \dots additional parameters to be passed to the invoked \emph{weird} method.
#'
#' @return stranger object -- that is a data.table with attributes and overloaded with class stranger
#'
#' @aliases stranger
#' @rdname stranger
#'
#' @examples
#' \dontrun{
#' library(stranger)
#' data(iris)
#' crazydata <- crazyfy(iris[,1:4])
#' curious <- strange(crazydata, weird="knn")
#' }
strange <- function(data, weird="knn",tuneGrid=NULL,colname=NULL,...){

  # equivalent to caret "train"
  # we have to require a crazy objct or to try to crazyfy it:
  # .id column
  # crazyfy recodes some data (or do nothing) + handles some basic requirements such as dealing with missing values
  # duplicates...

  assertthat::assert_that(assertthat::is.string(weird),assertthat::not_empty(weird),msg="weird method argument must be a string naming a weird method.")

  assertthat::assert_that(weird %in% unlist(weirds_list()$methods),msg=paste(weird ,"is not a weird method available -- check list of methods with weirds()"))
  assertthat::assert_that(inherits(data,"crazy.data.table"),msg="data must be preprocessed with crazyfy function.")

  wfoo <- get(paste("weird",weird,sep="_"))
  if (is.null(tuneGrid)){
    out <- wfoo(data[,-".id",with=FALSE],colname=colname,...)
  } else {
    assertthat::assert_that(is.data.frame(tuneGrid),msg="tuneGrid must be a named data.frame (names for weird method parameters).")
    assertthat::assert_that(assertthat::not_empty(tuneGrid),msg="tuneGrid is empty.")
    assertthat::assert_that(all(colnames(tuneGrid) %in% names(wfoo(info=TRUE)$parameters)),msg="tuneGrid must contain columns named after weird method possible parameter.")
    out <- vector(mode='list',length=nrow(tuneGrid))
    #columns <- paste(method,apply(sapply(colnames(tuneGrid),function(n)paste(n, tuneGrid[[n]],sep='_')),1,paste, collapse="_"),sep="_")
    for (it in 1:nrow(tuneGrid)){
      args <- as.list(tuneGrid[it,])
      names(args) <- colnames(tuneGrid)
      args <- c(list(data[,-".id",with=FALSE]),args,colname=colname,list(...))
      out[[it]] <- do.call("wfoo",args)
    }
    out <- Reduce(cbind,out)
  }
  out <- setDT(out)
  out[[".id"]] <- data[[".id"]]

  setkey(out,.id)

  crazymeta <- attr(data,"meta")

  # reuse mapping table with eventual duplicates to enrich outliers with their eventual  duplicate ID
  if (crazymeta$has.duplicates){
    #<!> To be tested
    # dup.id.mapping
    dup <- crazymeta$duplicated.mapping
    setkey(dup,.id)
    out=dup[out, on=.(.id)]
    out[!is.na(out[,mappedid]),][[".id"]] <- out[!is.na(out[,mappedid]),mappedid]
    out[,mappedid:=NULL]
  }

  attr(out,"meta") <- list(
    data=data,
    variables=colnames(data),
    crazymeta=crazymeta,
    normalized=FALSE,
    class="stranger")

  class(out) <- c("stranger",class(out))
  return(out)
}



tuneCheck <- function(x)
  # derived from caretEnsemble tuneCheck
{
  stopifnot(is.list(x))
  methods <- sapply(x, function(a) a$weird)
  assertthat::assert_that(methodCheck(methods), msg="One weird method is not available.")
  if (is.null(names(x))) {
    names(x) <- methods
  }
  i <- names(x) == ""
  if (any(i)) {
    names(x)[i] <- methods[i]
  }
  names(x) <- make.names(names(x), unique = TRUE)
  stopifnot(all(sapply(x, is.list)))
  return(x)
}

stranger <- function(data,methodList=c("knn","lof"), tuneList=NULL,...){
  ## inspired from caretEnsemble:: caretList
  ## equivalent to caretList or combining several weirds methods with merge
  ## tuneList: list of weird(method="",params=,...)
  if (is.null(tuneList) & is.null(methodList)) {
    stop("Please either define a methodList or tuneList")
  }
  if (!is.null(methodList) & any(duplicated(methodList))) {
    warning("Duplicate entries in methodList.  Using unique methodList values.")
    methodList <- unique(methodList)
  }
  if (!is.null(tuneList)) assertthat::assert_that(all(sapply(tuneList,function(w)inherits(w,"weirdSpecs"))),msg="tuneList must be a list with elements being built by weird function")
  if (!is.null(methodList)) {
    methodCheck(methodList)
    tuneList_extra <- lapply(methodList, weird)
    tuneList <- c(tuneList_extra,tuneList)
  }

  tuneList <- tuneCheck(tuneList)
  global_args <- c(list(data=data),list(...))

  weirdList <- lapply(tuneList, function(w) {
    weird_args <- c(global_args, w)
    # print(weird_args)
    strange_w <- do.call(strange, weird_args)
    return(strange_w)
  })
  # names(weirdList) <- names(tuneList)

  out <- Reduce("merge",weirdList)

  # nMethods <- length(methodList)+length(tuneList)
  # availableMethods <-weirds()
  # assert_that(nMethods>0, msg="No weird method invoked. Use either methodList or tuneList (or both).")
  # if (length(methodList)>0){
  #   check <- sapply(methodList,
  #                   function(im){
  #                     assert_that(im %in% availableMethods$methods,
  #                                 msg=paste("Weird method",im, "is not available"))
  #                   })
  #
  #   check <- sapply(methodList,
  #                   function(im){
  #                     assert_that(availableMethods$detail[paste("weird",im,sep='_'),"installed"]=="*",
  #                                 msg=paste("Weird method",im, "requires package not installed - check requirements with weirds() function. Eventually considrr using install.weirdness()."))
  #                   })
  # }
  #<TODO>
  return(out)
}
