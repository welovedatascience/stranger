crazyfy <- function
#--------------------------------------------------------------------------------
#' Data preparation before detection of strangers
#'
#' \code{crazyfy} preprocess data for anomalies detection computational
#' routines with \code{strange} : missing values
#' treatement, variables standardisation, eventual recoding in log,
#' treatment of character/factor variables.
#'
#' @details
#' See here this list of possible pre-treatment operations.
#' Factors/characters are transformed into numeric by using term frequency–inverse
#' document frequency approach (td-idf). Note that we use the smooth weighting IDF weight,
#' ie. we take the log of 1+N/nt where N is the number of observations and nt the frequency
#' for the specific term t.
#'
#' @param data Source data (data.frame or data.table).
#' @param do character vector - List of processing steps to apply -- see details.
#' @param id (optional) character - name of a preexisting variable to be used as ID.
#' @param skewness.cutpoint numeric - value that is used to determine whether
#' log recoding should be applied.
#' @param NA.method character - method to be used for missing values imputation;
#' one of "mean" or "value" (then using following parameter \code{NA.value}).
#' @param NA.value numeric Value to be used to impute missing values when \code{NA.method}
#' if "value".
#' @param verbose logical - should function display some details about processing.
#'
#' @return Pre-processed data of classes data.table overloaded by crazy.data.table.
#' @examples
#' library(stranger)
#' data(iris)
#' crazy <- crazyfy(iris[,1:4])
#' @export
#---------------------------------------------------------------------------------
(
  data,
  do=c("factor","log","impute","range"),
  id=NULL,
  skewness.cutpoint=2,
  NA.method="mean",
  NA.value=0,
  verbose=FALSE)
#---------------------------------------------------------------------------------
  {

  stopifnot(require("data.table"))
  assertthat::assert_that(require("data.table"),msg="data.table required for this function")

  ## Checks on parameters
  NA.method=match.arg(NA.method,c("mean","value"))
  do = match.arg(do,c("factor","log","impute","range","scale"),several.ok = TRUE)
  assertthat::assert_that(is.numeric(skewness.cutpoint),msg="Parameter skewness.cutpoint must be a numeric value.")


  ## Preparation of output
  out <- as.data.table(data)
  meta.preprocess <- list()
  cols <- colnames(out) # get current columns -- ie without ID
  if (!is.null(id)) cols <- cols[!cols%in%id]
  colclasses <- sapply(out[,cols, with=FALSE],class)



  ## Some basic preleminary checks about the variables

  ## <CHECK0> Any factor/character but not asked for do: factor <STOP>
  if (any(colclasses %in% c("factor","character")) & !"factor"%in% do) stop("Data contains a factor/character column -- consider removing it or call crazyfy with 'factor' in do parameters.")

  ### <CHECK 1> All missings <STOP>
  allmiss <- apply(out,2,function(vec)all(is.na(vec)))
  if (any(allmiss)) stop(paste("Variables",paste(colnames(out)[allmiss],collapse=" and "),"contain only missing values. Please remove them before applying crazyfy."))

  ### <CHECK 2> 1 unique value <STOP>
  allunique <- apply(out,2,function(vec)length(unique(vec))==1)
  if (any(allunique)) stop(paste("Variables",paste(colnames(out)[allunique],collapse=" and "),"do not have variance. Please remove them before applying crazyfy."))

  ### <CHECK 3> Mode > 50% (all variables) <WARN>
  allmode50 <- apply(out, 2, function(vec) modefreq(vec)$freq > 0.5)
  if (any(allmode50)) warning(paste("Variables",paste(colnames(out)[allmode50],collapse=" and "),"have one mode with frequency higher than 50%. You may want to remove them before applying crazyfy."))

  ### <CHECK 4> Equal repartition between categories -- remove ID (!)<STOP>
  do.cols <- cols[colclasses %in% c("factor","character")]
  if (!is.null(id)) do.cols <- do.cols[!do.cols%in%id]
  if (length(do.cols)>0){
    factorequal <- apply(out[,do.cols, with=FALSE],2,
                         function(vec){
                           m=modefreq(vec)
                           totfreq=length(m$mode)*m$freq
                           return(identical(totfreq,1))}
    )
    if (any(factorequal)){
      stop(paste("Variables",paste(do.cols[factorequal],collapse=" and ")," are factor/character but with equal repartition of categories. Their treatment will thus result in a ITDF with no variance (plus they don't have interest for anomalies).  Please remove them before applying crazyfy."))
    }
  }

  ## Create/Assign id
  if (is.null(id))  {
    if (".id" %in% colnames(data)){
      had.id=TRUE
    } else {
      had.id=FALSE
      ### Add .id column
      out[[".id"]] <- 1:nrow(data)
    }
    id <- ".id"
  } else {
    had.id=TRUE
    setnames(out,id,".id")
  }

  ## <CHECK 5> If had.id, check valid one
  if (had.id) assertthat::assert_that(length(unique(out[[".id"]]))==nrow(out), msg="Selected id does not seem to be a valid one.")


  setkey(out,.id)


  do_factor <- function(){
    do.cols <- cols[colclasses %in% c("factor","character")]
    do.cols <- do.cols[!do.cols %in% id] # remove ID (in case character)
    meta.preprocess$factor <<- list(done=length(do.cols)>0, vars=do.cols)
    if (length(do.cols)>0){
      for (ifac in do.cols){
        ifac.x <- as.vector(out[[ifac]])
        ifac.N <- length(ifac.x)
        ifac.x[ifac.x==""] <- NA
        ifac.freqNA <- sum(is.na(ifac.x))
        ifac.freq <- table(ifac.x)[ifac.x]
        ifac.out <- log(N / ifac.freq)
        ifac.out[is.na(ifac.out)] <- log( 1+ ifac.N / ifac.freqNA)
        out[[ifac]] <- ifac.out
      }
    }
    return(out)
  }



  do_log <- function(){
    if (verbose) print("crazyfy: LOG")
    meta.preprocess$log <<- list(done=FALSE,vars=vector(mode="character",length=0),parameters=c(skewness.cutpoint=skewness.cutpoint))

    do.cols <- names(out)[sapply(out,is.numeric)]
    do.cols <- do.cols[!do.cols %in% id] # remove ID
    for (ivar in do.cols){
      sk <- skewness(out[[ivar]])
      if (!is.na(sk)){
        if (sk > skewness.cutpoint) {
          meta.preprocess$log$done <<- TRUE
          meta.preprocess$log$vars <<- c(meta.preprocess$log$vars,ivar)
          out[[ivar]] <- log(out[[ivar]] - min(out[[ivar]],na.rm=TRUE) +1)
        }
      }
    }
    return(out)
  }

  do_impute <- function(){
    if (verbose) print("crazyfy: MISSING")
    meta.preprocess$impute <<- list(
      done=FALSE,
      vars=vector(mode="character",length=0),
      parameters=list(NA.method=NA.method, NA.value=NA.value))
    do.cols <- names(out)[sapply(out,is.numeric)]
    do.cols <- do.cols[!do.cols %in% id] # remove ID

    if (verbose) print(do.cols)
    for (ivar in do.cols){
      ivar.NA <- is.na(out[[ivar]])
      if (any(ivar.NA)){
        if (verbose) print(paste(ivar,"has missing!"))
        meta.preprocess$impute$done <<- TRUE
        meta.preprocess$impute$vars <<- c(meta.preprocess$impute$vars,ivar)
        if (verbose) print(paste("Missing Method:", NA.method))
        if (NA.method=="mean") out[[ivar]][which(ivar.NA)] <- mean(out[[ivar]],na.rm=TRUE)
        else if (NA.method=="value") out[[ivar]][ivar.NA] <- NA.value
      }
    }

    return(out)
  }

  do_range <- function(){
    if (verbose) print("crazyfy: RANGE")
    do.cols <- names(out)[sapply(out,is.numeric)]
    do.cols <- do.cols[!do.cols %in% id] # remove ID
    meta.preprocess$range <<- list(
      done=TRUE,
      vars=do.cols)
    for (ivar in do.cols){
      out[[ivar]] <- (out[[ivar]] - min(out[[ivar]],na.rm=TRUE))/max(out[[ivar]],na.rm=TRUE)
    }
    return(out)
  }

  do_scale <- function(){
    if (verbose) print("crazyfy: SCALE")
    do.cols <- names(out)[sapply(out,is.numeric)]
    meta.preprocess$scale <<- list(
      done=TRUE,
      vars=do.cols)
    for (ivar in do.cols){
      out[[ivar]] <- scale(out[[ivar]])[,1]
    }
    return(out)
  }

  ## Apply 'do' functions
  # If 'factor' is one of the method, ensure it is the first one applied
  if("factor" %in% do) do <- unique(c("factor",do))

  for (ido in do) out <- get(paste("do", ido, sep = "_"))()

  ### Manage duplicated vector of values resulting in 0 distance (problem for some weirds method)
  ### Hypothesis: only data used in computations, ID in .id columns
  ## For data.table adaptation: cf. https://stackoverflow.com/questions/45485373/create-a-mapping-table-of-duplicated-id-keys/45487193#45487193

  # print(class(out))
  dup <- as.data.table(out)
  # print(class(dup))
  dup <- dup[,g:=.GRP, by=cols]
  dup <- dup[,N:=.N,by=g]
  idmaptable <- dup[N>1,c(".id","g"),with=FALSE]
  setnames(idmaptable,c(".id","g"),c("mappedid",".id"))

  out <- dup[!duplicated(dup$g),-c("g","N"),with=FALSE]


  setattr(out,"meta",list(
    sourcevars=colnames(data)[!(colnames(data)%in% id)],
    id=id,
    had.id=had.id,
    preprocessing=meta.preprocess,
    has.duplicates=(nrow(idmaptable)>0),
    duplicated.mapping= idmaptable
  ))

  class(out)=c("crazy.data.table",class(out))
  return(out)
}

#---------------------------------------------------------------------------------

# EOF
