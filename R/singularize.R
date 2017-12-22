
#' @export
singularize <- function(strangerObject,
                        methods=c("norm","rank"),
                        stacks=c("max","avg","min","damavg", "pruavg"),
                        prefix="N_anom",...)
{

  assertthat::assert_that(assertthat::is.string(prefix),assertthat::not_empty(prefix),msg="prefix must be an nonempty string.")
  assertthat::assert_that(inherits(strangerObject,"stranger"),msg="First argument must be a stranger Object.")

  methods <- match.arg(methods,c("rank","norm"),several.ok = TRUE)
  staks <- match.arg(stacks,c("max","avg","min","damavg", "pruavg"),several.ok=TRUE)


  meta <- attr(strangerObject,"meta")

  # Will prepare all stack, but only for all specified  methods:  default both  rank and  norm
  # binarization stacking method is chosen with the next method that binarize/flags anomalies (get_strangest)

  # start from an empty data.frame having same ID than our source data
  id <- select(strangerObject,.id)

  out <- id
  strangerObject <- select(strangerObject,-.id)

  ncomp <- ncol(strangerObject)



  buildStack <- function(df,stacks,ids=id,method="norm"){
    # note: method has to be defined in calling environment
    sname <- function(stack){paste(prefix,method,stack,sep="_")}
    ## build stacked derivatives
    if ("max" %in% stacks) ids[[sname("max")]] <- apply(df,1,max)
    if ("min" %in% stacks) ids[[sname("min")]] <- apply(df,1,min)
    if ("avg" %in% stacks) ids[[sname("avg")]] <- apply(df,1,mean)
    if ("damavg" %in% stacks) {
      if (method=="norm"){
        tmp <- apply(df,1,
                     function(x){
                       x[x <= 0] <- -10
                       x[x > 0] <- log(x[x > 0])
                       mean(x)})
        ids[[sname("damavg")]] <- (tmp - min(tmp))/(max(tmp)-min(tmp))
      } else {
        ids[[sname("damavg")]] <- apply(df,1,function(vec) mean(log(vec)))
      }

    }
    if ("pruavg" %in% stacks) {
      if (ncol(df)<2) {
        warning("Not enough columns to derive pruned Averages -- ignoring this derivation.")
      } else {
        ids[[sname("pruavg")]] <- apply(df,1,function(x) ifelse(min(x)==max(x),min(x),mean(x[x!=max(x)])))
      }
    }
    return(select(ids,-.id))
  }


  if ("rank" %in% methods){
    method <- "rank"

    ranks <- as.data.table(lapply(1:ncol(strangerObject),function(i){
      frankv(strangerObject[[i]], order = attr(strangerObject[[i]],"sort"))
    }))
    colnames(ranks) <- paste(prefix, method,names(strangerObject),sep="_")

    out <- cbind(out,ranks)

    # stacked versions
    if (ncomp > 1) {
      out <- cbind(out,buildStack(ranks, stacks,method="rank"))
    } else out <- cbind(out,buildStack(ranks, "avg",method="rank"))

  }


  if ("norm" %in% methods){
    method <- "norm"
    out.norm <- normalize(strangerObject)

    colnames(out.norm) <- paste(prefix,"norm",colnames(strangerObject),sep="_")

    out <- cbind(out,out.norm)


    # stacked versions
    # stacked versions
    if (ncomp > 1) {
      out <- cbind(out,buildStack(out.norm, stacks,method="norm"))
    } else out <- cbind(out,buildStack(out.norm, "avg",method="norm"))



  }
  out <- cbind(strangerObject,out)
  class(out) <- c("singular","stranger",class(out))
  attr(out,"meta") <- meta
  attr(out,"methods") <- methods
  attr(out,"stacks") <- stacks
  return(out)

}
