
# WEIRDNESS ------------------------------------------------------------------

#' Define a call to a weird function
#'
#' weirds function are wrapper around other packages anomalies
#' detection routines. \code{weird} function is intended to be
#' used when invoking \code{\link{stranger}} so that several anomalies
#' detection routines are used at one time.
#'
#' @param method - weird method to be used, check list of available
#' methods by using \code{\link{weirds_list}}
#' @param \dots - additional parameters to be passed to weird method
#' @return list with class weirdSpecs to be used as value when using
#' the parameter \code{tuneList} of \code{\link{stranger}} function.
#'
#' For every \code{weird} function, possible parameters in \dots should be checked by looking to the help of the underlying function. Some special common parameters are \code{info} to be set to TRUE to have information about the method, inclusing the name and package of the main underlying function. Another common parameter  is  \code{colname} that allow to overide default prefix name used for the outcome of the computation.
#'
#' Methods specific parameters, additional parameters may be used (for instance \code{simplify} for _knn_ method or \code{type} for kmeans. See the vignette describing the list of available _weirds_ methods in base package.

weird <- function(method, ...){
  # will build a call to weird_method (after checks of existence) to be used in
  # caretList equivalent function caretEnsemble for tuneList and caretModelSpec:
  # weird will specifically correspond to a caretModelSpec object and
  # return an object of class weirdSpec for instance

  assertthat::assert_that(is.character(method),msg="weird method must be a character string")
  assertthat::assert_that(methodCheck(method), msg="weird method is not available
                            -- check available weird methods with weird_list()")
  out <- c(list(weird = method), list(...))
  class(out) <- c("weirdSpecs",class(out))
  return(out)
}

#' Provides the list of available methods
#' @param onlynames - logical: should be return some details or only the nickname
#' of methods to be used in \code{\link{strange}}, \code{\link{stranger}},
#' \code{\link{weird}} or \code{\link{lucky_odds}}.

weirds_list <- function(onlynames=FALSE){
  infos <- t(sapply(apropos(what="weird_",mode="function",ignore.case=FALSE),
                    function(str_foo){
                      get(str_foo)(data=NULL,info=TRUE)
                    }))
  infos <- infos[!duplicated(infos),]
  installed <- ifelse(unlist(infos[,"package"]) %in% installed.packages(),"*","-")
  infos <- cbind(infos,installed)
  out <- list(methods=unlist(infos[,"name"]),detail=infos)
  if (onlynames) out <- out$methods
  return(out)
}

methodCheck <- function(m){
  assertthat::assert_that(
    all(m %in% weirds_list(onlynames=TRUE)),
    msg="One weird method not correctly defined or not available")
  }


# WEIRD METHODS -----------------------------------------------------------



#' weirdness wrappers for available anomalies detection methods
#'
#' Those wrapper are mainly used internally by other functions and the
#' recommended way is to start your detection process by using one of:
#'  \code{\link{strange}}, \code{\link{stranger}} or \code{\link{lucky_odds}} functions.
#'
#' For each weird wrapper, please refer to the documentation of the underlying
#' function in its package.
#'
#' @param data data pre-processed with \code{\link{crazyfy}} function
#' @param info logical: either run the function (default) or simply return some
#' information about it
#' @param colname character: optional prefix to be used to prepare the naming
#' of the generated column. All weird methods do have a default prefix (applied when
#' colname is \code{NULL}). Note that generated output column name will still have some
#' information added to reflect values used for some key parameters.
#' @param simplify character of function - used by knn to aggregates distances
#' to nearest k points.
#' @param \dots additional parameters to be passed to weird method, for instance k for knn
#' @rdname weirdness

weird_knn <- function(data=NULL,info=FALSE,colname=NULL,simplify="mean",...){

  # manage default parameters values
  weird.args <-list(...)
  if (!hasArg("k")) {
    weird.args$k=10
  }

  simpfoo <- match.fun(simplify)
  if (is.null(colname)) colname="knn" # default prefix name for this weird method
  if (!info & is.null(data)) {
    info <- TRUE
    warning("data is NULL, returning only information/metadata on this weird method.")
  }
  if(!info) assertthat::assert_that(inherits(data,"crazy.data.table"),msg="data must have been preprocessed with crazyfy function (you may perform none operation).")
  # ensure data if a crazy data.table

  meta <- list(
    weird_method="k-Nearest Neighbour",
    name="knn",
    package="FNN",
    package.source="CRAN",
    package.github=NULL,
    foo="knn.dist",
    type="distance",
    sort=-1,
    detail="Positive numeric value (distance)",
    parameters=c(weird.args,simplify=simplify),
    normalizationFunction=I
  )
  if(info) return(meta)

  assertthat::assert_that(require(meta$package,character.only=TRUE),msg=paste0("Package ",meta$package,"is required for this weird method."))

  weird.args <- c(list(data=data),weird.args)
  comp <- do.call(meta$foo,weird.args)

  comp <- data.frame(x=apply(comp,1,simpfoo))

  # build colname pattern
  colname <- paste(colname,paste("k",weird.args$k,sep="_"),simplify,sep="_")
  colnames(comp) <- colname
  meta$colname <- colname

  attributes(comp[,1]) <- meta
  class(comp) <- c("weird",class(comp))
  return(comp)
}
# data <- iris %>% select(-Species) %>% crazyfy()
# anom <- data %>% weird_knn()
# anom <- data %>% strange()

# weird_abod: ! normalization function

#' @rdname weirdness

weird_lof <- function(data=NULL,info=FALSE,colname=NULL,...){
  if (!info & is.null(data)) {
    info <- TRUE
    warning("data is NULL, returning only information/metadata on this weird method.")
  }
  if(!info) assertthat::assert_that(inherits(data,"crazy.data.table"),msg="data must have been preprocessed with crazyfy function (you may perform none operation).")
  # ensure data if a crazy data.table

  weird.args <-list(...)

  # manage default parameters values
  if (!hasArg("k")) {
    weird.args$k=10
  }


  if (is.null(colname)) colname="lof" # default prefix name for this weird method

  meta <- list(
    weird_method="Local Outlier Factor",
    name="lof",
    package="dbscan",
    package.source="CRAN",
    package.github=NULL,
    foo="lof",
    type="lofactor",
    sort=-1,
    detail="Positive numeric value (local outlier factors)",
    parameters=weird.args,
    normalizationFunction=function(x) x-1
  )
  if(info)return(meta)

  assertthat::assert_that(require(meta$package,character.only=TRUE),msg=paste0("Package ",meta$package,"FNN is required for this weird method."))

  weird.args <- c(list(x=data),weird.args)

  comp <- data.frame(x=do.call(meta$foo,weird.args))

  colname <- paste(colname,"k",weird.args$k,sep="_")
  colnames(comp) <- colname
  meta$colname  <- colname

  attributes(comp[,1]) <- meta

  class(comp) <- c("weird",class(comp))
  return(comp)
}

#' @rdname weirdness

weird_autoencode <- function(data=NULL,info=FALSE,colname=NULL,...){
  if (!info & is.null(data)) {
    info <- TRUE
    warning("data is NULL, returning only information/metadata on this weird method.")
  }
  if(!info) assertthat::assert_that(inherits(data,"crazy.data.table"),msg="data must have been preprocessed with crazyfy function (you may perform none operation).")
  # ensure data if a crazy data.table

  if (is.null(colname)) colname="autoencode" # default prefix name for this weird method


  # treat some default parameters (expert user may change using ...)
  # manage default parameters values
    weird.args <-list(...)
  if (!hasArg(nl))                weird.args$nl=3
  if (!hasArg(N.hidden))          weird.args$N.hidden=10
  if (!hasArg(unit.type))         weird.args$unit.type="tanh"
  if (!hasArg(lambda))            weird.args$lambda=0.0002
  if (!hasArg(beta))              weird.args$beta=6
  if (!hasArg(rho))               weird.args$rho=0.001
  if (!hasArg(epsilon))           weird.args$epsilon=0.0001
  if (!hasArg(optim.method))      weird.args$optim.method="BFGS"
  if (!hasArg(max.iterations))    weird.args$max.iterations=100
  if (!hasArg(rescale.flag))      weird.args$rescale.flag=TRUE
  #if (!hasArg(rescaling.offset))  weird.args$rescaling.offset=0.001 # alredy from autoencde function

  meta <- list(
    weird_method="autoencode",
    name="autoencode",
    package="autoencoder",
    package.source="CRAN",
    package.github=NULL,
    foo="autoencode",
    type="probability",
    sort=-1,
    detail="Positive numeric value (probability)",
    parameters=weird.args,
    normalizationFunction=I
  )
  if(info)return(meta)

  assertthat::assert_that(require(meta$package,character.only=TRUE),msg=paste0("Package ",meta$package," is required for this weird method."))

  # autoencoe expects a matrix
  weird.args <- c(list(X.train=as.matrix(data)),weird.args)

  # specific to weird method
  model <- do.call(meta$foo,weird.args) # don't change
  scores <- autoencoder::predict.autoencoder(model,X.input=as.matrix(data), hidden.output=FALSE)
  rajmse <-function(x_hat,x) rowMeans((x_hat-x)^2)
  scores2 <- rajmse(data, scores$X.output)
  comp <- as.data.frame(scores2)[,1,drop=FALSE]

  # Handle column names
  colname <- paste(colname,"nl",weird.args$nl,"Nhidden",paste(weird.args$N.hidden,collapse='_'),sep="_")
  colnames(comp) <- colname
  meta$colname  <- colname

  attributes(comp[,1]) <- meta

  class(comp) <- c("weird",class(comp))
  return(comp)
}



#' @rdname weirdness

weird_abod <- function(data=NULL,info=FALSE,colname=NULL,...){
  if (!info & is.null(data)) {
    info <- TRUE
    warning("data is NULL, returning only information/metadata on this weird method.")
  }
  if(!info) assertthat::assert_that(inherits(data,"crazy.data.table"),msg="data must have been preprocessed with crazyfy function (you may perform none operation).")
  # ensure data if a crazy data.table

  # weird.args <- list()
  weird.args <-list(...)

  # manage default parameters values (will be used to name column)

  # treat some default parameters (expert user may change using ...)
  # manage default parameters values
  if (!hasArg(method))            weird.args$method="randomized"
  if (!hasArg(k))                 weird.args$k=10



  if (is.null(colname)) colname="abod" # default prefix name for this weird method

  meta <- list(
    weird_method="Angle-based Outlier Factor",
    name="abod",
    package="abodOutlier",
    package.source="CRAN",
    package.github=NULL,
    foo="abod",
    type="abod",
    sort=1,
    detail="Numeric value (outlier factor)",
    parameters=weird.args,
    normalizationFunction=function(x) -log(x / max(x), base =10)
  )
  if(info)return(meta)


  assertthat::assert_that(require(meta$package,character.only=TRUE),msg=paste0("Package ",meta$package," is required for this weird method."))

  weird.args <- c(list(data=data),weird.args)
  comp <- do.call(meta$foo,weird.args)
  comp <- data.frame(x=comp)

  colname <- paste(colname,"k",weird.args$k,sep="_")
  colnames(comp) <- colname
  meta$colname  <- colname

  attributes(comp[,1]) <- meta

  class(comp) <- c("weird",class(comp))
  return(comp)
}





#' @rdname weirdness

weird_pcout <- function(data=NULL,info=FALSE,colname=NULL,...){
  if (!info & is.null(data)) {
    info <- TRUE
    warning("data is NULL, returning only information/metadata on this weird method.")
  }
  if(!info) assertthat::assert_that(inherits(data,"crazy.data.table"),msg="data must have been preprocessed with crazyfy function (you may perform none operation).")
  # ensure data if a crazy data.table

  # weird.args <- list()
  weird.args <-list(...)

  # manage default parameters values (will be used to name column)

  # treat some default parameters (expert user may change using ...)
  # manage default parameters values
  weird.args$makeplot=FALSE

  if (!hasArg(explvar))           weird.args$explvar=0.99
  if (!hasArg(crit.M1))           weird.args$crit.M1=1/3
  if (!hasArg(crit.M2))           weird.args$crit.M2=1/4
  if (!hasArg(crit.c2))           weird.args$crit.c2=0.99
  if (!hasArg(crit.cs))           weird.args$crit.cs=0.25
  if (!hasArg(outbound))          weird.args$outbound=0.25

  if (is.null(colname)) colname="abod" # default prefix name for this weird method

  meta <- list(
    weird_method="Semi-robust principal components > distances",
    name="pcout",
    package="mvoutlier",
    package.source="CRAN",
    package.github=NULL,
    foo="pcout",
    type="distance",
    sort=-1,
    detail="Positive numeric value (distance)",
    parameters=weird.args,
    normalizationFunction=I
  )
  if(info)return(meta)


  assertthat::assert_that(require(meta$package,character.only=TRUE),msg=paste0("Package ",meta$package," is required for this weird method."))

  weird.args <- c(list(x=data),weird.args)
  comp <- do.call(meta$foo,weird.args)
  comp <- data.frame(x=comp$x.dist1)

  colname=paste(colname,weird.args$explvar,sep="_")
  colnames(comp) <- colname
  meta$colname  <- colname

  attributes(comp[,1]) <- meta

  class(comp) <- c("weird",class(comp))
  return(comp)
}





#' @rdname weirdness

weird_isofor <- function(data=NULL,info=FALSE,colname=NULL,...){
  if (!info & is.null(data)) {
    info <- TRUE
    warning("data is NULL, returning only information/metadata on this weird method.")
  }
  if(!info) assertthat::assert_that(inherits(data,"crazy.data.table"),msg="data must have been preprocessed with crazyfy function (you may perform none operation).")
  # ensure data if a crazy data.table

  weird.args <-list(...)

  # manage default parameters values
  # Take from package/foo defaults
  if (!hasArg("nt")) weird.args$nt=100
  if (!hasArg("phi")) weird.args$phi=min(nrow(data)-1,256)
  if (!hasArg("seed")) weird.args$seed=1234
  if (!hasArg("multicore")) weird.args$multicore=FALSE
  if (!hasArg("replace_missing")) weird.args$replace_missing=TRUE
  if (!hasArg("sentinel")) weird.args$sentinel=-9999999999


  if (is.null(colname)) colname="isofor" # default prefix name for this weird method

  meta <- list(
    weird_method="isolation Forest",
    name="isofor",
    package="isofor",
    package.source="github",
    package.github="Zelazny7/isofor",
    foo="iForest",
    type="distance",
    sort=-1,
    detail="Positive numeric value (distance)",
    parameters=weird.args,
    normalizationFunction=I
  )
  if(info)return(meta)

  assertthat::assert_that(require(meta$package,character.only=TRUE),msg=paste0("Package ",meta$package,paste(metapackage,"is required for this weird method.")))

  weird.args <- c(list(X=data),weird.args)

  iforest <- do.call(meta$foo,weird.args)
  comp <- data.frame(x=predict(iforest,newdata=data))

  colname <- paste(colname,"nt",weird.args$nt,"phi",weird.args$phi,sep="_")
  colnames(comp) <- colname
  meta$colname  <- colname

  attributes(comp[,1]) <- meta

  class(comp) <- c("weird",class(comp))
  return(comp)
}
## anom <- data %>% strange(weird="isofor",phi=30)
## anom <- data %>% strange(weird="isofor",nt=30)



#' @rdname weirdness
#' @param type one of "means" or "euclidian". See \code{kmeans} from \code{stats} package.

weird_kmeans <- function(data=NULL,info=FALSE,colname=NULL,type="means",...){
  if (!info & is.null(data)) {
    info <- TRUE
    warning("data is NULL, returning only information/metadata on this weird method.")
  }
  if(!info) assertthat::assert_that(inherits(data,"crazy.data.table"),msg="data must have been preprocessed with crazyfy function (you may perform none operation).")
  # ensure data if a crazy data.table

  weird.args <-list(...)

  # manage default parameters values
  # Take from package/foo defaults

  type <- match.arg( type,c("means","euclidian"))

   if (!hasArg("centers")) weird.args$centers=4
  if (!hasArg("algorithm")) weird.args$algorithm="Hartigan-Wong"
  if (!hasArg("iter.max")) weird.args$iter.max=10
  if (!hasArg("nstart")) weird.args$nstart=weird.args$centers


  if (is.null(colname)) colname="kmeans" # default prefix name for this weird method

  meta <- list(
    weird_method=paste0("kmeans (",weird.args$type,")"),
    name="kmeans",
    package="stats",
    package.source="CRAN",
    package.github=NULL,
    foo="kmeans",
    type="distance",
    sort=-1,
    detail="Positive numeric value (distance)",
    parameters=weird.args,
    normalizationFunction=I
  )
  if(info)return(meta)

  assertthat::assert_that(require(meta$package,character.only=TRUE),msg=paste0("Package ",meta$package,paste(metapackage,"is required for this weird method.")))

  weird.args <- c(list(x=data),weird.args)

  res.kmeans <- do.call(meta$foo,weird.args)
  centers <- res.kmeans$centers[res.kmeans$cluster, ]
  d <- sqrt(rowSums((data - centers)^2))

  if (type=="euclidian"){
    comp <- data.frame(x=d)

  }
  else {
    # means
    m <- tapply(d, res.kmeans$cluster,mean)
    distance <- d/(m[res.kmeans$cluster])
    comp <- data.frame(x=as.numeric(distance))
  }


  colname <- paste(colname,"type",type,"centers",paste(weird.args$centers,collapse="_",sep="_"),sep="_")
  colnames(comp) <- colname
  meta$colname  <- colname

  attributes(comp[,1]) <- meta

  class(comp) <- c("weird",class(comp))
  return(comp)
}

## anom <- data %>% strange(weird="kmeans")
## anom <- data %>% strange(weird="kmeans",type="euclidian",centers=10)




#' @rdname weirdness

weird_mahalanobis <- function(data=NULL,info=FALSE,colname=NULL,...){
  if (!info & is.null(data)) {
    info <- TRUE
    warning("data is NULL, returning only information/metadata on this weird method.")
  }
  if(!info) assertthat::assert_that(inherits(data,"crazy.data.table"),msg="data must have been preprocessed with crazyfy function (you may perform none operation).")
  # ensure data if a crazy data.table

  weird.args <-list(...)

  # manage default parameters values
  # Take from package/foo defaults

  if (is.null(colname)) colname="mahalanobis" # default prefix name for this weird method

  meta <- list(
    weird_method="Mahalanobis distance",
    name="mahalanobis",
    package="stats",
    package.source="CRAN",
    package.github=NULL,
    foo="mahalanobis",
    type="distance",
    sort=-1,
    detail="Positive numeric value (distance)",
    parameters=weird.args,
    normalizationFunction=I
  )
  if(info)return(meta)

  assertthat::assert_that(require(meta$package,character.only=TRUE),msg=paste0("Package ",meta$package,paste(metapackage,"is required for this weird method.")))

  weird.args <- c(list(x=data),c(center=list(colMeans(data)), cov=list(cov(data)),weird.args))


  comp <- data.frame(x=do.call(meta$foo,weird.args))

  # colname <- paste(colname,"nt",weird.args$nt,"phi",weird.args$phi,sep="_")
  colnames(comp) <- colname
  meta$colname  <- colname

  attributes(comp[,1]) <- meta

  class(comp) <- c("weird",class(comp))
  return(comp)
}

## anom <- data %>% strange(weird="mahalanobis")


#' @rdname weirdness

weird_randomforest <- function(data=NULL,info=FALSE,colname=NULL,...){
  if (!info & is.null(data)) {
    info <- TRUE
    warning("data is NULL, returning only information/metadata on this weird method.")
  }
  if(!info) assertthat::assert_that(inherits(data,"crazy.data.table"),msg="data must have been preprocessed with crazyfy function (you may perform none operation).")
  # ensure data if a crazy data.table

  weird.args <-list(...)

  # manage default parameters values
  # Take from package/foo defaults

  if (!hasArg("ntree")) weird.args$ntree=500
  if (!hasArg("mtry")) weird.args$mtry=ifelse(is.null(data),"sqrt(ncol(data))",floor(sqrt(ncol(data))))
  if (!hasArg("replace")) weird.args$replace=TRUE



  if (is.null(colname)) colname="randomforest" # default prefix name for this weird method

  meta <- list(
    weird_method="randomforest outlier metric",
    name="randomforest",
    package="randomForest",
    package.source="CRAN",
    package.github=NULL,
    foo="randomForest",
    type="distance",
    sort=-1,
    detail="Positive numeric value (distance)",
    parameters=weird.args,
    normalizationFunction=I
  )
  if(info)return(meta)

  assertthat::assert_that(require(meta$package,character.only=TRUE),msg=paste0("Package ",meta$package,paste(metapackage,"is required for this weird method.")))

  weird.args <- c(list(x=data),weird.args)


  rf <- do.call(meta$foo,weird.args)
  d <- as.data.frame(outlier(rf))
  comp <- d[,1,drop=FALSE]

  colname <- paste(colname,"ntree",weird.args$ntree,"mtry",weird.args$mtry,sep="_")
  colnames(comp) <- colname
  meta$colname  <- colname

  attributes(comp[,1]) <- meta

  class(comp) <- c("weird",class(comp))
  return(comp)
}


## anom <- data %>% strange(weird="randomforest")

