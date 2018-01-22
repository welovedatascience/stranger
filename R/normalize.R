

# to be put in NAMESPACE or DESCRIPTION (dependencies)
# library(assertthat)
# library(magrittr)
# library(data.table)
# library(dtplyr)


# WORKFLOW -- UNEXPORTED --------------------------------------------------

normalize <- function(x){
  assertthat::assert_that(inherits(x,"stranger"),msg="x must be a stranger object.")
  methods <- sapply(x,function(col)attr(col,"type"))

  norm_one  <- function(x,type="distance",normfoo=I){
    errFun <- function(x) 2 * pnorm(x * sqrt(2)) - 1
    x <- normfoo(x)  # from weird method metadata, we have special stuff for lofactor and abod
    out <- errFun(x = (x - mean(x)) / (sqrt(x = 2) * sd(x)))
    out[out < 0] <- 0
    attributes(out) <- attributes(x)
    attr(out,"normalized") <- TRUE
    return(out)
  }

  out <- lapply(x,
                function(col){
                  norm_one(col,type=attr(col,"type"),normfoo=attr(col,"normalizationFunction"))
                })
  out <- as.data.table(out)
  attributes(out) <- attributes(x)
  attr(out,"meta")$normalized <- TRUE
  class(out) <- class(x)
  return(out)
}

