

# cf. https://github.com/tidyverse/dplyr/issues/719
# Also have a look at what dtplyr does
# <todo>: https://github.com/hadley/dtplyr/blob/master/R/compat-dplyr-0.6.0.R
# We may prevent having to explicitely export those functions

#' dplyr methods
#'


## NEW REQUIREMENT dplyr>...0.7.4
#' @rdname dplyr-methods
#' @export
filter.stranger <- function(.data, ...) {    # dplyr 0.7.4+
  cl <- class(.data)
  out <-dplyr:::filter.default(as.data.frame(.data),...)
  class(out) <- cl
  attr(out,"meta") <- attr(.data,"meta")
  return(out)
}

#' @rdname dplyr-methods
#' @export
filter.singular <- function(.data,...){     # dplyr 0.7.4+
  cl <- class(.data)
  out <-dplyr:::filter.default(as.data.frame(.data),...)
  class(out) <- cl
  attr(out,"meta") <- attr(.data,"meta")
  return(out)
}

# filter_.scanonevar <- function(vs, ...) {
#   out <- vs
#   class(out) <- class(out)[-1]
#   out <- dplyr::filter_(out, ...)
#   class(out) <- class(vs)
#   attr(out, 'attr1') <- attr(vs, 'attr1')
#   return(out)
# }

as.data.frame.singular <- function(x,...){
  class(x) <- c("data.frame")
  return(x)
}


as.data.frame.stranger <- function(x,...){
  class(x) <- c("data.frame")
  return(x)
}
