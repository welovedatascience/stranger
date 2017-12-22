
#' @export
"[.singular" <- function (x,...)
{
  cl <- class(x)
 # print(cl)
  out <- NextMethod("[",x) # will go to [.stranger
  class(out) <- cl
  attr(out,"meta") <- attr(x,"meta")
  return(out)
}

#' @export
"[.stranger" <-function (x,...)
  {
  cl <- class(x)
  class(x) <- class(x)[!class(x) %in% c("singular","stranger")]
  out <- NextMethod("[",x) # will go to [.data.table
  class(out) <- cl
  attr(out,"meta") <- attr(x,"meta")
  return(out)
}

#' @export
"[<-.singular" <-function (x,...)
{
  cl <- class(x)
  # print(cl)
  out <- NextMethod("[<-",x) # will go to [<-.stranger
  class(out) <- cl
  attr(out,"meta") <- attr(x,"meta")
  return(out)
}

#' @export
"[<-.stranger" <-function (x,...)
{
  cl <- class(x)
  class(x) <- class(x)[!class(x) %in% c("singular","stranger")]
  out <- NextMethod("[<-",x) # will go to [<-.data.table
  class(out) <- cl
  attr(out,"meta") <- attr(x,"meta")
  return(out)
}
