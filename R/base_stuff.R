



"[.singular" <- function (x,...)
{
  cl <- class(x)
 # print(cl)
  out <- NextMethod("[",x) # will go to [.stranger
  class(out) <- cl
  attr(out,"meta") <- attr(x,"meta")
  return(out)
}


"[.stranger" <-function (x,...)
  {
  cl <- class(x)
  class(x) <- class(x)[!class(x) %in% c("singular","stranger")]
  out <- NextMethod("[",x) # will go to [.data.table
  class(out) <- cl
  attr(out,"meta") <- attr(x,"meta")
  return(out)
}


"[<-.singular" <-function (x,i,j,value)
{
  cl <- class(x)
  # print(cl)
  out <- NextMethod("[<-",x) # will go to [<-.stranger
  class(out) <- cl
  attr(out,"meta") <- attr(x,"meta")
  return(out)
}


"[<-.stranger" <-function (x,i,j,value)
{
  cl <- class(x)
  class(x) <- class(x)[!class(x) %in% c("singular","stranger")]
  out <- NextMethod("[<-",x) # will go to [<-.data.table
  class(out) <- cl
  attr(out,"meta") <- attr(x,"meta")
  return(out)
}
