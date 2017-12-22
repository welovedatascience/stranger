#' @export
merge.stranger <- function(x,y,fix.names=TRUE,...){
  assertthat::assert_that(inherits(x,"stranger"),msg="x is not a stranger object.")
  assertthat::assert_that(inherits(y,"stranger"),msg="y is not a stranger object.")

  assertthat::assert_that(all(x[[".id"]]==y[[".id"]]), msg="x and  do not have same ID or same ordering.")
  mx=attr(x,"meta")
  my=attr(y, "meta")

  setkey(x,.id)
  setkey(y,.id)


  assertthat::assert_that(mx$normalized==my$normalized,msg="x and y do not have same normalization status.")
  assertthat::validate_that(all(mx$data==my$data),msg="Different source data for x and y. Using x ones. Unexpected outcome may happen.")
  out <- cbind(x,y[,-'.id',with=FALSE])
  if (fix.names) colnames(out) <- make.names(colnames(out),unique=TRUE)
  attr(out,"meta") <- mx
  class(out) <- c("stranger",class(out))
  return(out)

}
