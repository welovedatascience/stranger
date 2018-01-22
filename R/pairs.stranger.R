
pairs.stranger <- function(x,id=NULL,...){
  assertthat::assert_that(inherits(x,"stranger"),msg="x must be a stranger object")
  if (is.null(id)){
    if (".id" %in% colnames(x)) id <- ".id"
  } else {
    assertthat::assert_that(id%in% colnames(x),msg=paste("ID",id, "not found in stranger object"))
  }
  data <- as.data.frame(x[,colnames(x)!=id,with=FALSE]) # x is a data.table
  pairs(data,...)
}
##<TODO> test number of records, take a sample.
