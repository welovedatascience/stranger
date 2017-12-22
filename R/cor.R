


#'@export
setGeneric("cor")

#' Correlation for stranger objects
#'
#' describeIn cor Correlation method for stranger objects.
#' @export
setMethod("cor",signature(x="stranger"),function(x, method = c("pearson", "kendall", "spearman")){
  selectMethod("cor","ANY")(x[,-'.id',with=FALSE],y=NULL, use="everything",method=method)
})
