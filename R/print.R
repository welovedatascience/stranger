#' @method print anomalies
#' @export
print.anomalies <- function(x,...){
  print(as.vector(x))
  cat(paste0("\n*** Anomaly object containing ", length(x), " anomalies provided with their value for id variable '",attributes(x)$meta$crazymeta$id,"'.\n"))
  cat("Use as a vector. Available methods: fortify, plot")

}
