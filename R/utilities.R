# UTILITIES  ---------------------------------------------------------------



skewness <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  n <- length(x)
  M3 <- sum((x - mean(x)) ^ 3) / n
  M2 <- sum((x - mean(x)) ^ 2) / n
  skew <- M3 / M2 ^ (3 / 2)
  return(skew)
}

modefreq <- function(x) {
  # compute mode(s) and associated frequency; works with factors and handles NA
  ## adaptation from: https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  mode <- ux[tab == max(tab)]
  if(is.na(mode[1])) freq=sum(is.na(x))/length(x) else freq=sum(x==mode[1],na.rm=TRUE)/length(x)
  return(list(mode=mode,freq=freq))
}

#' @export
install.weirdness <- function(upgrade=FALSE,github=TRUE,...){
  # check with weirds and (re)install packages
  # ... : paramaters to be passed to install or install_github (if github=TRUE)
}
