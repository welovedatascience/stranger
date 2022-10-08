#' Enrich source data with an ID
#'
#' \code{add_id} just ensures data is a data.table and adds a columns named .id
#'
#' \code{add_id} in called behing the scence by \code{\link{crazyfy}}
#' if needed to ensure a valid id is present in source data.
#' Still, we recommand to start from a dataset already having an id, be it generated
#' by a call to \code{add_id} or already pre-existing to ensure correct source data enrichment.
#'
#' @param data Source data (data.frame or data.table)
#' @return data.table object with a column named .id
#' @examples
#' library(stranger)
#' data(iris)
#' (iris.id <- add_id(iris))
#' @export
add_id <- function(data){
  assertthat::assert_that(inherits(data,"data.frame"),msg="data must be a data.frame or a data.table")
  assertthat::assert_that(ncol(data)>0 & nrow(data)>0,msg="data must be non empty")
  data <- as.data.table(data)
  data[[".id"]] <- 1:nrow(data)
  return(data)
}
