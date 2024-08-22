#' var function with na.rm = TRUE
#'
#' Modifies nonsensical default behavior of the var function with respect to NA
#' @param x a number
#' @keywords var na.rm
#' @export
#' @examples
#' var(c(1,2,3,NA))

var <- function(x, ..., na.rm = TRUE) {
  stats::var(x, ..., na.rm = na.rm)
}


