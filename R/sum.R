#' sum function with na.rm = TRUE
#'
#' Modifies nonsensical default behavior of the sum function with respect to NA
#' @param x a number
#' @keywords sum na.rm
#' @export
#' @examples
#' sum(c(1,2,3,NA))

sum <- function(x, ..., na.rm = TRUE) {
  base::sum(x, ..., na.rm = na.rm)
}

