#' mean function with na.rm = TRUE
#'
#' Modifies nonsensical default behavior of the mean function with respect to NA
#' @param x a number
#' @keywords mean na.rm
#' @export
#' @examples
#' mean(c(1,2,3,NA))

mean <- function(x, ..., na.rm = TRUE) {
  base::mean(x, ..., na.rm = na.rm)
}

