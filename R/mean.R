#' mean function with na.rm = TRUE
#'
#' Modifies nonsensical default behavior of these functions
#' @param
#' @keywords mean na.rm
#' @export
#' @examples
#' mean(c(1,2,3,NA))

mean <- function(x, ..., na.rm = TRUE) {
  base::mean(x, ..., na.rm = na.rm)
}

