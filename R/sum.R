#' sum function with na.rm = TRUE
#'
#' Modifies nonsensical default behavior of these functions
#' @param
#' @keywords sum na.rm
#' @export
#' @examples
#' sum(c(1,2,3,NA))

sum <- function(x, ..., na.rm = TRUE) {
  base::sum(x, ..., na.rm = na.rm)
}

