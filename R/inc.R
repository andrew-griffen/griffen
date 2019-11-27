#' An increment function
#'
#' This function increments a counter
#' @param x integer
#' @keywords
#' @export
#' @examples
#' x <- 1
#' inc(x), inc(3)

#increment function
inc <- function(x){eval.parent(substitute(x <- x + 1))}

