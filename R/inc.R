#' An increment function
#'
#' This function increments a counter
#' @param x integer
#' @keywords increment
#' @export
#' @examples
#' x <- 1
#' inc(x)

inc <- function(x){eval.parent(substitute(x <- x + 1))}

#' An increment function
#'
#' This function increments a counter but prints as well
#' @param x integer
#' @keywords increment
#' @export
#' @examples
#' x <- 1
#' incp(x)

incp <- function(x){
  x = eval.parent(substitute(x <- x + 1))
  return(x)
}

