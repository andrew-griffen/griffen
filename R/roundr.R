#' A rounding function
#'
#' custom rounding function: number of digits depends on size of number
#' @param
#' @keywords roundr
#' @export
#' @examples
#' roundr(3.2)

roundr <- function(x){
  if(is.null(x) | is.na(x) | is.nan(x)){return(x <- NA)}
  if(abs(x)<100){
    x <- format(round(x, 2), nsmall = 2)
  }else{
    if(abs(x)<1000){
      x <- format(round(x, 1), nsmall = 1)
    }else{
      x <- format(round(x, 0),big.mark=",",scientific=FALSE, nsmall = 0)
    }
  }
  return(x)
}
