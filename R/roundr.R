#' A rounding function
#'
#' custom rounding function: number of digits depends on size of number
#' @param
#' @keywords roundr
#' @export
#' @importFrom stringr str_trim
#' @importFrom dplyr case_when
#' @examples
#' roundr(3.2)

roundr <- function(x){
  x = case_when(abs(x) >= 100 ~ format(round(x,digits = 0), nsmall = 0),
                abs(x) >= .1 ~ format(round(x,digits = 2), nsmall = 2),
                abs(x) < .1 ~ format(round(x,digits = 3), nsmall = 3),
                TRUE ~ NA_character_
  )
  x = str_trim(x)
  return(x)
}
