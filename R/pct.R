#' @importFrom magrittr %>%
NULL

#' Computes percentages by group
#'
#' @param df A data frame.
#' @param ... Some variables.
#' @importFrom dplyr mutate select summarise arrange group_by n
#' @export
#' @return data frame
#' @examples
#' pct(mtcars, cyl)

pct <- function(df,...,.drop = FALSE){
   if (!missing(...)) {
     out <- group_by(df, ..., .add = TRUE, .drop = .drop)
   } else {
     out <- df
   }
  out <- out %>% dplyr::summarise(n = n(), .groups="drop") %>%
  dplyr::mutate(pct = n / sum(n)) %>%
  dplyr::select(-n)
  out <- out %>% arrange(...,pct)
  return(out)
}
