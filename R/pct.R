#' @importFrom magrittr %>%
NULL

#' Computes percentages by group
#'
#' @param df A data frame.
#' @param ... Some variables.
#' @importFrom dplyr mutate select summarise arrange group_by n is_grouped_df
#' @export
#' @return data frame
#' @examples
#' pct(mtcars, cyl)

pct <- function (df, ..., .drop = FALSE){
      if (!missing(...)) {
          df <- group_by(df, ..., .add = TRUE, .drop = .drop)
      }
      out <- dplyr::select(dplyr::mutate(dplyr::summarise(df, 
          n = n(), .groups = "drop"), pct = n/sum(n)), -n)
      return(out)
}


