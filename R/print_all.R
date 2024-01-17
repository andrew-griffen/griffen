#' @importFrom magrittr %>%
NULL

#' A modified print function
#'
#' This function print all rows of a data frame
#' @param df a data frame
#' @keywords print rows
#' @export
#' @examples
#' print_all(mtcars)

print_all <- function(df){
  if(inherits(df, "tbl_df")){
    df %>% print(n=nrow(df))
  }else{
    df %>% print()
  }
}

