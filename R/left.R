#' @importFrom magrittr %>%
NULL

#' An additional dplyr verb
#'
#' This moves either var or the last column to the first column
#' @param df a data frame
#' @keywords dplyr
#' @importFrom dplyr select everything
#' @export
#' @return data.frame
#' @examples
#' left(mtcars)

left <- function(df,...){
if(missing(...)){
  df <- df %>% dplyr::select(names(df)[length(df)],dplyr::everything())
}else{
  df <- df %>% dplyr::select(...,dplyr::everything())
}
return(df)
}