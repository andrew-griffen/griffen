#' An additional dplyr verb
#'
#' This moves either var or the last column to the first column
#' @param
#' @keywords dplyr
#' @export
#' @examples
#' tibble_display()
left <- function(df,var){
if(missing(var)){
  df <- df %>% dplyr::select(names(df)[length(df)],everything())
}else{
  df <- df %>% dplyr::select({{var}},everything())
}
return(df)
}
