#' Removes all factors from a data frame
#' @param
#' @keywords factor killah
#' @export
#' @examples
#' remove_factors(mtcars)

remove_factors <- function (df){
    for (var in names(df)) {
        if ("factor" %in% class(df[[var]])) {
            df[[var]] = as.character(df[[var]])
        }
    }
    return(df)
}
