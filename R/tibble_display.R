#' A tibble preference function
#'
#' This function makes tibbles wider to fit system environment
#' @param
#' @keywords tibble options
#' @export
#' @examples
#' tibble_display()

tibble_display <- function() {
  num_cols <- Sys.getenv("COLUMNS")
  options(width=as.integer(num_cols))
  options(tibble.print_min = 30)
}
