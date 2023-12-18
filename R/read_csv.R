#' mean function with na.rm = TRUE
#'
#' Shuts up read_csv's incessant printing of crap
#' @param
#' @keywords read_csv na.rm
#' @importFrom readr read_csv
#' @export

read_csv <- function(file, ..., show_col_types = FALSE) {
  readr::read_csv(file, ..., show_col_types = show_col_types)
}