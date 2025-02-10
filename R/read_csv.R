#' mean function with na.rm = TRUE
#'
#' Tries to shuts up read_csv's incessant printing of crap
#' @param file a csv file
#' @keywords read_csv na.rm
#' @importFrom readr read_csv
#' @export

read_csv <- function(file, ..., show_col_types = FALSE, progress = FALSE) {
  readr::read_csv(file, ..., show_col_types = show_col_types, progress = progress)
}