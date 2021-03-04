#' Screen and tibble display preference function
#'
#' This function makes the screen wider given system environment
#' @param
#' @keywords screen width
#' @export
#' @examples
#' howWide=Sys.getenv("COLUMNS")
#' wide_screen()

wide_screen <- function(){
  num_cols <- Sys.getenv("COLUMNS")
  options(width=as.integer(num_cols))
  options(tibble.print_min = 30)
  options(scipen=999)
  options(pillar.min_chars = 30)
}
