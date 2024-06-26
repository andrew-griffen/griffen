#' Screen and tibble display preference function
#'
#' This function makes the screen wider given system environment
#' @keywords screen width
#' @export
#' @examples
#' howWide=Sys.getenv("COLUMNS")
#' wide_screen()

wide_screen <- function(){
  num_cols = as.integer(Sys.getenv("COLUMNS"))
  try(options(width=num_cols, silent = TRUE))
  options(tibble.print_min = 15)
  options(scipen=999)
  options(pillar.min_chars = 30)
  options(pillar.min_title_chars = 20)
  options(pillar.advice = FALSE)
  options(pillar.max_footer_lines=1)
}
