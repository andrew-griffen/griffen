#' R6 method that keeps tracks of a counter increment and of total points
#'
#' Useful for making quizzes and exams
#' @keywords points
#' @importFrom R6 R6Class
#' @export

points <- R6Class("points", list(
  total = 0,
  i = 0,
  count = function(x = 1) {
    self$i <- self$i + 1 
    invisible(self)
  },
  p = function(x) {
    self$count()
    self$total <- self$total + x 
    cat(self$i, ". (", x, " points)\n",sep="")
    invisible(self)
  }
  )
)
