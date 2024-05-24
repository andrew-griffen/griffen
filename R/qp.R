#' Keeps track of question points
#'
#' I use this for homeworks because I can assign points 
#' and it will print a formatted number of points as well
#' as keep track of a running total.
#' @importFrom R6 R6Class
#' @export

qp <- R6Class("QuestionPoints",
  public = list(
    points = NULL,
    initialize = function(points = NA) {
      self$points <- vector()
    },
    question = function(p) {
        self$points <- c(self$points, p)
        question = paste0("(",tail(self$points, 1),")")
        return(question)
    },
    print = function(...) {
      cat("Total points:", sum(self$points), "\n")
      invisible(self)
    }
  )
)

