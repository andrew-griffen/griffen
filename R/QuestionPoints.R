#' Keeps track of question points
#'
#' This moves either var or the last column to the first column
#' @importFrom R6 R6Class
#' @export

QuestionPoints <- R6Class("QuestionPoints",
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

