#' Prints a vector vertically
#'
#' @param vec a vector
#' @keywords as_vert
#' @export
#' @examples
#' as_vert(letters)

as_vert <- function(vec){
  for (i in 1:length(vec)){
    if(!is.null(names(vec)[i])){
      cat(names(vec)[i],"=",vec[i],"\n")
    }else{
      cat(vec[i],"\n")
    }
  }
}

#' Prints a vector horizontally
#'
#' @param vec a vector
#' @keywords as_hori
#' @export
#' @examples
#' as_hori(letters)

as_hori <- function(vec){
  for (i in 1:length(vec)){
    if(!is.null(names(vec)[i])){
      cat(names(vec)[i],"=",vec[i]," ")
    }else{
      cat(vec[i]," ")
    }
  }
  cat("\n")
}

#' Prints a vector how you would write a vector
#'
#' @param vec a vector
#' @param name a logical
#' @keywords as_vec
#' @export
#' @examples
#' as_vec(letters)

as_vec <- function(vec, name = TRUE){

  char = is.character(vec)
  vec_name = deparse(substitute(vec))

  #if want to use vec name or not
  if (name){
    cat(paste0(vec_name," = c("))
  }else{
    cat("c(")
  }

  #loop over vector
  for (i in 1:length(vec)){
    #check whether vector is named or not
    if(!is.null(names(vec)[i])){
      cat(paste0('\"',names(vec)[i],'\"'),"=")
    }
    if (char){
      cat('\"',vec[i],'\"',sep="")
      #cat(vec[i])
    }else{
      cat(vec[i])
    }
    if (i<length(vec)){
      cat(", ")
    }else{
      cat(")")
    }
  }
  cat("\n")
}


