#' A timezone function
#'
#' This function returns the current time of some of my co-authors
#' @param
#' @keywords coauthors timezone
#' @importFrom lubridate with_tz
#' @export
#' @examples
#' time_coauthors()

time_coauthors <- function(time){
  if(missing(time)){
    time <- Sys.time()
  }else{
    if(!is.POSIXct(time)){
      if(is.numeric(time)){
        time <- as.character(time)
      }
      if(grepl(":",time)){
        time <- as.POSIXct(time,format="%H")
      }else{
        time <- as.POSIXct(time,format="%H:%M")
      }
    }
  }
  cat("たって今東京で",format(with_tz(time,tz="Asia/Tokyo"), "%H:%M"),"時間です","\n")
  cat("The current time in Bristol is ",format(with_tz(time,tz="GMT"), "%H:%M"),"\n")
  #cat("         -> From March the time will be","\n")
  cat("L'hora actual a Barcelona és a les ",format(with_tz(time,tz="CET"), "%H:%M"),"\n")
  #cat("         -> From March the time will be","\n")
  cat("The current time in Washington, D.C. is ",format(with_tz(time,tz="EST"), "%H:%M"),"\n")
  #cat("         -> From March the time will be","\n")
  cat("L'heure actuelle à Paris est ",format(with_tz(time,tz="CET"), "%H:%M"),"\n")
}



