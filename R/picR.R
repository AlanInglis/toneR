

#' picR
#' @description Display a graphic when process is complete
#'
#'
#'
#'
#' @return NULL
#'
#' @examples
#' art()
#'
#' @export

picR <- function(pic) {

  if(pic == 1){
    art <-
      c("", " -------------- ", "Process Complete ", " --------------", "    \\",
        "      \\", "        \\", "            |\\___/|", "          ==) ^Y^ (==",
        "            \\  ^  /", "             )=*=(", "            /     \\",
        "            |     |", "           /| | | |\\", "           \\| | |_|/\\",
        "      jgs  //_// ___/", "               \\_)", "  ")

  }else{
    art <- c('Complete Complete Complete')
  }

  suppressWarnings(
    return(cat(art, sep = "\n"))
  )
}
