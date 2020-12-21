
#' grab arg names
#'
#' @param data
#'
#' @return
#'
#' @examples

grab_names <- function(data) {

lng <- length(data$x$calls)-7
names <- vector()
for(i in 1:lng){

  fam <- data$x$calls[[7+i]]$args[[3]]

  names <- append(fam, names)

}
names
}
