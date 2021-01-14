#' Graph Style
#' This function sets the graphics to 'cairo-png' for better graphing in the IDE. It also sets
#' the ggplot2 theme to 'theme_light()'.
#' @return
#' @export
#'

light_and_cairo <- function() {


  trace(grDevices::png, quote({
    if (missing(type) && missing(antialias)) {
      type <- "cairo-png"
      antialias <- "subpixel"
    }
  }), print = FALSE)

  theme_set(new = theme_light())

}
