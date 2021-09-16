#' Visualize 30m Climatic Water Deficit
#'
#' @return
#' @export
#'

viz_Def <- function(data) {

  defPal <- leaflet::colorQuantile("RdBu", def, reverse = TRUE, na.color = "transparent")
  data %>% leaflet::addRasterImage(def,
                         colors = defPal,
                         opacity = 0.5,
                         group = 'def') %>%
    leaflet::addLegend(pal = defPal, values = raster::values(def),
              title = "30m Water Deficit") %>%
    leafem::addImageQuery(def, project = TRUE,
                  layerId = "def")

}

#' Visualize 30m TWI
#'
#' @return

#'

viz_TWI <- function(data) {

  twiPal <- leaflet::colorQuantile("RdBu", twi, na.color = "transparent")
  data %>% leaflet::addRasterImage(twi,
                          colors = twiPal,
                          opacity = 0.5,
                          group = 'twi') %>%
    leaflet::addLegend(pal = twiPal, values = raster::values(twi),
              title = "10m TWI") %>%
    leafem::addImageQuery(data, project = TRUE,
                  layerId = "twi")

}
