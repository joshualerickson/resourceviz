
#' Title
#'
#' @param aoi Area of Interest
#' @param maptype What google maptype to use,
#' @param zoom
#'
#' @return
#' @export
#' @note google map types: "terrain", "terrain-background", "satellite", "roadmap", "hybrid",
#' "toner", "watercolor", "terrain-labels", "terrain-lines", "toner-2010", "toner-2011",
#' "toner-background", "toner-hybrid", "toner-labels", "toner-lines", "toner-lite".

google_map <- function(aoi, maptype = 'terrain', source = 'google', zoom = 12){

  bbox <- stats::setNames(sf::st_bbox(sf::st_buffer(aoi, .05)), c("left", "bottom", "right", "top"))
  basemap_satellite <- ggmap::get_map(maptype = maptype, location = bbox, zoom = zoom, source = source)
  satellite <- ggmap::ggmap(basemap_satellite)


}
