
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

#' Get Stream Stats Stream Tile
#'
#' @param state Must be a single state abbreviation or full name
#' @return A mapedit result
#' @export
#'
get_StreamStats_points <- function(state){

  if(missing(state)){stop('need a state tile map to choose from')}

  state_num <- data.frame(id = c(0,seq(3,30,3),seq(34,64,3),
                                 seq(68,143,3)),
                          abb = c('AK','AL','AR','AZ', 'CA', 'CO',
                                  'CRB', 'CT', 'DE', 'DRB', 'GA',
                                  'HI', 'IA', 'ID', 'IL', 'IN', 'KS',
                                  'KY', 'MA', 'MD', 'ME', 'MN', 'MO',
                                  'MO STL', 'MS', 'MT', 'NC', 'ND',
                                  'NH', 'NJ', 'NM', 'NY', 'OH', 'OK',
                                  'OR', 'PA', 'PR', 'RI', 'RRB', 'SC',
                                  'SD', 'TN', 'UT', 'VA', 'VT', 'WA',
                                  'WI', 'WV'))

  state_num <- state_num %>%
    dplyr::filter(abb %in% state)
  grp <- c("Esri.WorldImagery", "CartoDB.Positron",
           "OpenStreetMap", "CartoDB.DarkMatter", "OpenTopoMap",
           "Hydrography")
  att <- paste0("<a href='https://www.usgs.gov/'>", "U.S. Geological Survey</a> | ",
                "<a href='https://www.usgs.gov/laws/policies_notices.html'>",
                "Policies</a>")
  GetURL <- function(service, host = "basemap.nationalmap.gov") {
    sprintf("https://%s/arcgis/services/%s/MapServer/WmsServer",
            host, service)
  }
  map <- leaflet::leaflet()
  map <- leaflet::addProviderTiles(map = map, provider = grp[[1]],
                                   group = grp[[1]])
  map <- leaflet::addProviderTiles(map = map, provider = grp[[2]],
                                   group = grp[[2]])
  map <- leaflet::addProviderTiles(map = map, provider = grp[[3]],
                                   group = grp[[3]])
  map <- leaflet::addProviderTiles(map = map, provider = grp[[4]],
                                   group = grp[[4]])
  map <- leaflet::addProviderTiles(map = map, provider = grp[[5]],
                                   group = grp[[5]])
  opt <- leaflet::WMSTileOptions(format = "image/png",
                                 transparent = TRUE)
  map <- leaflet::addWMSTiles(map, GetURL("USGSHydroCached"),
                              group = grp[6], options = opt, layers = "0", attribution = att)
  opt <- leaflet::layersControlOptions(collapsed = TRUE)
  map <- leaflet::addLayersControl(map, baseGroups = grp[1:5],
                                   overlayGroups = grp[6], options = opt)
  map <- map %>% leafem::addMouseCoordinates(epsg = "EPSG:4326",
                                             proj4string = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  url <- 'https://gis.streamstats.usgs.gov/arcgis/rest/services/StreamStats/stateServices/MapServer'

  map <- map %>%
    leaflet.esri::addEsriDynamicMapLayer(url = url,
                                         options =  leaflet.esri::dynamicMapLayerOptions(minZoom = 12,
                                                                                         layers = list(state_num$id))) %>%
    mapedit::drawFeatures(map = .)
}


#' Add R2 to ggplot
#' @param ... Additional params to add to stat_poly_eq.
#' @description This is an idea and code taken from
#' Anatolli Tsyplenkov https://github.com/atsyplenkov to add the R2 statistic to a ggplot.
#' @return A ggplot layer
#' @export
#'

Add_R2 <- function(...){

  list(
    ggpmisc::stat_poly_eq(aes(label =  paste(stat(eq.label),
                                             stat(rr.label),
                                             sep = "~~~~")),
                          formula = y~x,
                          rr.digits = 2,
                          # coef.digits = 2,
                          parse = TRUE,
                          ...),
    ggplot2::geom_smooth(
                         linetype = 'dashed',
                         method = 'lm',
                         formula = y~x,
                         se = F)
  )
}


#' Helper List Function
#' @description This helps get the stations with the transducers into a list
#' after they've been processed.
#' @param names A character vector indicating names you want discarded in list.
#' @return A list
#' @export
#'

sites_to_list <- function(names = NULL) {

  list_sites <- list('Pony' = pony_wl_final,
       'Edna' = edna_wl_final,
       'Stahl' = stahl_wl_final,
       'Brimstone' = brimstone_wl_final,
       'Middle Fortine' = fortine_wl_final,
       'Parsnip' = parsnip_wl_final,
       'Sutton' = sutton_wl_final)

  if(!is.null(names)){

  list_sites <- purrr::discard(list_sites, names(list_sites) %in% names)
  }

  list_sites
}
