
#' Leaflet with USGS basemaps
#'
#' @return A leaflet map with USGS basemaps
#' @export
#' @imort ggplot2
#'
#' @examples
viz_Area <- function() {

  grp <- c("USGS Topo", "USGS Imagery Only", "USGS Imagery Topo",
           "USGS Shaded Relief", "Hydrography")
  att <- paste0("<a href='https://www.usgs.gov/'>",
                "U.S. Geological Survey</a> | ",
                "<a href='https://www.usgs.gov/laws/policies_notices.html'>",
                "Policies</a>")
  GetURL <- function(service, host = "basemap.nationalmap.gov") {
    sprintf("https://%s/arcgis/services/%s/MapServer/WmsServer", host, service)
  }

  map <- leaflet::leaflet()
  map <- leaflet::addWMSTiles(map, GetURL("USGSTopo"),
                              group = grp[1], attribution = att, layers = "0")
  map <- leaflet::addWMSTiles(map, GetURL("USGSImageryOnly"),
                              group = grp[2], attribution = att, layers = "0")
  map <- leaflet::addWMSTiles(map, GetURL("USGSImageryTopo"),
                              group = grp[3], attribution = att, layers = "0")
  map <- leaflet::addWMSTiles(map, GetURL("USGSShadedReliefOnly"),
                              group = grp[4], attribution = att, layers = "0")

  opt <- leaflet::WMSTileOptions(format = "image/png", transparent = TRUE)
  map <- leaflet::addWMSTiles(map, GetURL("USGSHydroCached"),
                              group = grp[5], options = opt, layers = "0")
  map <- leaflet::hideGroup(map, grp[5])
  opt <- leaflet::layersControlOptions(collapsed = TRUE)
  map <- leaflet::addLayersControl(map, baseGroups = grp[1:4],
                                   overlayGroups = grp[5], options = opt)
  map %>%
    leafem::addMouseCoordinates(epsg = "EPSG:4326", proj4string = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

}



#' Leaflet with INFRA Roads
#'
#' @param INFRA A INFRA road layer. Variable must be all caps.
#'
#' @return A leafet map with roads colored by Route Status and popup with select info.
#' @export
#'
#' @examples
viz_Roads <- function(INFRA) {

  INFRA <- sf::st_zm(INFRA) %>%
    sf::st_transform(crs = 4326,proj4string = "+init=epsg:4326")

  rdPal <- leaflet::colorFactor(c("orange", "red", "blue", "black"), INFRA$ROUTE_STAT)

  viz_Area() %>%  leaflet::addPolylines(data = INFRA, color = ~rdPal(ROUTE_STAT),
                                        group = "road",
                                     popup = paste0("<b>","ID: ","</b>", INFRA$ID, " Sq.Miles",
                                                    "<br>","<b>", "Name: ", "</b>", INFRA$NAME,
                                                    "<br>", "<b>", "Length: ", "</b>",INFRA$SEG_LENGTH, " mi",
                                                    "<br>", "<b>", "Route Status: ", "</b>",INFRA$ROUTE_STAT,
                                                    "<br>", "<b>", "Maintenance Level: ", "</b>", INFRA$ML,
                                                    "<br>", "<b>", "Surface Type: ", "</b>", INFRA$SURFACE_TY,
                                                    "<br>", "<b>", "Jurisdiction: ", "</b>", INFRA$JURISDICTI))

}


#' Visualize District Geology
#'
#' @return
#' @export
#'

viz_Geo <- function() {

  rockPal <- leaflet::colorFactor("Paired", geo$ROCKTYPE1)
  viz_Area() %>% leaflet::addPolygons(data = geo,
                                      color = ~rockPal(ROCKTYPE1),
                                      popup = paste0("<b>","Rocktype 1: ","</b>", geo$ROCKTYPE1,
                                                     "<br>","<b>", "Rocktype 2: ", "</b>", geo$ROCKTYPE2,
                                                     "<br>", "<b>", "Unit Age ", "</b>",geo$UNIT_AGE,
                                                     "<br>", "<b>", "Unit Label: ", "</b>",geo$ORIG_LABEL))

}


