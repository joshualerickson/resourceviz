
#' Leaflet with USGS basemaps
#'
#' @return A leaflet map with USGS basemaps
#' @export
#' @import ggplot2
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
#' @param data A INFRA road layer. Variable must be all caps.
#'
#' @return A leafet map with roads colored by Route Status and popup with select info.
#' @export
#'
#' @examples
viz_Roads <- function(data) {

  rdPal <- leaflet::colorFactor(c("orange", "red", "blue", "black"), INFRA$ROUTE_STAT)

 data %>% leaflet::addPolylines(data = INFRA, color = ~rdPal(ROUTE_STAT),
                                        group = "road",
                                     popup = paste0("<b>","ID: ","</b>", INFRA$ID,
                                                    "<br>","<b>", "Name: ", "</b>", INFRA$NAME,
                                                    "<br>", "<b>", "Length: ", "</b>",INFRA$SEG_LENGTH, " mi",
                                                    "<br>", "<b>", "Route Status: ", "</b>",INFRA$ROUTE_STAT,
                                                    "<br>", "<b>", "Maintenance Level: ", "</b>", INFRA$ML,
                                                    "<br>", "<b>", "Surface Type: ", "</b>", INFRA$SURFACE_TY,
                                                    "<br>", "<b>", "Jurisdiction: ", "</b>", INFRA$JURISDICTI))

}


#' Visualize District Geology
#' @param data Viz_Area()
#' @return
#' @export
#'

viz_Geo <- function(data) {

  rockPal <- leaflet::colorFactor("Paired", geo$ROCKTYPE1)
  data %>% leaflet::addPolygons(data = geo, group = "geo",
                                      color = ~rockPal(ROCKTYPE1),
                                      popup = paste0("<b>","Rocktype 1: ","</b>", geo$ROCKTYPE1,
                                                     "<br>","<b>", "Rocktype 2: ", "</b>", geo$ROCKTYPE2,
                                                     "<br>", "<b>", "Unit Age ", "</b>",geo$UNIT_AGE,
                                                     "<br>", "<b>", "Unit Label: ", "</b>",geo$ORIG_LABEL,
                                "<br>", '<a href = "https://pubs.usgs.gov/imap/i2267/"> More Info </a>'))

}



#' Visualize District Geology 2
#' @param data Viz_Area()
#' @return
#' @export
#'

viz_GeoII <- function(data) {

  rockPalII <- leaflet::colorFactor("Paired", geoII$DESC_)
  data %>% leaflet::addPolygons(data = geoII, group = "geoII",
                                color = ~rockPalII(DESC_),
                                popup = paste0("<b>","Description: ","</b>", geoII$DESC_,
                                               "<br>", "<b>", "Unit Label: ", "</b>",geoII$LABEL,
                                "<br>", '<a href = "https://pubs.usgs.gov/imap/i2267/"> More Info </a>'))

}



#' Visualize District Landtype
#' @param data Viz_Area()
#' @return
#' @export
#'

viz_Soils <- function(data) {

  soilsPal <- leaflet::colorFactor("RdBu", soils$cut_L, reverse = TRUE)
  data %>% leaflet::addPolygons(data = geoII, group = "soils",
                                color = ~soilsPal(soils$cut_L),
                                popup = paste0("<b>","Soil Level: ","</b>", soils$cut_L,
                                               "<br>", "<b>", "Soil Type: ", "</b>",soils$LANDTY))

}

#' Visualize District Landtype
#' @param data Viz_Area()
#' @return
#' @export
#'

viz_Faults <- function(data) {

  faultsPal <- leaflet::colorFactor("Paired", faults$DESC_)
  data %>% leaflet::addPolylines(data = faults, group = "faults",
                                color = ~faultsPal(DESC_),
                                popup = paste0("<b>","Description: ","</b>", faults$DESC_,
                                               "<br>", "<b>", "Type of Structure: ", "</b>",faults$TYPE,
                                               "<br>", "<b>", "Type of Fold: ", "</b>",faults$FOLD,
                                               "<br>", "<b>", "Type of Vertical: ", "</b>",faults$VERTICAL,
                                               "<br>", "<b>", "Type of Plunge: ", "</b>",faults$PLUNGE,
                                               "<br>", "<b>", "Name: ", "</b>",faults$NAME,
                                               "<br>", "<b>", "Type: ", "</b>",faults$TYPE,
                                               "<br>", "<b>", "Accuracy: ", "</b>",faults$ACCURACY,
                                "<br>", '<a href = "https://pubs.usgs.gov/imap/i2267/"> More Info </a>'))

}


#' Visualize District Harvest
#' @param data Viz_Area()
#' @return
#' @export
#'

viz_Harvest <- function(data) {

  harvestPal <- leaflet::colorFactor("Paired", past_harv$ACTIVITY)
  data %>% leaflet::addPolygons(data = past_harv, group = "harvest",
                                color = ~harvestPal(ACTIVITY),
                                popup = paste0("<b>","Activity Type: ","</b>", past_harv$ACTIVITY,
                                               "<br>", "<b>", "Activity Code #: ", "</b>",past_harv$ACTIVITY_C,
                                               "<br>", "<b>", "Date Planned: ", "</b>",past_harv$DATE_PLANN,
                                               "<br>", "<b>", "Date Accomplished: ", "</b>",past_harv$DATE_ACCOM,
                                               "<br>", "<b>", "Date Completed: ", "</b>",past_harv$DATE_COMPL,
                                               "<br>", "<b>", "Elevation: ", "</b>",past_harv$ELEVATION,
                                               "<br>", "<b>", "Aspect: ", "</b>",past_harv$ASPECT,
                                               "<br>", "<b>", "Ownership: ", "</b>",past_harv$OWNERSHIP_,
                                               "<br>", '<a href = "https://data.fs.usda.gov/geodata/edw/edw_resources/meta/S_USA.Activity_TimberHarvest.xml"> More Info </a>'
                                               ))

}





#' add Layers
#' @param data previous viz_vectors*
#' @return
#' @export
#'

add_Layers <- function(data){
  bgrps <- c("USGS Topo", "USGS Imagery Only", "USGS Imagery Topo",
             "USGS Shaded Relief")
  ovgrp <- grab_names(data)

data %>%
  leaflet::addLayersControl(baseGroups = bgrps,
                   overlayGroups = c(ovgrp, "Hydrography"))
}
