
#' Title
#'
#' @param sf_pt A sf point object
#' @param leaflet TRUE/FALSE for plotting with leaflet
#' @param maptype A \code{character} indicating maptype. see \link[ggmap]{get_map} documentation
#' @param zoom A \code{numeric} value between 1-13, zoom level for ggmap.
#' @param source A \code{character} indicating what maptype source.
#' @importFrom sf st_length st_area
#' @import ggplot2
#' @importFrom magrittr '%>%'
#'
#' @return A leaflet map with NLDI basin boundary, UT, UM and point.
#' @export
#'
#' @examples
viz_NLDI <- function(sf_pt, leaflet = TRUE, maptype = "terrain", source = "google", zoom = 8) {




  if(is.atomic(sf_pt)) {
    point <- sf_pt
    clat <- point[[1]]
    clng <- point[[2]]
    point <- data.frame(clat = clat, clng = clng)
    point <- sf::st_as_sf(point, coords = c("clat", "clng")) %>% sf::st_set_crs(4269)

  } else {

    if(!class(sf::st_geometry(sf_pt)[[1]])[[2]] == "POINT")stop({"Need a sf_POINT object"})

    point <- sf_pt
    clat <- point$geometry[[1]][[2]]
    clng <- point$geometry[[1]][[1]]

  }

  ids <- paste0("https://labs.waterdata.usgs.gov/api/nldi/linked-data/comid/position?coords=POINT%28",
                clng,"%20", clat, "%29")

  error_ids <- httr::GET(url = ids,
                         httr::write_disk(path = file.path(tempdir(),
                                                           "nld_tmp.json"),overwrite = TRUE))

  nld <- jsonlite::fromJSON(file.path(tempdir(),"nld_tmp.json"))


  nldiURLs <- list(site_data = paste0("https://labs.waterdata.usgs.gov/api/nldi/linked-data/comid/position?coords=POINT%28",
                                      clng,"%20", clat, "%29"),
                   basin_boundary = paste0("https://labs.waterdata.usgs.gov/api/nldi/linked-data/comid/",nld$features$properties$identifier,"/basin"),
                   UT = paste0("https://labs.waterdata.usgs.gov/api/nldi/linked-data/comid/",nld$features$properties$identifier,"/navigation/UT/flowlines?distance=999"),
                   UM = paste0("https://labs.waterdata.usgs.gov/api/nldi/linked-data/comid/",nld$features$properties$identifier,"/navigation/UM/flowlines?distance=999"))

  nldi_data <- list()


  for(n in names(nldiURLs)) {
    nldi_data[n] <- list(sf::read_sf(nldiURLs[n][[1]]))
    print(paste(n, "is of class", class(nldi_data[[n]]), "and has", nrow(nldi_data[[n]]), "features"))
  }


  base_url <- paste0("https://labs.waterdata.usgs.gov/api/nldi/linked-data/comid/",nld$features$properties$identifier,"/tot")

  error <- httr::GET(url = base_url,
                     httr::write_disk(path = file.path(tempdir(),
                                                       "catch_tmp.json"),overwrite = TRUE))

  catch_tmp <- jsonlite::fromJSON(file.path(tempdir(),"catch_tmp.json"))
  catch_tmp <- catch_tmp$characteristics %>% dplyr::as_tibble()
  catch_tmp <- dplyr::left_join(catch_tmp, chars, by = "characteristic_id") %>%
    dplyr::select(Description = "characteristic_description", Value = "characteristic_value",Units = "units",  Theme = "theme_label") %>% dplyr::arrange(Theme)



  if(leaflet == "TRUE") {
    #write_rds(catch_tmp, file.path(tempdir(), "catch_tmp"))
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

    map_nldi <- leaflet::addPolygons(map,
                                     data=nldi_data$basin_boundary,
                                     color = "black",
                                     fill = FALSE,
                                     weight = 2,
                                     opacity = 1)



    map_nldi <- leaflet::addPolylines(map_nldi,
                                      data = nldi_data$UT,
                                      color = "blue",
                                      weight = 1,
                                      opacity = 1)

    map_nldi <- leaflet::addPolylines(map_nldi,
                                      data = nldi_data$UM,
                                      color = "red",
                                      weight = 3,
                                      opacity = 0.5,
                                      popup = nldi_data$UM)

    map_nldi <- leaflet::addCircleMarkers(map_nldi, lng=clng, lat=clat,
                                          radius = 5,
                                          color = "red",
                                          popup = paste0("<b>","Drainage Area (DA): ","</b>", round(units::set_units(sf::st_area(nldi_data$basin_boundary), mi^2), 1), " Sq.Miles",
                                                         "<br>","<b>", "DA acres: ", "</b>", scales::comma(as.numeric(round(units::set_units(sf::st_area(nldi_data$basin_boundary), acres), 1)),1), " Acres",
                                                         "<br>", "<b>", "Length of Main Stem: ", "</b>", round(sum(units::set_units(sf::st_length(nldi_data$UM), mi)), 1), " Miles",
                                                         "<br>", "<b>", "Total length of Tribs: ", "</b>", round(sum(units::set_units(sf::st_length(nldi_data$UT), mi)), 1), " Miles",
                                                         "<br>", "<b>", "DA/Length: ", "</b>", round((sum(units::set_units(sf::st_length(nldi_data$UM), mi))+sum(units::set_units(sf::st_length(nldi_data$UT), mi)))/(units::set_units(sf::st_area(nldi_data$basin_boundary), mi^2)),1), " Miles",
                                                         "<br>", "<b>", "Identifier: ", "</b>",nldi_data$site_data$identifier))

    map_nldi


  } else if (leaflet == "FALSE") {

    bbox <- stats::setNames(sf::st_bbox(sf::st_buffer(nldi_data$basin_boundary, .05)), c("left", "bottom", "right", "top"))

    basemap_satellite <- ggmap::get_map(maptype = maptype, source = {{ source }}, location = bbox, zoom = zoom)
    satellite <- ggmap::ggmap(basemap_satellite)

    satellite +
      geom_sf(data = nldi_data$basin_boundary, fill = NA, inherit.aes = FALSE, color = "black", size = 2) +
      geom_sf(data = nldi_data$UT, col = "blue", inherit.aes = FALSE) +
      geom_sf(data = nldi_data$UM, col = "red", inherit.aes = FALSE) +
      geom_sf(data = point, col = "black", size = 3, inherit.aes = FALSE) +
      theme_void() +
      labs(title = paste0(nldi_data$site_data$sourceName, " : ","'", nldi_data$site_data$identifier, "'"))


  }

}



#' Visualise NLDI catchments
#'
#' @param comidID A nhdplus comid.
#'
#' @return A leaflet map with select catchment variables.
#' @export
#' @importFrom magrittr '%>%'
#'
#' @examples
viz_NLDIcatch <- function(comidID) {

  nldi_feature <- list(featureSource = "comid",
                       featureID = comidID)

  outlet_comid <- nhdplusTools::discover_nhdplus_id(nldi_feature = nldi_feature)

  nhd_catch <- nhdplusTools::plot_nhdplus(nldi_feature, flowline_only = FALSE, actually_plot = FALSE)


  local_characteristic <-  data.frame(COMID = nhd_catch$flowline$COMID) %>%
    dplyr::mutate(ID = dplyr::row_number()) %>%
    dplyr::group_by(ID) %>%
    tidyr::nest() %>%
    dplyr::mutate(chars = map(data, ~nhdplusTools::get_nldi_characteristics(list(featureSource = "comid", featureID = as.character(.$COMID)),
                                                                  type = "local"))) %>%
    tidyr::unnest() %>%
    tidyr::unnest() %>%
    dplyr::ungroup() %>%
    dplyr::filter(stringr::str_detect(characteristic_id, "CAT_RECHG|CAT_ET|CAT_PET|CAT_PPT7100_ANN|CAT_TWI|CAT_BFI")) %>%
    dplyr::select(COMID, characteristic_id, characteristic_value) %>%
    tidyr::pivot_wider(names_from = "characteristic_id", values_from = "characteristic_value") %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("CAT"), as.numeric),
           CAT_PPT7100_ANN = CAT_PPT7100_ANN*0.0393701,
           Deficit = CAT_PET-CAT_ET)
  cat <- dplyr::right_join(nhd_catch$catchment, local_characteristic, by = c("FEATUREID" = "COMID")) %>%
    sf::st_as_sf() %>%
    sf::st_transform(crs = 4326,proj4string = "+init=epsg:4326")



    map <- viz_Area() %>% leaflet::addPolygons(data = cat, fillOpacity = 0.6, weight = 1.5, popup = paste0(
      "<p style=line-height:30px;margin:0px;>","<b>Station: </b>",cat$FEATUREID,
      "<br>", "<b>Polygon Area: </b>", paste(scales::comma(as.numeric(round(units::set_units(sf::st_area(cat), mi^2),0))), " sq.mi"),
      "<br>","<b>Base Flow Index (BFI): </b>",paste(round(cat$CAT_BFI,1), " %"),
      "<br>","<b>Recharge: </b>",paste(round(cat$CAT_RECHG,1), " mm/year"),
      "<br>","<b>Actual ET: </b>",paste(round(cat$CAT_ET,1), " mm/year"),
      "<br>","<b>Potential ET: </b>",paste(round(cat$CAT_PET,1), " mm/year"),
      "<br>", "<b>Water Deficit: </b>", paste(round(cat$CAT_PET-cat$CAT_ET,1), " mm/year"),
      "<br>","<b>Precip (PRISM 30 year mean): </b>",paste(round(cat$CAT_PPT7100_ANN,1), " in."),
      "<br>","<b>Topographic Wetness Index (TWI): </b>",round(cat$CAT_TWI,1)))

    opt <- leaflet::layersControlOptions(collapsed = TRUE)

    map %>%
      leafem::addMouseCoordinates(epsg = "EPSG:4326", proj4string = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")





}


#' Plot GridMET
#'
#' @description A wrapper function around \link[climateR]{getGridMET} that plot's time series for
#' selected parameters.
#' @param data A sf point object
#' @param row \code{numeric}. If a specific place is wanted then enter row.
#' @param param \code{character}. See \link[climateR]{getGridMET} for more info.
#' @param startDate \code{character}. "2020-01-01"
#' @param endDate \code{character}. "2020-01-01"
#'
#' @return A ggplot.
#' @export
#'
#' @examples
viz_GridMET <- function(data, row, param = "prcp", startDate = "2019-01-01", endDate = "2020-01-01") {


  if(!missing(row)){
    data <- data %>% slice({{ row }})
    ts  <- getGridMET(data, param = param, startDate = startDate, endDate = endDate)

    ggplot(data = ts, aes_string(x = "date", y = param)) +
      geom_line() +
      stat_smooth(col = "red") +
      theme_bw() +
      labs(title = paste0("GridMET Daily: ", param, " at ", data$station_nm), x = "Date", y = paste0(param))

  } else {

    ts <- data.frame()
    for (i in 1:nrow(data)) {

      time  <- getGridMET(data[i,], param = param, startDate = startDate, endDate = endDate)
      time <- time %>% mutate(group = data[i,]$station_nm)

      ts <- plyr::rbind.fill(ts, time)
    }

    ggplot(data = ts, aes_string(x = "date", y = param)) +
      geom_line() +
      stat_smooth(col = "red") +
      theme_bw() +
      facet_wrap(~group, scales = "free")

  }

}

