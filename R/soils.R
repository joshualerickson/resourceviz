


#' Get soiDB sf
#'
#' @param sf
#'
#' @return an sf object
#' @export
#'
get_soilDB_sf <- function(sf) {

ssurgo.geom <- soilDB::SDA_spatialQuery(
  sf::as_Spatial(sf::st_as_sfc(sf::st_bbox(sf))),
  what = 'geom',
  db = 'SSURGO',
  geomIntersection = TRUE
)

surgo_sf <- ssurgo.geom %>% sf::st_as_sf()

# convert results into an SQL "IN" statement
# useful when there are multiple intersecting records
mu.is <- soilDB::format_SQL_in_statement(surgo_sf$mukey)

# composite SQL WHERE clause
sql <- sprintf("mukey IN %s", mu.is)

# get commonly used map unit / component / chorizon records
# as a SoilProfileCollection object
# confusing but essential: request that results contain `mukey`
# with `duplicates = TRUE`
x <- soilDB::fetchSDA(sql, duplicates = TRUE)

surgo_sf <- surgo_sf %>%
  dplyr::left_join(x@site, by = 'mukey')

}


#' Get HSG
#' @description Gets hydrologic soil group per composition per polygon
#' but also a lot of other ssurgo information.
#'
#' @param sf
#'
#' @return
#' @export
#'
get_HSG <- function(sf) {

  location <- get_soilDB_sf(sf)

  location <- location %>% dplyr::select(gid,mukey, cokey,compname, comppct_r, hydgrp)
}


#' Get Max Composition
#'
#' @param soildf a previously created get_HSG() or get_soilDB_sf() object.
#' @param sf a sf object that intersects soildf
#'
#' @return
#' @export
#'
get_compMax_intersection <- function(soildf, sf) {

   if(is.null(sf$group)){
    sf <- sf %>%
      dplyr::mutate(group = dplyr::row_number())
   }

  soil_catch <- soildf %>%
    sf::st_intersection(sf)


  soil_catch_2 <- soil_catch %>%
    dplyr::group_by(group, gid) %>%
    dplyr::slice_max(comppct_r) %>%
    dplyr::ungroup()
}


#' Get Soil Proportion
#'
#' @param soil_catch a previously created get_compMax_intersection() object
#'
#' @return
#' @export
#'
get_soil_proportion <- function(soil_catch) {

  soil_catch %>%
  dplyr::mutate(area = units::set_units(sf::st_area(.), 'acres'),
         area_ac = as.numeric(area)) %>%
  dplyr::group_by(group) %>%
    dplyr::mutate(area_sum = sum(area_ac)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(group, hydgrp) %>%
    dplyr::mutate(hsg_proportion = area_ac/area_sum) %>%
    dplyr::summarise(sum = sum(hsg_proportion)) %>%
  dplyr::ungroup()
}
