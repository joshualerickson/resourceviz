

#' Get Shape Clip
#' @description Get vector clip with GDAL. Provide sf object and clip large vector.
#' @param sf
#' @param infile
#' @param outfile
#' @param layers
#'
#' @return
#' @export
#'
#' @examples
gdal_clip_shape <- function(sf, infile, outfile, layers){

  bb <- sf::st_bbox(sf)
  if(missing(layers)){
    system(paste('ogr2ogr -f GPKG -spat ', paste(bb[[1]], bb[[2]], bb[[3]], bb[[4]], outfile, infile)))

  } else {
  system(paste('ogr2ogr -f GPKG -spat ', paste(bb[[1]], bb[[2]], bb[[3]], bb[[4]], outfile, infile, paste(layers, sep = ' ', collapse = ' '))))
  }
}

#' Get raster clip
#'
#' @param sf
#' @param infile
#' @param outfile
#' @param layers
#'
#' @return
#' @export
#'
#' @examples
gdal_clip_rast <- function(sf, infile, outfile, layers){

  bb <- sf::st_bbox(sf)

  system(paste('gdalwarp -te ', paste(bb[[1]], bb[[2]], bb[[3]], bb[[4]], infile, outfile)))

}
