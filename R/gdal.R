

#' Get OGR Clip
#' @description Get OGR (OpenGIS Simple Features Reference Implementation)
#' Simple Features Library to clip simple features with GDAL. Provide an sf object and clip large
#' simple feature (infile). The
#' clip will be based on the bounding box of the sf object.
#' @param sf An sf object.
#' @param infile A file path to the simple feature to clip.
#' @param outfile A file path to write the clip to.
#' @param layers A character vector. In case you are using a GPKG or gdb you can add the layers here.
#'
#' @return
#' @export
#' @note Sometimes you will need to double quote your path if there are spaces or unique
#' characters that CLI needs quoted, e.g. '"C:/josh erickson/some file path/"'.
#'
#' @examples
gdal_clip_ogrsf <- function(sf, infile, outfile, layers){

  bb <- sf::st_bbox(sf)

  if(missing(layers)){

    system(paste('ogr2ogr -f GPKG -spat ', paste(bb[[1]], bb[[2]], bb[[3]], bb[[4]], outfile, infile)))

  } else {

    system(paste('ogr2ogr -f GPKG -spat ', paste(bb[[1]], bb[[2]], bb[[3]], bb[[4]], outfile, infile, paste(layers, sep = ' ', collapse = ' '))))

    }
}

#' Get raster clip
#'
#' @param sf An sf object.
#' @param infile A file path to the simple feature to clip.
#' @param outfile A file path to write the clip to.
#' @note Sometimes you will need to double quote your path if there are spaces or unique
#' characters that CLI needs quoted, e.g. '"C:/josh erickson/some file path/"'.
#' @return
#' @export
#'
#' @examples
gdal_clip_rast <- function(sf, infile, outfile){

  bb <- sf::st_bbox(sf)

  system(paste('gdalwarp -te ', paste(bb[[1]], bb[[2]], bb[[3]], bb[[4]], infile, outfile)))

}
