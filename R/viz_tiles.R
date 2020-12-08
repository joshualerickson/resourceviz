
#' Visualize Tiles
#'
#' @param data
#'
#' @return
#' @export
#'

viz_Tiles <- function(data, group = "deficit"){


data  %>% leafem::addTileFolder(folder = paste0("D:/R_folder/Rasters/Tiles/tiling/",group))

}
