#' Title
#'
#' @return
#' @export
#'
#' @examples
aoi_draw <- function() {


  shiny::shinyApp(

  ui = shiny::fluidPage(leaflet::leafletOutput('aoi'),shiny::textInput('export_filename', label = 'Filename'),
                          shiny::downloadButton("downloadData", label = "Download")
    ),

    server = function(input, output) {


  output$aoi <- leaflet::renderLeaflet({

    viz_Area() %>% leaflet::setView(lat = 48.91167, lng = -114.90246, zoom = 4) %>%
      leafem::addMouseCoordinates(epsg = "EPSG:4326", proj4string = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") %>%
      leaflet.extras::addDrawToolbar(polylineOptions = F, circleOptions = T, markerOptions = T,
                                     circleMarkerOptions = F, polygonOptions = T)


  })


  shiny::observeEvent(input$aoi_draw_new_feature, {



    feat <- input$aoi_draw_new_feature
    coords <- unlist(feat$geometry$coordinates)
    coords <- matrix(coords, ncol = 2, byrow = T)


    poly <- sf::st_sf(sf::st_sfc(sf::st_polygon(list(coords))), crs = sf::st_crs(4326)) %>% sf::st_as_sf()

    maps <- shiny::reactive(poly)

    leaflet::leafletProxy('aoi') %>% leafem::addMouseCoordinates(epsg = "EPSG:4326", proj4string = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")


      map_update <- shiny::reactive({ viz_Area() %>% leafem::addMouseCoordinates(epsg = "EPSG:4326", proj4string = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") %>%
          leaflet::addPolygons(data = maps() )})




    output$aoi <- leaflet::renderLeaflet({

      map_update() %>%
        leaflet.extras::addDrawToolbar(polylineOptions = F, circleOptions = T, markerOptions = T,
                       circleMarkerOptions = T, polygonOptions = T)
    })
    output$downloadData <- shiny::downloadHandler(
      filename = function() {

          paste0(input$export_filename, ".zip")
      },
      content = function(file) {
        tmp.path <- dirname(file)
        name.base <- file.path(tmp.path, input$export_filename)
        name.glob <- paste0(name.base, ".*")
        name.shp  <- paste0(name.base, ".shp")
        name.zip  <- paste0(name.base, ".zip")
          sf::st_write(maps(), dsn = name.shp, layer = "shpExport",
                   driver = "ESRI Shapefile", quiet = TRUE, delete_dsn = T)
        zip::zipr(zipfile = name.zip, files = Sys.glob(name.glob))
        shiny::req(file.copy(name.zip, file))

})




      }
    )

    }

) #end shinyApp


}

#' Title
#'
#' @param filename A \code{characater}. Input file name, e.g. 'aoi_draw'.
#' @param inDir A \code{characater} indicating where .zip file is, e.g. 'D:/downloads'.
#' @param outDir A \code{characater} indicating where shape file should go, e.g. 'D:/downloads'.
#'
#' @return
#' @export
#'

aoi_unzip <- function(filename, inDir, outDir = tempfile(), pt = F) {

  zipF<- file.path(inDir, paste0(filename,".zip"))
  zip::unzip(zipfile = zipF,exdir = file.path(outDir))
  if(isTRUE(pt)){
  sf::read_sf(paste0(outDir,'/',filename,".shp")) %>% sf::st_cast("POINT")
  } else {
    sf::read_sf(paste0(outDir,'/',filename,".shp"))
  }
}
