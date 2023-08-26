
#' Draw bounding box for the area of interest and get it in local environment
#'
#' @return once the user draw a rectangle, it return as bbox in the local environment
#' @export
#'
#' @examples
#'
#' @importFrom leaflet leaflet
#' @importFrom leaflet setView
#' @importFrom leaflet.extras addSearchOSM
#' @importFrom leaflet.extras addDrawToolbar
#' @importFrom sf st_polygon
#' @importFrom sf st_sf
#'
#'
#'
#'
bboxapp <- function () {

  #set up the map
  ui <- shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
      #map {
        position: absolute;
        top: 0;
        bottom: 0;
        width: 100%;
      }
    "))
    ),
    leaflet::leafletOutput("map", width = "100%", height = "100vh")
  )

  #local server set up
  server <- function(input, output, session) {
    output$map <- leaflet::renderLeaflet({
      m <- leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet.extras::addSearchOSM(options = leaflet.extras::searchOptions(collapsed = T))%>%
        leaflet.extras::addDrawToolbar(polylineOptions = F, circleOptions = F, markerOptions = F,
                       circleMarkerOptions = F, polygonOptions = F) %>%
        leaflet::setView(lng = -0.000687, lat = 51.477110, zoom = 2)
    })

    #to draw the box and get on local environment
    observeEvent(input$map_draw_new_feature, {
      feat <- input$map_draw_new_feature
      coords <- unlist(feat$geometry$coordinates)
      coords <- matrix(coords, ncol = 2, byrow = T)
      poly <- sf::st_sf(sf::st_sfc(sf::st_polygon(list(coords))), crs = 4326)
      bbox <<- sf::st_bbox(poly) #this will be saved on the local environment which can work as bbox for other functions
      print(sf::st_bbox(poly))
    })
  }

  shiny::shinyApp(ui, server)


}


