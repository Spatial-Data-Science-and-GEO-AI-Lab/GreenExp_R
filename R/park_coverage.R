
#' Park Coverage
#'
#' @param address_location A spatial object representing the location of interest, the location should be in projected coordinates.
#' @param park_layer A park layer, the layer should be in projected coordinates
#' @param buffer_distance  A distance in meters to create a buffer or isochrone around the address location
#' @param net an optional \code{sfnetworks} object representing a road network
#' @param UID A character string representing a unique identifier for each point of interest
#'
#' @return Returns the percentage of park coverage given a certain buffer.
#' @export
#'
#' @examples
park_pct <- function(address_location, park_layer, buffer_distance, net, UID) {
  ### Make sure main data set has projected CRS and save it


  if (sf::st_is_longlat(address_location)){
    warning("The CRS in your main data set has geographic coordinates, the Projected CRS will be set to WGS 84 / World Mercator")
    st_crs(address_location) <- 3395
  }
  projected_crs <- sf::st_crs(address_location)

  ### Creating net if not given
  if (missing(net)){
    ### Address into point
    if ("POINT" %in% sf::st_geometry_type(address_location)) {
      #Do nothing
    }else if (missing(buffer_distance)) {
      stop("You do not have a point geometry and did not provide a buffer, please provide a point geometry or a buffer distance")
    }
    else {
      message('There are nonpoint geometries, they will be converted into centroids')
      address_location <- sf::st_centroid(address_location)}


    iso_area <- sf::st_buffer(sf::st_convex_hull(
      sf::st_union(sf::st_geometry(address_location))),
      buffer_distance)
    iso_area <- sf::st_transform(iso_area, crs = 4326)
    bbox <- sf::st_bbox(iso_area)
    q <- osmdata::opq(bbox) %>%
      osmdata::add_osm_feature(key = "highway") %>%
      osmdata::osmdata_sf()

    # extract lines and polygons from the OSM data
    lines <- q$osm_lines
    polys <- q$osm_polygons


    #Remove Factors
    lines$osm_id <- as.numeric(as.character(lines$osm_id))
    polys$osm_id <- as.numeric(as.character(polys$osm_id))

    #remove the invalid polygons
    polys <- polys[!is.na(polys$highway),]
    polys <- polys[is.na(polys$area),]

    #Change Polygons to Lines
    polys <- sf::st_cast (polys, "LINESTRING")
    lines <- sf::st_cast (lines, "LINESTRING")

    # remove invalid geometry
    #lines <- lines[st_is_valid(lines) %in% TRUE,] # %in% TRUE handles NA that occure with empty geometries
    polys <- polys[sf::st_is_valid(polys) %in% TRUE,]
    #Bind together
    lines <- rbind(lines,polys)


    #Change to user projection
    lines <- sf::st_transform(lines, projected_crs)
    lines <- tidygraph::select(lines, "osm_id", "name")
    region_shp <- sf::st_transform(iso_area, projected_crs)

    #Download osm used a square bounding box, now trim to the exact boundary
    #note that lines that that cross the boundary are still included
    lines <- lines[region_shp,]

    # Round coordinates to 0 digits.
    sf::st_geometry(lines) <- sf::st_geometry(lines) %>%
      lapply(function(x) round(x, 0)) %>%
      sf::st_sfc(crs = sf::st_crs(lines))



    # Network
    net <- sfnetworks::as_sfnetwork(lines, directed = FALSE)
    net <- tidygraph::convert(net, sfnetworks::to_spatial_subdivision)



    #convert network to an sf object of edge
    net_sf <- net %>% tidygraph::activate("edges") %>%
      sf::st_as_sf()
    # Find which edges are touching each other
    touching_list <- sf::st_touches(net_sf)
    # create a graph from the touching list
    graph_list <- igraph::graph.adjlist(touching_list)
    # Identify the cpnnected components of the graph
    roads_group <- igraph::components(graph_list)
    # cont the number of edges in each component
    roads_table <- table(roads_group$membership)
    #order the components by size, largest to smallest
    roads_table_order <- roads_table[order(roads_table, decreasing = TRUE)]
    # get the name of the largest component
    biggest_group <- names(roads_table_order[1])

    # Subset the edges corresponding to the biggest connected component
    osm_connected_edges <- net_sf[roads_group$membership == biggest_group, ]
    # Filter nodes that are not connected to the biggest connected component
    net <- net %>%
      tidygraph::activate("nodes") %>%
      sf::st_filter(osm_connected_edges, .pred = sf::st_intersects)
  }
  else  {
    # If the CRS of the network data set is geographic, tranfrom it to the project CRS
    if (sf::st_crs(address_location) != sf::st_crs(net))
    {
      print("The CRS of your network data set is geographic, CRS of main data set will be used to transform")
      net <- sf::st_transform(net, projected_crs)
    }
  }




  ### Adding edge length for future calculations
  net <- tidygraph::mutate(tidygraph::activate(net, "edges"),
                           weight = sfnetworks::edge_length())


  ### Creating park_layer set if not given
  if (missing(park_layer)) {
    ### Building area polygon and loading park_layer
    # Area assignment
    calculation_area <- sf::st_buffer(address_location, dist = buffer_distance)

    # Initial load of park_layer
    q1 <- osmdata::opq(sf::st_bbox(iso_area)) %>%
      osmdata::add_osm_feature(key = "landuse",
                               value = c('allotments','forest',
                                         'greenfield','village_green')) %>%
      osmdata::osmdata_sf()

    q2 <- osmdata::opq(sf::st_bbox(iso_area)) %>%
      osmdata::add_osm_feature(key = "leisure",
                               value = c('garden','fitness_station',
                                         'nature_reserve','park','playground')) %>%
      osmdata::osmdata_sf()

    q3 <- osmdata::opq(sf::st_bbox(iso_area)) %>%
      osmdata::add_osm_feature(key = "natural",
                               value = c('grassland')) %>%
      osmdata::osmdata_sf()
    res <- c(q1, q2, q3)

    # park_layer cleaning
    park_layer <- res$osm_polygons
    park_layer <- tidygraph::select(park_layer, "osm_id", "name")
    park_layer <- sf::st_make_valid(park_layer)
    park_layer <- sf::st_transform(park_layer, projected_crs)
  } else {
    if (sf::st_crs(address_location) != sf::st_crs(park_layer))
    {
      print("The CRS of your park_layer data set is geographic, CRS of main data set will be used to transform")
      park_layer <- sf::st_transform(park_layer, original_pr)
    }}



  # Make sure the park layer are polygons and no points
  if ("MULTIPOLYGON" %in% sf::st_geometry_type(park_layer) | "POLYGON" %in% sf::st_geometry_type(park_layer)){
    # Do nothing
  } else {
    stop("The park layer provided has the wrong geometry type, please input a (multi)polygon geometry   ")
  }

  park_pct <- list()

  # if (nrow(calculation_area) > 1){
  n_iter <- nrow(calculation_area)



  pb <- progress::progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                         total = n_iter,
                         complete = "=",   # Completion bar character
                         incomplete = "-", # Incomplete bar character
                         current = ">",    # Current bar character
                         clear = FALSE,    # If TRUE, clears the bar when finish
                         width = 100)      # Width of the progress bar


  for (i in 1:n_iter) {
    pb$tick()

    # Clip tree park to polygon
    park_clip <- sf::st_intersection(park_layer, calculation_area[i,])
    # Calculate area of clipped tree park
    park_area <- sf::st_area(park_clip)
    total_area <- sum(park_area)
    # Calculate area of polygon
    polygon_area <- sf::st_area(calculation_area[i,])
    # Calculate tree park percentage
    park_pct[i] <- total_area / polygon_area * 100


  }


  #pb$close()
  buffer <- calculation_area
  names(buffer) <- "buffer"
  df <- data.frame(UID = nrow(calculation_area), park_pct = cbind(unlist(park_pct)),
                   sf::st_geometry(address_location))
  df$UID <- seq.int(nrow(df))
  if (!missing(UID)){
    df$UID <- UID}

  df <- sf::st_as_sf(df)

  return(df)

}
