#' Distance to the nearest fake entries of parks
#'
#' @param address_location A spatial object representing the location of interest, the location should be in projected coordinates.
#' @param buffer_distance A distance in meters to create a buffer or isochrone around the address location
#' @param parks A spatial object representing parks
#' @param UID A character string representing a unique identifier for each point of interest
#' @param network_file An optional sfnetwork object representing a road network, If missing the road network will be created.
#' @param speed A numeric value representing the speed in km/h to calculate the buffer distance (required if `time` is provided)
#' @param time A numeric value representing the travel time in minutes to calculate the buffer distance (required if `speed` is provided)
#' @param city When using a network buffer, you can add a city where your address points are to speed up the process
#' @param download_dir A directory to download the network file, the default will be `tempdir()`.
#' @param epsg_code A espg code to get a Projected CRS in the final output, If missing, the default is `3395`
#' @param euclidean Whether the distance between the fake entry points and the address location is euclidean, or calculated with the network.
#' @param pseudo_entrance The possibility to calculate the distance to created pseudo entrances. These pseudo entrances are created,
#' by intersecting a buffer that was created around a park with network points. default is `FALSE`
#' @param entrances_within_buffer The possibility to get the closest park for every point. default is `FALSE`
#'
#' @return The distance to the nearest fake entries of parks and whether the parks are within the buffer distance that is given
#' @export
#'
#' @examples
parks_access <- function(address_location, parks = NULL, buffer_distance = NULL, network_file=NULL,
                           epsg_code=NULL, download_dir = tempdir(), euclidean=TRUE,
                           pseudo_entrance=FALSE, entrances_within_buffer=TRUE,
                           speed=NULL, time=NULL, city=NULL, UID=NULL) {
  # timer for function
  start_function <- Sys.time()

  ### Make sure main data set has projected CRS and save it
  # Make sure main data set has projected CRS and save it
  if (sf::st_is_longlat(address_location)){
    warning("The CRS in your main data set has geographic coordinates, the Projected CRS will be set to WGS 84 / World Mercator")
    if (missing(epsg_code)) {
      projected_crs <- sf::st_crs(3395)
      address_location <- sf::st_transform(address_location, projected_crs)
    }
    else{
      projected_crs<-sf::st_crs(epsg_code)
      address_location <- sf::st_transform(address_location, projected_crs)
    }
  } else{
    # If address location is given as projected, save the crs
    projected_crs <- sf::st_crs(address_location)
  }


  if (missing(network_file)){
    message('You did not provide a network file, a network will be created using osm.')
    if (missing(buffer_distance)){
      if(missing(speed)||missing(time)){
        stop("You didn't enter speed and time or the buffer distance,
             please enter speed and time, or the buffer distance.")
      } else if (!speed > 0) {
        stop("Speed must be a positive integer")
      } else if (!time > 0) {
        stop("Time must be a positive integer")
      } else{
        buffer_distance <- speed * 1000/ 60 * time
      }
    }
    if ("POINT" %in% sf::st_geometry_type(address_location)) {
    }else if (missing(buffer_distance)) {
      stop("You do not have a point geometry and did not provide a buffer,
      or speed and time,
           please provide a point geometry or a buffer distance (or spede and time)")
    }
    else {
      message('There are nonpoint geometries, they will be converted into centroids')
      address_location <- sf::st_centroid(address_location)
    }
    iso_area <- sf::st_buffer(sf::st_convex_hull(
      sf::st_union(sf::st_geometry(address_location))),
      buffer_distance)
    iso_area <- sf::st_transform(iso_area, crs = 4326)
    # bbox might be redundant
    bbox <- sf::st_bbox(iso_area)
    # Use the osmextract package to extract the lines in the area.
    if (pseudo_entrance || !euclidean){
      if (!missing(city)) {

        lines <- osmextract::oe_get(city, stringsAsFactors=FALSE, boundary=iso_area,
                                    download_directory=download_dir,
                                    max_file_size = 5e+09, boundary_type = 'spat')


      } else{
        message('If a city is missing, it will take more time to run the function')

        lines <- osmextract::oe_get(iso_area, stringsAsFactors=FALSE, boundary=iso_area,
                                    download_directory=download_dir,
                                    max_file_size = 5e+09, boundary_type = 'spat')
      }
    }
  }

  else  {
    # If the CRS of the network data set is geographic, tranfrom it to the project CRS
    if (sf::st_crs(address_location) != sf::st_crs(network_file))
    {
      print("The CRS of your network data set is geographic, CRS of main data set will be used to transform")
      network_file <- sf::st_transform(network_file, projected_crs)
    }
  }
  if (pseudo_entrance||!euclidean) {
    # now I have the lines I want.
    lines <- sf::st_transform(lines, projected_crs)
    lines <- tidygraph::select(lines, "osm_id", "name")

    region_shp <- sf::st_transform(iso_area, projected_crs)

    #Download osm used a square bounding box, now trim to the exact boundary
    #note that lines that that cross the boundary are still included
    lines <- lines[region_shp,]

    # Round coordinates to 0 digits.
    sf::st_geometry(lines) <- sf::st_geometry(lines) %>%
      sf::st_sfc(crs = sf::st_crs(lines))

    # Network
    network_file <- sfnetworks::as_sfnetwork(lines, directed = FALSE)
    network_file <- tidygraph::convert(network_file, sfnetworks::to_spatial_subdivision)

    #convert network to an sf object of edge

    net_sf <- network_file %>% tidygraph::activate("edges") %>%
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
    network_file <- network_file %>%
      tidygraph::activate("nodes") %>%
      sf::st_filter(osm_connected_edges, .pred = sf::st_intersects)

    net_intersect <- sf::st_transform(network_file,  crs = 4326)
    net_points <- sf::st_geometry(net_intersect)
    # Compute the edge weights bsased on their length
    network_file <- tidygraph::mutate(tidygraph::activate(network_file, "edges"),
                                      weight = sfnetworks::edge_length())
  }

  if (missing(parks)) {

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



    # Parks cleaning
    parks <- res$osm_polygons
    parks <- tidygraph::select(parks, "osm_id", "name")
    parks <- sf::st_make_valid(parks)

    if (pseudo_entrance){
      parks_buffer <- sf::st_buffer(parks, 20)

      st_intersection_faster <- function(x,y){
        #faster replacement for st_intersection(x, y,...)

        y_subset <-
          sf::st_intersects(x, y) %>%
          unlist() %>%
          unique() %>%
          sort() %>%
          {y[.,]}

        sf::st_intersection(x, y_subset)
      }
      parks_point <- st_intersection_faster(net_points, parks_buffer)
      parks_point <- parks_point %>% sf::st_transform(crs=projected_crs)
    }
    else{
      parks_point <- sf::st_centroid(parks) %>% sf::st_transform(crs=projected_crs)
    }


  }
  else {
    if (sf::st_crs(address_location) != sf::st_crs(parks))
    {
      message("The CRS of your parks data set is geographic, CRS of main data set will be used to transform")
      parks <- sf::st_transform(parks, projected_crs)
    }

    if ("POINT" %in% sf::st_geometry_type(parks)){
      # Do nothing
    } else {
      if (pseudo_entrance){
        message('Fake entrances for the given park polygons will be created')
        parks_buffer <- sf::st_buffer(parks, 20)
        parks_point <- sf::st_intersection(net_points, parks_buffer)
        parks_point <- parks_point %>% sf::st_transform(crs=projected_crs)
      }
      else {
        parks_point <- sf::st_centroid(parks) %>% sf::st_transform(crs=projected_crs)
      }
    }
  }

  ### Calculations
  address_location <- sf::st_transform(address_location, projected_crs)

  if (euclidean) {

    # get the nearest neighbhors of the address_location + parks
    nearest_neighbours <- FNN::get.knnx(sf::st_coordinates(parks_point), sf::st_coordinates(address_location), k = 5)
    closest_park <- nearest_neighbours$nn.dist[,1]
    euclidean_dist_df <- as.data.frame(closest_park)
    parks_in_buffer <- ifelse((rowSums(units::drop_units(euclidean_dist_df) < buffer_distance) > 0), TRUE, FALSE)

  }else{
    #if (pseudo_entrance) {
    if (entrances_within_buffer) {
      closest_park <- vector("numeric", length = nrow(address_location))

      for (i in 1:nrow(address_location)) {
        address <- address_location[i, ]
        address_buffer <- sf::st_buffer(address, dist = buffer_distance)
        parks_within_buffer <- sf::st_intersection(address_buffer, parks_point)

        if (nrow(parks_within_buffer) > 0) {
          add_park_dist <- sfnetworks::st_network_cost(network_file, from = address, to = parks_within_buffer)
          closest_park[i] <- min(add_park_dist)
        } else {
          closest_park[i] <- NA
        }
      }
      parks_in_buffer <- ifelse(is.na(closest_park), FALSE, TRUE)

    }else{
      add_park_dist <- as.data.frame(sfnetworks::st_network_cost(network_file, from = address_location, to = parks_point))
      closest_park <- apply(add_park_dist, 1, FUN = min)
      parks_in_buffer <- ifelse((rowSums(units::drop_units(add_park_dist) < buffer_distance) > 0), TRUE, FALSE)

    }

    # Clip the buffer distance as default

  }




  if (missing(UID)) {
    UID <- 1:nrow(address_location)
  }


  df <- data.frame(UID, sf::st_geometry(address_location), closest_park, parks_in_buffer)
  df <- sf::st_as_sf(df)

  print(Sys.time()-start_function)
  print('time to run the entire function')



  ### Finalizing the output


  return(df)





}





