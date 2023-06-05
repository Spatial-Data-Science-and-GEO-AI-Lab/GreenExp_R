
#' Park Coverage
#'
#' @param address_location A spatial object representing the location of interest, the location should be in projected coordinates.
#' @param park_layer A park layer, the layer should be in projected coordinates
#' @param buffer_distance  A distance in meters to create a buffer or isochrone around the address location
#' @param UID A character string representing a unique identifier for each point of interest
#' @param network_buffer A logical, the default is an euclidean buffer, when TRUE, a network buffer will be used.
#' @param speed  A numeric value representing the speed in km/h to calculate the buffer distance (required if `time` is provided)
#' @param time  numeric value representing the travel time in minutes to calculate the buffer distance (required if `speed` is provided)
#' @param network_file An optional sfnetwork object representing a road network, If missing the road network will be created.
#' @param city When using a network buffer, you can add a city where your address points are to speed up the process
#' @param address_calculation  A logical, indicating whether to calculate the address location (if not a point) as the centroid of the polygon containing it (default is 'TRUE')
#' @param download_dir A directory to download the network file, the default will be `tempdir()`.
#' @param epsg_code A espg code to get a Projected CRS in the final output, If missing, the default is `3395`
#'
#' @return Returns the percentage of park coverage given a certain buffer.
#' @export
#'
#' @examples


park_pct <- function(address_location, park_layer=NULL, buffer_distance=NULL, network_buffer=FALSE, download_dir = tempfile(),
                      speed=NULL, time=NULL, epsg_code=NULL, UID=NULL, network_file=NULL, city=NULL, address_calculation=TRUE) {
  start_function <- Sys.time()
#####
#Make sure main data set has projected CRS and save it
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
  ### Address vs area
  if (address_calculation) {
    ### Check for any polygons, convert into centroids if there are any
    if ("POINT" %in% sf::st_geometry_type(address_location)) {
    }else if (missing(buffer_distance)) {
      stop("You do not have a point geometry and did not provide a buffer, please provide a point geometry or a buffer distance")
    }
    else {
      message('There are nonpoint geometries, they will be converted into centroids')
      address_location <- sf::st_centroid(address_location)
    }
    if (missing(buffer_distance)){
      if(missing(speed)||missing(time)){
        stop("You didn't enter speed or time, please enter speed or time, of the buffer distance.")
      } else if (!speed > 0) {
        stop("Speed must be a positive integer")
      } else if (!time > 0) {
        stop("Time must be a positive integer")
      } else{
        buffer_distance <- speed * 1000/ 60 * time
      }
      # print("To calculate buffer distance")
      #
    }

    # If people want to calculate the network buffer.
    if (network_buffer) {
      message('You will use a network to create a buffer around the address location(s),
              Keep in mind that for large files it can take a while to run the funciton.')
      if(missing(network_file)){
        message('You did not provide a network file, osm will be used to create a network file.')
        # make sure that speed and time are given in the function.
        if(missing(speed)||missing(time)){
          stop("You didn't enter speed or time, please enter speed or time.")
        } else if (!speed > 0) {
          stop("Speed must be a positive integer")
        } else if (!time > 0) {
          stop("Time must be a positive integer")
        }
        # Now we know that the speed and time are given, calculations can be done.
        # start <- Sys.time()
        ### Extracting OSM road structure to build isochrone polygon
        iso_area <- sf::st_buffer(sf::st_convex_hull(
          sf::st_union(sf::st_geometry(address_location))),
          buffer_distance)
        iso_area <- sf::st_transform(iso_area, crs = 4326)
        # bbox might be redundant
        bbox <- sf::st_bbox(iso_area)
        # Use the osmextract package to extract the lines in the area.
        if (!missing(city)) {

          # start<-Sys.time()
          lines <- osmextract::oe_get(city, stringsAsFactors=FALSE, boundary=iso_area, max_file_size = 5e+09,
                                      download_directory=download_dir,
                                      boundary_type = 'spat')
          # print(Sys.time()-start)

        } else{
          warning('If a city is missing, it will take more time to run the function')
          # start<-Sys.time()
          lines <- osmextract::oe_get(iso_area, stringsAsFactors=FALSE, boundary=iso_area, max_file_size = 5e+09,
                                      download_directory=download_dir,
                                      boundary_type = 'spat')
          # print(Sys.time()-start)
        }

        ## We might need the multilinestrings as well?
        # start<-Sys.time()
        # b <- osmextract::oe_get(iso_area, stringsAsFactors=FALSE,quiet=T, layer='multilinestrings', boundary=iso_area, boundary_type = 'spat')
        # print(Sys.time()-start)


      }
      else{
        #Check if the address location and the network file that was given have the same CRS.
        if (sf::st_crs(address_location) != sf::st_crs(network_file))
        {
          print("The CRS of your network data set is geographic, CRS of main data set will be used to transform")
          network_file <- sf::st_transform(network_file, sf::st_crs(address_location))
        }

      }
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
      # start=Sys.time()
      net_sf <- network_file %>% tidygraph::activate("edges") %>%
        sf::st_as_sf()
      # print(Sys.time()-start)
      # print('To activate edges')
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
      # start <- Sys.time()

      # Compute the edge weights bsased on their length
      network_file <- tidygraph::mutate(tidygraph::activate(network_file, "edges"),
                                        weight = sfnetworks::edge_length())

      network_file <- network_file%>%
        tidygraph::activate("edges") %>%
        tidygraph::mutate(speed = units::set_units(speed[dplyr::cur_group_id()], "m/s")) %>%
        tidygraph::mutate(duration = weight / speed) %>%
        tidygraph::ungroup()

      network_file<- tidygraph::activate(network_file, "nodes")

      iso_list <- list()

      n_iter <- nrow(address_location)


      pb <- progress::progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                                       total = n_iter,
                                       complete = "=",   # Completion bar character
                                       incomplete = "-", # Incomplete bar character
                                       current = ">",    # Current bar character
                                       clear = FALSE,    # If TRUE, clears the bar when finish
                                       width = 100)      # Width of the progress bar



      # Calculate nearest features for all address locations
      nearest_features <- sf::st_nearest_feature(address_location, network_file)
      start <- Sys.time()
      iso_list <- lapply(1:n_iter, function(i) {
        pb$tick()
        tidygraph::filter(network_file, tidygraph::node_distance_from(
          nearest_features[i], weights = duration) <= time * 60)
      })
      n_iter2 <- length(iso_list)

      pb2 <- progress::progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                                        total = n_iter2,
                                        complete = "=",   # Completion bar character
                                        incomplete = "-", # Incomplete bar character
                                        current = ">",    # Current bar character
                                        clear = FALSE,    # If TRUE, clears the bar when finish
                                        width = 100)      # Width of the progress bar


      # Building polygons
      iso_poly <- NULL
      for (i in 1:n_iter2) {
        pb2$tick()
        iso_poly[i] <- iso_list[[i]] %>%
          # for each isochrone extract the geometry
          sf::st_geometry() %>%
          # combine them into a single geometry
          sf::st_combine() %>%
          # computes the smallest convex polygon that contains the geometry
          sf::st_convex_hull()
      }
      calculation_area <- sf::st_as_sf(sf::st_sfc(iso_poly)) %>% sf::st_set_crs(projected_crs)







    }
    else {
      message('Euclidean distance will be used to calculate the buffers around the address location that is given')
      calculation_area <- sf::st_buffer(address_location, dist = buffer_distance)

      iso_area <- sf::st_buffer(sf::st_convex_hull(
        sf::st_union(sf::st_geometry(address_location))),
        buffer_distance)
      iso_area <- sf::st_transform(iso_area, crs = 4326)
    }

  }
  else {
    calculation_area <- address_location

    iso_area <- sf::st_buffer(sf::st_convex_hull(
      sf::st_union(sf::st_geometry(address_location))),
      buffer_distance)
    iso_area <- sf::st_transform(iso_area, crs = 4326)
  }

  ### Creating park_layer set if not given
  if (missing(park_layer)) {
    ### Building area polygon and loading park_layer
    # Area assignment
    # start <- Sys.time()

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
    # print('Time to extract parks'
    # )
    # print(Sys.time()-start)
    #
    # start <- Sys.time()

    # park_layer cleaning
    park_layer <- res$osm_polygons
    park_layer <- tidygraph::select(park_layer, "osm_id", "name")
    park_layer <- sf::st_make_valid(park_layer)
    park_layer <- sf::st_transform(park_layer, projected_crs)

    # print('time to clean park layer')
    # print(Sys.time()-start)

  } else {
    if (sf::st_crs(address_location) != sf::st_crs(park_layer))
    {
      paste("The CRS of your park_layer data set is geographic, CRS of main data set will be used to transform")
      park_layer <- sf::st_transform(park_layer, projected_crs)
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
  print('running the function took:')
  print(Sys.time()-start_function)

  return(df)
}
