
#' Calculate the percentage of a canopy within a given buffer distance or location
#'
#' @param address_location  A spatial object representing the location of interest, the location should be in projected coordinates.
#' @param canopy_layer  A canopy layer that represents a canopy, the layer should be in projected coordinates
#' @param buffer_distance A distance in meters to create a buffer or isochrone around the address location
#' @param UID A character string representing a unique identifier for each point of interest
#' @param address_location_neighborhood A logical, indicating whether to calculate with an address point or a neighbourhood. default is `FALSE`
#' @param time A numeric value representing the travel time in minutes to calculate the buffer distance (required if `speed` is provided)
#' @param network_buffer A logical, the default is an euclidean buffer, when TRUE, a network buffer will be used.
#' @param network_file An optional sfnetwork object representing a road network, If missing the road network will be created.
#' @param city When using a network buffer, you can add a city where your address points are to speed up the process
#' @param epsg_code A espg code to get a Projected CRS in the final output, If missing, the default is `3395`
#' @param folder_path_network optional; Folder path to where the retrieved network should be saved continuously. Must not include a filename extension (e.g. '.shp', '.gpkg').
#'
#' @return The percentage of the canopy within a given buffer or isochrone around a set of locations is printed.
#' @export
#'
#' @examples


canopy_pct <- function(address_location, canopy_layer, buffer_distance=NULL, network_buffer=FALSE, network_file=NULL,
                        epsg_code=NULL, folder_path_network = NULL,
                        UID=NULL, address_location_neighborhood = FALSE, speed=NULL, time=NULL, city=NULL){

  ###### 1. Preperation + Cleaning #######
  start_function <- Sys.time()

  # Make sure main data set has projected CRS and save it as default WGS84 if it has geographich coordinates
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
    projected_crs <- sf::st_crs(address_location)
  }

  if (missing(canopy_layer)){
    stop('You did not provide a canopy layer')
  }


  if (!address_location_neighborhood) {
    ###### 2. Address points ######
    #Check for any polygons, convert into centroids if there are any
    if ("POINT" %in% sf::st_geometry_type(address_location)) {
    }else if (missing(buffer_distance)) {
      stop("You do not have a point geometry and did not provide a buffer, please provide a point geometry or a buffer distance")
    }
    else {
      message('There are nonpoint geometries, they will be converted into centroids')
      address_location <- sf::st_centroid(address_location)
    }

    # If buffer distnace is missing, make sure to calculate it with time and speed,
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
      print("To calculate buffer distance")

    }

    ###### 3. Network buffer ######
    # If people want to calculate the network buffer.
    if (network_buffer) {
      message('You will use a network to create a buffer around the address location(s),
              Keep in mind that for large files it can take a while to run the funciton.')
      # If the network file is missing create the network file using osmextract
      if(missing(network_file)){
        message('You did not provide a network file, osm will be used to create a network file.')
        ###### 2.1 Calculating the network buffer when missing ######
        # Extracting OSM road structure to build isochrone polygona
        iso_area <- sf::st_buffer(sf::st_convex_hull(
          sf::st_union(sf::st_geometry(address_location))),
          buffer_distance)
        iso_area <- sf::st_transform(iso_area, crs = 4326)
        # Use the osmextract package to extract the lines in the area.
        if (!missing(city)) {
          lines <- osmextract::oe_get(city, stringsAsFactors=FALSE, boundary=iso_area,
                                      download_directory=download_dir, max_file_size = 5e+09, boundary_type = 'spat')

        } else{
          message('If a city is missing, it will take more time to run the function')
          lines <- osmextract::oe_get(iso_area, stringsAsFactors=FALSE, boundary=iso_area,
                                      download_directory=download_dir, max_file_size = 5e+09, boundary_type = 'spat')
        }
        # save network file
        if (!is.null(folder_path_network)) {
          if (!dir.exists(folder_path_network)) {
            dir.create(folder_path_network)
          }
          sf::st_write(lines, paste0(folder_path_network,'/','network.gpkg'))
        }

      }

      # If network file is given, make sure the crs are both the same
      else{
        #Check if the address location and the network file that was given have the same CRS.
        if (sf::st_crs(address_location) != sf::st_crs(network_file))
        {
          print("The CRS of your network data set is geographic, CRS of main data set will be used to transform")
          network_file <- sf::st_transform(network_file, sf::st_crs(address_location))
        }
      }

      ####### 2.2 Calculation of the network files ########


      # Give the lines the correct crs, and select items we need
      lines <- sf::st_transform(lines, projected_crs)
      lines <- tidygraph::select(lines, "osm_id", "name")

      # make sure the region is correct crs and trim the lines
      region_shp <- sf::st_transform(iso_area, projected_crs)
      lines <- lines[region_shp,]

      # convert to a network file
      network_file <- sfnetworks::as_sfnetwork(lines, directed = FALSE)
      network_file <- tidygraph::convert(network_file, sfnetworks::to_spatial_subdivision)

      #convert network to an sf object the activated edges
      start=Sys.time()
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
      start <- Sys.time()

      # Compute the edge weights bsased on their length
      network_file <- tidygraph::mutate(tidygraph::activate(network_file, "edges"),
                                        weight = sfnetworks::edge_length())
      # Convert speed to m/s
      if (!missing(speed)){
        network_file <- network_file %>%
          tidygraph::activate("edges") %>%
          tidygraph::mutate(speed = units::set_units(speed[dplyr::cur_group_id()], "m/s")) %>%
          tidygraph::mutate(weight = weight / speed) %>%
          tidygraph::ungroup()
      }

      # Activate the nodes of the network file
      network_file<- tidygraph::activate(network_file, "nodes")

      # the amount of iterations
      n_iter <- nrow(address_location)

      # Set up a progress bar
      pb <- progress::progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                                       total = n_iter,
                                       complete = "=",   # Completion bar character
                                       incomplete = "-", # Incomplete bar character
                                       current = ">",    # Current bar character
                                       clear = FALSE,    # If TRUE, clears the bar when finish
                                       width = 100)      # Width of the progress bar


      # create an empty list for the nearest features for the address locations
      iso_list <- list()
      # Calculate nearest features for all address locations
      nearest_features <- sf::st_nearest_feature(address_location, network_file)
      start <- Sys.time()
      iso_list <- lapply(1:n_iter, function(i) {
        pb$tick()
        tidygraph::filter(network_file, tidygraph::node_distance_from(
          nearest_features[i], weights = weight) <= buffer_distance)
      })

      # iterations
      n_iter2 <- length(iso_list)
      # second progress bar
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
    }

  }
  else {
    # If the provided address location is an area
    valid_types_area <- c("POLYGON", "MULTIPOLYGON")
    if (!as.character(sf::st_geometry_type(address_location, by_geometry = FALSE)) %in% valid_types_area){
      stop('Your address location file is not a polygon, or multipolygon, either provide a polygon file,
           or set address_location_neighborhood to TRUE')
    }
    calculation_area <- address_location
  }
##### 4. Canopy layer ####
  if(sf::st_crs(terra::crs(canopy_layer))$epsg != sf::st_crs(calculation_area)$epsg){
    warning('The crs of your canopy layer is not the same as the projected crs of the input location,
              the projected crs of the canopy layer will be transformed into the projected crs of the address location')
    epsg_raster <- sf::st_crs(calculation_area)$epsg
    canopy_layer <- terra::project(canopy_layer, paste0('EPSG:',epsg_raster))

  }

  ### Make the calculations here
  canopy_pct <- list()


  # second progress bar
  pb3 <- progress::progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                                    total = n_iter,
                                    complete = "=",   # Completion bar character
                                    incomplete = "-", # Incomplete bar character
                                    current = ">",    # Current bar character
                                    clear = FALSE,    # If TRUE, clears the bar when finish
                                    width = 100)      # Width of the progress bar

##### 5. calculation #####

    for (i in 1:n_iter) {
      pb3$tick()
      # Clip tree canopy to polygon
      canopy_clip <- sf::st_intersection(canopy_layer, calculation_area[i,])
      # Calculate area of clipped tree canopy
      canopy_area <- sf::st_area(canopy_clip)
      total_area <- sum(canopy_area)
      # Calculate area of polygon
      polygon_area <- sf::st_area(calculation_area[i,])
      # Calculate tree canopy percentage
      canopy_pct[i] <- total_area / polygon_area * 100
    }
    address_location <- sf::st_transform(address_location, projected_crs)
    buffer <- calculation_area
    names(buffer) <- "buffer"
    df <- data.frame(UID = nrow(calculation_area), canopy_pct = cbind(unlist(canopy_pct)),
                     sf::st_geometry(address_location))
    df$UID <- seq.int(nrow(df))
    if (!missing(UID)){
      df$UID <- UID}

    df <- sf::st_as_sf(df)
  return(df)
}
