
#' greenspace Coverage
#'
#' @param address_location A \code{\link[sf]{sf}} object representing the location of interest, the location should be in projected coordinates.
#' @param greenspace_layer Optional; a \code{\link[sf]{sf}} object representing a greenspace layer, the layer should be in projected coordinates
#' @param buffer_distance  A distance in meters to create a buffer or isochrone around the address location
#' @param UID Optional; a  character string representing a unique identifier for each point of interest
#' @param network_buffer Optional; a  logical, the default is an euclidean buffer, when TRUE, a network buffer will be used.
#' @param network_file Optional; a \code{\link[sfnetworks]{sfnetwork}} object representing a road network, If missing the road network will be created.
#' @param speed  Optional; a  numeric value representing the speed in km/h to calculate the buffer distance (required if `time` is provided)
#' @param time  Optional; a numeric value representing the travel time in minutes to calculate the buffer distance (required if `speed` is provided)
#' @param folder_path_network optional; Folder path to where the retrieved network should be saved continuously. Must not include a filename extension (e.g. '.shp', '.gpkg').
#' @param city Optional; when using a network buffer, you can add a city where your address points are to speed up the process
#' @param address_location_neighborhood Optional; a logical, indicating whether to calculate with an address point or a neighbourhood. default is `FALSE`
#' @param epsg_code Optional; epsg code to get a Projected CRS in the final output, If missing, the default is `3395`
#' @param folder_path_greenspace optional; Folder path to where the retrieved OSM park data will be saved. Must not include name extension
#'
#' @examples
#' # Read a dataset, in this instance we will use the first ten neighborhoods from the
#' # Ams_Neighborhood dataset, which contains polygon geometry
#' df <- Ams_Neighborhoods[1:10,]
#'
#' # calculate the greenspace percentage for the neighborhoods with default settings
#' greenspace_pct(df, buffer_distance=300)
#'
#' # calculate the greenspace percentage for neighborhoods, using the neighborhood as buffer
#' greenspace_pct(df, address_location_neighborhood = TRUE)
#'
#' # calculate the greenspace percentage for the neighborhoods, using the network as buffer
#' greenspace_pct(df, buffer_distance=300, network_buffer=TRUE)
#'
#'
#' @return Returns the percentage of greenspace coverage given a certain buffer.
#' @export
#'
#'


greenspace_pct <- function(address_location, greenspace_layer=NULL, buffer_distance=NULL, network_buffer=FALSE, folder_path_network = NULL,
                      speed=NULL, time=NULL, epsg_code=NULL, UID=NULL, network_file=NULL, city=NULL, address_location_neighborhood=FALSE, folder_path_greenspace = NULL) {
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
  ##### 2. If address location is an address point ######
  if (!address_location_neighborhood) {
    ##### 2.1 Preprocessing #####
    #Check for any polygons, convert into centroids if there are any
    if ("POINT" %in% sf::st_geometry_type(address_location)) {
      #Do nothing
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

    ###### 2.2 Network buffer ######
    # If people want to calculate the network buffer.
    if (network_buffer) {
      message('You will use a network to create a buffer around the address location(s),
              Keep in mind that for large files it can take a while to run the function.')
      # If the network file is missing create the network file using osmextract
      if(missing(network_file)){
        message('You did not provide a network file, osm will be used to create a network file.')
        ###### 3.1 Calculating the network buffer when missing ######
        # Extracting OSM road structure to build isochrone polygon
        iso_area <- sf::st_buffer(sf::st_convex_hull(
          sf::st_union(sf::st_geometry(address_location))),
          buffer_distance)
        iso_area <- sf::st_transform(iso_area, crs = 4326)
        # Use the osmextract package to extract the lines in the area.
        if (!missing(city)) {
          lines <- osmextract::oe_get(city, stringsAsFactors=FALSE, boundary=iso_area,
                                      max_file_size = 5e+09, boundary_type = 'spat')

        } else{
          message('If a city is missing, it will take more time to run the function')
          lines <- osmextract::oe_get(iso_area, stringsAsFactors=FALSE, boundary=iso_area,
                                      max_file_size = 5e+09, boundary_type = 'spat')
        }
        # Save network file
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

      ####### 2.3 Calculation of the network files ########


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
    ###### 2.4 Euclidean Buffer ######
    else {
      message('Euclidean distance will be used to calculate the buffers around the address location that is given')
      calculation_area <- sf::st_buffer(address_location, dist = buffer_distance)
    }

  }
  ##### 3. Area polygon #####
  else {
    # If the provided address location is an area
    valid_types_area <- c("POLYGON", "MULTIPOLYGON")
    if (!as.character(sf::st_geometry_type(address_location, by_geometry = FALSE)) %in% valid_types_area){
      stop('Your address location file is not a polygon, or multipolygon, either provide a polygon file,
           or set address_location_neighborhood to FALSE')
    }
    message('You are using the provided area as buffer to extract the greenspace percentage')
    calculation_area <- address_location
  }

##### 4. greenspace layer #######
  ### Creating greenspace_layer set if not given
  if (missing(greenspace_layer)) {
    message('You did not provide a greenspace layer, osmdata will be used to find greenspaces')
    calculation_area_osm <- sf::st_transform(calculation_area, 4326)
    # Initial load of greenspace_layer
    q1 <- osmdata::opq(sf::st_bbox(calculation_area_osm)) %>%
      osmdata::add_osm_feature(key = "landuse",
                               value = c('allotments', 'recreation_ground', 'grass', 'forest',
                                         'greenfield','village_green', 'meadow', 'orchard')) %>%
      osmdata::osmdata_sf()

    q2 <- osmdata::opq(sf::st_bbox(calculation_area_osm)) %>%
      osmdata::add_osm_feature(key = "leisure",
                               value = c('garden','fitness_station', 'common', 'dog_park',
                                         'greenfield', 'grassland', 'heath',
                                         'nature_reserve','park','playground')) %>%
      osmdata::osmdata_sf()

    q3 <- osmdata::opq(sf::st_bbox(calculation_area_osm)) %>%
      osmdata::add_osm_feature(key = "natural",
                               value = c('grassland','wood', 'scrub', 'heath', 'moor')) %>%
      osmdata::osmdata_sf()
    #res <- c(q1, q2, q3)
    #get individual layer for each green space information, there may be some overlap between layers
    resq1 <- q1
    resq2 <- q2
    resq3 <- q3

    greenlayer1 <- resq1$osm_polygons %>% dplyr::select (osm_id, name) #only select two columns to join
    greenlayer2 <- resq2$osm_polygons %>% dplyr::select (osm_id, name) #only select two columns to join
    greenlayer3 <- resq3$osm_polygons %>% dplyr::select (osm_id, name) #only select two columns to join

    # greenspace_layer cleaning
    greenspace_layer <- rbind(greenlayer1, greenlayer2, greenlayer3) #join all greenspace types

    greenspace_layer <- greenspace_layer[!duplicated(greenspace_layer$osm_id), ] #if there is duplicate osm_id, keep one row

    greenspace_layer <- sf::st_make_valid(greenspace_layer)
    greenspace_layer <- sf::st_transform(greenspace_layer, projected_crs)

    if (!is.null(folder_path_greenspace)) {
      if (!dir.exists(folder_path_greenspace)) {
        dir.create(folder_path_greenspace)
      }
      sf::st_write(greenspace_layer, paste0(folder_path_greenspace,'/','OSMgreenspace.gpkg'), delete_layer = TRUE)
    }

    # print('time to clean greenspace layer')
    # print(Sys.time()-start)

  } else {
    if (sf::st_crs(address_location) != sf::st_crs(greenspace_layer))
    {
      paste("The CRS of your greenspace_layer data set is geographic, CRS of main data set will be used to transform")
      greenspace_layer <- sf::st_transform(greenspace_layer, projected_crs)
    }
    # Make sure the greenspace layer are polygons and no points
    greenspace_geometries <- c("POLYGON", "MULTIPOLYGON")
    if (!as.character(sf::st_geometry_type(address_location, by_geometry = FALSE)) %in% greenspace_geometries){
      stop('The povided greenspace layer is not a polygon, or multipolygon, please provide a (multi)polygon file')
    }
  }

##### 5. Calculation #####
  greenspace_pct <- list()

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

    # Clip greenspace to polygon
    greenspace_clip <- sf::st_intersection(greenspace_layer, calculation_area[i,])
    # Calculate area of clipped greenspace
    greenspace_area <- sf::st_area(greenspace_clip)
    total_area <- sum(greenspace_area)
    # Calculate area of polygon
    polygon_area <- sf::st_area(calculation_area[i,])
    # Calculate tree greenspace percentage
    greenspace_pct[i] <- total_area / polygon_area * 100


  }
  if (missing(UID)){
    UID <- 1:nrow(calculation_area)}

  # Create a dataframe for the results
  df <- data.frame(UID, greenspace_pct = cbind(unlist(greenspace_pct)),
                   address_location) %>% sf::st_as_sf()
  df$greenspace_pct [df$greenspace_pct > 100] <- 100 #for the areas with more than 100% park area due to OSM multiple polygons on the same place!
  print('running the function took:')
  print(Sys.time()-start_function)

  return(df)
}
