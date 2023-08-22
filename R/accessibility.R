#' Distance to the nearest green spaces
#'
#' @param address_location A \code{\link[sf]{sf}} object representing the location of interest, the location should be in projected coordinates.
#' @param buffer_distance A distance in meters to create a buffer or isochrone around the address location
#' @param greenspace Optional; a \code{\link[sf]{sf}} object representing a greenspace layer, the layer should be in projected coordinates
#' @param UID A character string representing a unique identifier for each point of interest
#' @param network_file Optional; a \code{\link[sfnetworks]{sfnetwork}} object representing a road network, If missing the road network will be created.
#' @param speed A numeric value representing the speed in km/h to calculate the buffer distance (required if `time` is provided)
#' @param time A numeric value representing the travel time in minutes to calculate the buffer distance (required if `speed` is provided)
#' @param city When using a network buffer, you can add a city where your address points are to speed up the process
#' @param epsg_code A espg code to get a Projected CRS in the final output, If missing, the default is `3395`
#' @param euclidean Whether the distance between the fake entry points and the address location is euclidean, or calculated with the network.
#' @param pseudo_entrance The possibility to calculate the distance to created pseudo entrances. These pseudo entrances are created,
#' by intersecting a buffer that was created around a greenspace with network points. default is `FALSE`
#' @param entrances_within_buffer The possibility to get the closest greenspace for every point. default is `FALSE`
#' @param folder_path_network optional; Folder path to where the retrieved network should be saved continuously. Must not include a filename extension (e.g. '.shp', '.gpkg').
#' @param folder_path_greenspace optional; Folder path to where the retrieved greenspaces should be saved continuously. Must not include a filename extension (e.g. '.shp', '.gpkg').
#' @param folder_path_lines optional; Folder path to where the shortest distance lines should be saved continuously. Must not include a filename extension (e.g. '.shp', '.gpkg').
#' @param minimum_greenspace_size The minimum size of the greenspace in m2.
#'
#' @examples
#' #' # Load a dataframe
#' df <- Ams_Houses[1:10, ]
#'
#' # Euclidean Distance to Centroid of Green spaces
#' greenspace_access(address_location = df, buffer_distance=300)
#'
#' # Network Distance to Centroid of Green spaces
#' greenspace_access(address_location=df, buffer_distance=300, euclidean = FALSE)
#'
#' #Network Distance to Pseudo entrance points, and the greenspaces should be at least 400m2
#' greenspace_access(address_location=df, buffer_distance=300, euclidean=FALSE, pseudo_entrance=TRUE,
#'   minimum_greenspace_size=400)
#'
#'
#' @return The distance to the nearest greenspace and whether the greenspace are within the buffer distance that is given
#' @export
#'
#'
#'
greenspace_access <- function(address_location, greenspace = NULL, buffer_distance = NULL, network_file=NULL,
                           epsg_code=NULL, folder_path_network = NULL, euclidean=TRUE,
                           minimum_greenspace_size=0,
                           folder_path_greenspace=NULL, pseudo_entrance=FALSE, entrances_within_buffer=TRUE,
                           folder_path_lines=NULL, speed=NULL, time=NULL, city=NULL, UID=NULL) {
  # timer for function
  start_function <- Sys.time()
  ##### 1. preparation and cleaning #######
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
  # Make sure there is a buffer distance, or speed or time

  if (missing(buffer_distance)){
    if(missing(speed)||missing(time)){
      stop("You didn't enter speed and time or the buffer distance,
             please enter speed and time, or the buffer distance.")
    } else if (!speed > 0) {
      stop("Speed must be a positive number")
    } else if (!time > 0) {
      stop("Time must be a positive number")
    } else{
      buffer_distance <- speed * 1000/ 60 * time
    }
  }
  if ("POINT" %in% sf::st_geometry_type(address_location)) {
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

  ##### 2. Network file #######
  if (missing(network_file)){
    # Use the osmextract package to extract the lines in the area.
    if (pseudo_entrance || !euclidean){
      message('You did not provide a network file, a network will be created using osm.')
      if (!missing(city)) {

        lines <- osmextract::oe_get(city, stringsAsFactors=FALSE, boundary=iso_area,
                                    max_file_size = 5e+09, boundary_type = 'spat')


      } else{
        message('If a city is missing, it will take more time to run the function')

        lines <- osmextract::oe_get(iso_area, stringsAsFactors=FALSE, boundary=iso_area,
                                    max_file_size = 5e+09, boundary_type = 'spat')
      }
    }
    if (!is.null(folder_path_network)) {
      if (!dir.exists(folder_path_network)) {
        dir.create(folder_path_network)
      }
      sf::st_write(lines, paste0(folder_path_network,'/','network.gpkg'))
    }

  }

  else  {
    # If the CRS of the network data set is geographic, tranfrom it to the project CRS
    if (sf::st_crs(address_location) != sf::st_crs(network_file))
    {
      print("The CRS of your network data set is geographic, CRS of main data set will be used to transform")
      lines <- sf::st_transform(network_file, projected_crs)
    } else{
      lines <- network_file
    }
  }
  ##### 2.1 calculate the network file if we are using network distance or pseudo entrances ######

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

  ##### 3. greenspace #######
  # When greenspace are missing take them from osmdata
  if (missing(greenspace)) {
    message('You did not provide a greenspace layer, osmdata will be used to find greenspaces')
    ##### 3.1 If greenspace are not given ####

    q1 <- osmdata::opq(sf::st_bbox(iso_area)) %>%
      osmdata::add_osm_feature(key = "landuse",
                               value = c('allotments', 'recreation_ground', 'grass', 'forest',
                                         'greenfield','village_green', 'meadow', 'orchard')) %>%
      osmdata::osmdata_sf()

    q2 <- osmdata::opq(sf::st_bbox(iso_area)) %>%
      osmdata::add_osm_feature(key = "leisure",
                               value = c('garden','fitness_station', 'common', 'dog_park',
                                         'greenfield', 'grassland', 'heath',
                                         'nature_reserve','park','playground')) %>%
      osmdata::osmdata_sf()

    q3 <- osmdata::opq(sf::st_bbox(iso_area)) %>%
      osmdata::add_osm_feature(key = "natural",
                               value = c('grassland','wood', 'scrub', 'heath', 'moor')) %>%
      osmdata::osmdata_sf()
    res <- c(q1, q2, q3)




    # greenspace cleaning
    greenspace <- res$osm_polygons
    greenspace <- tidygraph::select(greenspace, "osm_id", "name")
    greenspace <- sf::st_make_valid(greenspace)
    threshold_units <- units::set_units(minimum_greenspace_size, "m^2")

    # Filter greenspaces based on area
    greenspace <- greenspace[sf::st_area(greenspace) > threshold_units, ]

    if (!is.null(folder_path_greenspace)) {
      if (!dir.exists(folder_path_greenspace)) {
        dir.create(folder_path_greenspace)
      }
      sf::st_write(greenspace, paste0(folder_path_lines,'/','greenspace.gpkg'))
    }
    ##### 3.1.1 greenspace entrances for psuedo entrances #####
    if (pseudo_entrance){
      greenspace_buffer <- sf::st_buffer(greenspace, 20)

      # When greenspace are overlapping, combine them
      greenspace_combined <- sf::st_union(greenspace_buffer)

      # make it a multipolygon
      greenspace_buffer <- sf::st_as_sf(sf::st_sfc(greenspace_combined))
      greenspace_buffer <- sf::st_make_valid(greenspace_buffer)

      st_intersection_faster <- function(x,y){
        #faster replacement for st_intersection(x, y,...)

        y_subset <-
          sf::st_intersects(x, y) %>%
          unlist() %>%
          sort() %>%
          rle() %>%
          {y[.,]}

        sf::st_intersection(x, y_subset)
      }
      greenspace_point <- st_intersection_faster(net_points, greenspace_buffer)
      greenspace_point <- greenspace_point %>% sf::st_transform(crs=projected_crs)
    }
    else{
      # Take centroid of the greenspace if no pseudo entrances are wanted
      greenspace_point <- sf::st_centroid(greenspace) %>% sf::st_transform(crs=projected_crs)
    }


  }
  ##### 3.2 If greenspace dataset is given #####
  else {
    if (sf::st_crs(address_location) != sf::st_crs(greenspace))
    {
      message("The CRS of your greenspace data set is different from the address location,
              CRS of main data set will be used to transform")
      greenspace <- sf::st_transform(greenspace, projected_crs)
    }

    if ("POINT" %in% sf::st_geometry_type(greenspace)){
      # Do nothing
    } else {
      threshold_units <- units::set_units(minimum_greenspace_size, "m^2")
      # Filter greenspaces based on area
      greenspace <- greenspace[sf::st_area(greenspace) > threshold_units, ]
      if (pseudo_entrance){
        message('Fake entrances for the given greenspace polygons will be created')
        greenspace_buffer <- sf::st_buffer(greenspace, 10)

          # When greenspace are overlapping, combine them
        greenspace_combined <- sf::st_union(greenspace_buffer)

        # make it a multipolygon
        greenspace_buffer <- sf::st_as_sf(sf::st_sfc(greenspace_combined))
        greenspace_buffer <- sf::st_make_valid(greenspace_buffer)


        st_intersection_faster <- function(x,y){
          #faster replacement for st_intersection(x, y,...)

          y_subset <-
            sf::st_intersects(x, y) %>%
            unlist() %>%
            sort() %>%
            rle() %>%
            {y[.,]}

          sf::st_intersection(x, y_subset)
        }
        greenspace_point <- st_intersection_faster(net_points, greenspace_buffer)
        greenspace_point <- greenspace_point %>% sf::st_transform(crs=projected_crs)
      }
      else {
        greenspace_point <- sf::st_centroid(greenspace) %>% sf::st_transform(crs=projected_crs)
      }
    }
  }

##### 4. Calculations #####
  address_location <- sf::st_transform(address_location, projected_crs)

##### 4.1 Euclidean distance calculations #####
  if (euclidean) {

    # get the nearest neighbhors of the address_location + greenspace
    nearest_neighbours <- FNN::get.knnx(sf::st_coordinates(greenspace_point), sf::st_coordinates(address_location), k = 5)
    closest_greenspace <- nearest_neighbours$nn.dist[,1]
    euclidean_dist_df <- as.data.frame(closest_greenspace)
    greenspace_in_buffer <- ifelse((rowSums(units::drop_units(euclidean_dist_df) < buffer_distance) > 0), TRUE, FALSE)

    if (!is.null(folder_path_lines)) {
      if (!dir.exists(folder_path_lines)) {
        dir.create(folder_path_lines)
      }
      indeces <- nearest_neighbours$nn.index[, 1]
      closest_greenspace <- greenspace_point[indeces, ]
      coords_address <- data.frame(sf::st_coordinates(address_location))
      closest_greenspace_coords <- data.frame(sf::st_coordinates(closest_greenspace))


      lines_list <- list()  # Create an empty list to store the line objects

      for (i in 1:nrow(coords_address)) {
        line <- sf::st_linestring(matrix(c(coords_address$X[i], closest_greenspace_coords$X[i],
                                           coords_address$Y[i], closest_greenspace_coords$Y[i]), ncol = 2))
        lines_list[[i]] <- line  # Store the line object in the list
      }

      lines <- sf::st_sfc(lines_list)
      lines <- sf::st_as_sf(lines) %>% sf::st_set_crs(projected_crs)
      sf::st_write(lines, paste0(folder_path_lines,'/','lines.gpkg'))
    }




  }
##### 4.2 Network distance calclulations ######
  else{
    # When the entrances are within the given buffer
    if (entrances_within_buffer) {


      closest_greenspace <- vector("numeric", length = nrow(address_location))

      n_iter <- nrow(address_location)

      # Set up a progress bar
      pb <- progress::progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                                       total = n_iter,
                                       complete = "=",   # Completion bar character
                                       incomplete = "-", # Incomplete bar character
                                       current = ">",    # Current bar character
                                       clear = FALSE,    # If TRUE, clears the bar when finish
                                       width = 100)      # Width of the progress bar

      for (i in 1:n_iter) {
        pb$tick()
        address <- address_location[i, ]
        address_buffer <- sf::st_buffer(address, dist = buffer_distance)
        greenspace_within_buffer <- sf::st_intersection(address_buffer, greenspace_point)

        if (nrow(greenspace_within_buffer) > 0) {
          add_greenspace_dist <- sfnetworks::st_network_cost(network_file, from = address, to = greenspace_within_buffer)
          closest_greenspace[i] <- min(add_greenspace_dist)
        } else {
          closest_greenspace[i] <- NA
        }
      }
      greenspace_in_buffer <- ifelse(is.na(closest_greenspace), FALSE, TRUE)
      #names(greenspace_in_buffer) <- paste0('greenspace_in_',round(buffer_distance),'m_buffer')

    }else{
      # if the entrance is not within the given buffer
      add_greenspace_dist <- as.data.frame(sfnetworks::st_network_cost(network_file, from = address_location, to = greenspace_point))
      closest_greenspace <- apply(add_greenspace_dist, 1, FUN = min)
      greenspace_in_buffer <- ifelse((rowSums(units::drop_units(add_greenspace_dist) < buffer_distance) > 0), TRUE, FALSE)

    }
    if (!is.null(folder_path_lines)) {
      if (!dir.exists(folder_path_lines)) {
        dir.create(folder_path_lines)
      }

      distance_parks<-sfnetworks::st_network_cost(network_file, from = address_location, to = greenspace_point)
      shortest_distances <- apply(distance_parks, 1, which.min)
      park_centroid_closest <- greenspace_centroid[shortest_distances, ]


      final_lines_list <- list()
      for (i in 1:nrow(address_location)) {
        closest_park <- st_network_paths(network_file, from = address_location[i, ], to = park_centroid_closest[i, ])

        node_path <- closest_park %>%
          dplyr::slice(1) %>%
          dplyr::pull(node_paths) %>%
          unlist()

        line_network <- tidygraph::activate(network_file, "nodes") %>%
          dplyr::slice(node_path)

        final_line <- line_network %>%
          tidygraph::activate("edges") %>%
          sf::st_as_sf()

        final_lines_list[[i]] <- final_line
      }

      final_lines_sf <- sf::st_sf(do.call(rbind, final_lines_list))
      sf::st_write(final_lines_sf, paste0(folder_path_lines,'/','lines.gpkg'), append=FALSE)
    }
    }


  if (missing(UID)) {
    UID <- 1:nrow(address_location)
  }


  df <- data.frame(UID, closest_greenspace, greenspace_in_buffer,
                   address_location) %>% sf::st_as_sf()
  colnames(df)[colnames(df) == "greenspace_in_buffer"] <- paste0('greenspace_in_',buffer_distance,'m_buffer')


  ###### Transparent about the usage in the interface ######
  if(euclidean & !pseudo_entrance) {
    message('To calculate the distance from the address to the greenspace,
    euclidean distance to the centroid of the greenspaces is used.')
  }

  if(euclidean & pseudo_entrance) {
    message('To calculate the distance from the address to the greenspace,
    euclidean distancne to pseudo entrances of the greenspaces is used.')
  }

  if(!euclidean & !pseudo_entrance) {
    message('To calculate the distance from the address to the greenspace,
    network distance to centroid of the greenspaces is used.')
  }

  if(!euclidean & pseudo_entrance) {
    message('To calculate the distance from the address to the greenspace,
    network distance to pseudo entrances the greenspaces is used.')
  }


  print(Sys.time()-start_function)
  print('time to run the entire function')



  ### Finalizing the output


  return(df)





}





