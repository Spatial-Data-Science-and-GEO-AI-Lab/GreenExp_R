#' Distance to the nearest fake entries of parks
#'
#' @param address A spatial object representing the location of interest, the location should be in projected coordinates.
#' @param buffer_distance A distance in meters to create a buffer or isochrone around the address location
#' @param net An optional \code{sfnetworks} object representing a road network
#' @param parks A spatial object representing parks
#' @param UID A character string representing a unique identifier for each point of interest
#'
#' @return The distance to the nearest fake entries of parks and whether the parks are within the buffer distance that is given
#' @export
#'
#' @examples
parks_access_entrance <- function(address, buffer_distance = 300, net, parks, UID) {

  start_function <- Sys.time()
  ### Make sure main data set has projected CRS and save it

  address_location <- sf::st_geometry(address)
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
    start <- Sys.time()

    iso_area <- sf::st_buffer(sf::st_convex_hull(
      sf::st_union(sf::st_geometry(address_location))),
      buffer_distance)
    iso_area <- sf::st_transform(iso_area, crs = 4326)
    bbox <- sf::st_bbox(iso_area)
    q <- osmdata::opq(bbox) %>%
      osmdata::add_osm_feature(key = "highway") %>%
      osmdata::osmdata_sf()
    print(Sys.time()-start)
    print('Time to calculate the osmdata sf')

    start <- Sys.time()

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

    print(Sys.time()-start)
    print('time to bind lines and polys together from osm data sf')


    #Change to user projection
    start <- Sys.time()
    lines <- sf::st_transform(lines, projected_crs)
    lines <- tidygraph::select(lines, "osm_id", "name")
    region_shp <- sf::st_transform(iso_area, projected_crs)
    print(Sys.time()-start)
    print('time to change user projection')

    #Download osm used a square bounding box, now trim to the exact boundary
    #note that lines that that cross the boundary are still included

    start <- Sys.time()
    lines <- lines[region_shp,]
    print(Sys.time()-start)
    print('time to trim the exact boundary')

    sf::st_geometry(lines) <- sf::st_geometry(lines) %>%
      sf::st_sfc(crs = sf::st_crs(lines))



    start <- Sys.time()
    # Network
    net <- sfnetworks::as_sfnetwork(lines, directed = FALSE)
    net <- tidygraph::convert(net, sfnetworks::to_spatial_subdivision)
    print(Sys.time()-start)
    print('time to make the network and convert it into spatial subdivision')



    start <-Sys.time()
    #convert network to an sf object of edge
    net_sf <- net %>% tidygraph::activate("edges") %>%
      sf::st_as_sf()
    print(Sys.time()-start)
    print('time to activate edges')

    start <- Sys.time()
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

    print(Sys.time()-start)
    print('Time to subset the edges corrsponding to the biggest connected component')
    # Filter nodes that are not connected to the biggest connected component
    start <- Sys.time()
    net <- net %>%
      tidygraph::activate("nodes") %>%
      sf::st_filter(osm_connected_edges, .pred = sf::st_intersects)
    print(Sys.time()-start)
    print('Time to activate nodes that are connected to biggest compontnt')


    net_intersect <- sf::st_transform(net,  crs = 4326)
    net_points <- sf::st_geometry(net_intersect)
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
  start <- Sys.time()
  net <- tidygraph::mutate(tidygraph::activate(net, "edges"), weight = sfnetworks::edge_length())
  print(Sys.time()-start)
  print('time to add edge length for future calc')

  ### Creating parks set if not given

  if (missing(parks)) {
    start <- Sys.time()
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
    parks_buffer <- sf::st_buffer(parks, 20)

    fake_entrance <- sf::st_intersection(net_points, parks_buffer)
    fake_entrance <- fake_entrance %>% sf::st_transform(crs=projected_crs)
    print(Sys.time()-start)
    print('time to calculate fake entrances parks')
  }
    else {
      if (sf::st_crs(address_location) != sf::st_crs(parks))
      {
        message("The CRS of your parks data set is geographic, CRS of main data set will be used to transform")
        parks <- sf::st_transform(parks, original_pr)
      }

      if ("POINT" %in% sf::st_geometry_type(parks)){
        # Do nothing
      } else {
        message('Fake entrances for the given park polygons will be created')
        parks_buffer <- sf::st_buffer(parks, 20)
        fake_entrance <- sf::st_intersection(net_points, parks_buffer)
        fake_entrance <- fake_entrance %>% sf::st_transform(crs=projected_crs)
      }
    }

    ### Calculations
  start <- Sys.time()
    add_park_dist <- as.data.frame(sfnetworks::st_network_cost(net, from = address_location, to = fake_entrance))
    closest_park <- apply(add_park_dist, 1, FUN = min)
    parks_in_buffer <- ifelse((rowSums(units::drop_units(add_park_dist) < buffer_distance) > 0), TRUE, FALSE)
    print(Sys.time()-start)
    print('time to do calculations')


    if (missing(UID)) {
      UID <- 1:nrow(address)
    }

    # if ("UID" %in% colnames(address)) {
    #   df <- data.frame(address, closest_park, parks_in_buffer)
    #   df <- sf::st_as_sf(df)
    # } else{
    #
    #   df <- data.frame(UID, address, closest_park, parks_in_buffer)
    #   df <- sf::st_as_sf(df)
    # }
    df <- data.frame(UID, address_location, closest_park, parks_in_buffer)
    df <- sf::st_as_sf(df)

    print(Sys.time()-start_function)
    print('time to run the entire function')



    ### Finalizing the output


    return(df)
}

