

parks_access <- function(address_location, buffer_distance = 300, net, parks, UID) {
  ### Make sure main data set has projected CRS and save it
  if (sf::st_is_longlat(address_location)){
    stop("The CRS your main data set is geographic, please transform it into your local projected CRS and rerun function")

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
  net <- mutate(activate(net, "edges"), weight = edge_length())

  ### Creating parks set if not given
  if (missing(parks)) {
    ### Building area polygon and loading parks
    # Area assignment
    calculation_area <- st_buffer(address_location, dist = buffer_distance)

    # Initial load of parks
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
    parks <- sf::st_centroid(parks)
    parks <- sf::st_transform(parks, projected_crs)
  } else {
    if (sf::st_crs(address_location) != sf::st_crs(parks))
    {
      print("The CRS of your parks data set is geographic, CRS of main data set will be used to transform")
      parks <- sf::st_transform(parks, original_pr)
    }

    if ("POINT" %in% sf::st_geometry_type(parks)){
      # Do nothing
    } else {
      parks <- sf::st_centroid(parks)
    }
  }

  ### Parks connection
  net_parks <- sfnetworks::st_network_blend(net, parks)

  ### Calculations
  add_park_dist <- as.data.frame(sfnetworks::st_network_cost(net, from = address_location, to = parks, weights = "weight"))
  closest_park <- apply(add_park_dist, 1, FUN = min)
  parks_in_buffer <- ifelse((rowSums(units::drop_units(add_park_dist) < buffer_distance) > 0), TRUE, FALSE)

  if (missing(UID)) {
    UID <- 1:nrow(address_location)
  }

  address <- sf::st_geometry(address_location)

  ### Finalizing the output

  df <- data.frame(UID, sf::st_geometry(address_location), closest_park, parks_in_buffer)

  return(df)
}
