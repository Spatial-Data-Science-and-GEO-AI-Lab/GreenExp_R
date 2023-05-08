

#' Calculate the percentage of area covered by each land cover class within a given buffer distance or location
#'
#' @param address_location A spatial object representing the location of interest, the location should be in projected coordinates.
#' @param class_raster A raster object representing a land cover classification map, where each pixel is assigned a land cover class
#' @param buffer_distance A distance in meters to create a buffer or isochrone around the address location
#' @param net an optional sfnetwork object representing a road network
#' @param UID A character string representing a unique identifier for each point of interest
#' @param address_calculation A logical, indicating whether to calculate the address location (if not a point) as the centroid of the polygon containing it (default is 'TRUE')
#' @param speed A numeric value representing the speed in km/h to calculate the buffer distance (required if `time` is provided)
#' @param time A numeric value representing the travel time in minutes to calculate the buffer distance (required if `speed` is provided)
#'
#' @return The percentage of each land cover type within a given buffer or isochrone around a set of locations is printed.
#' @export
#'
#' @examples
#'
#'
land_cover <- function(address_location, class_raster, buffer_distance, net, UID, address_calculation = TRUE, speed, time) {
  ### Preparation
  # Create full class set vector
  codes <- c("UID", unique(values(class_raster)))
  # replace the NA values to <NA>
  codes <- replace(codes, is.na(codes), "<NA>")


  # assign the name of class_Raster to the variable rast_value_name
  rast_value_name <- names(class_raster)

  # Check if the CRS is in longitude-latitude format
  if (sf::st_is_longlat(address_location)){
    stop("The CRS in your main data set has geographic coordinates, please transform it into your local projected CRS")

  }


  # assign the projected crs to the variable projected_crs
  projected_crs <- sf::st_crs(address_location)

  if (address_calculation) {
    ### Check for any polygons, convert into centroids if there are any
    if ("POINT" %in% sf::st_geometry_type(address_location)) {
      # Do nothing
    }
       else {
      message('There are nonpoint geometries, they will be converted into centroids')
      address_location <- sf::st_centroid(address_location)
    }
    # Create a buffer or isochrone around the address location
    if (missing(buffer_distance)) {
      # Check for speed and time + Count buffer for bbox of initial set
      if(missing(speed)||missing(time)){
        stop("You didn't enter speed or time")
      } else if (!speed > 0) {
        stop("Speed must be a positive integer bigger than 0")
      } else if (!time > 0) {
        stop("Time must be a positive integer bigger than 0")
      } else{
        buffer_distance <- speed * 1000/ 60 * time
      }

    ### Check, do we use a entered network or loading a new one
    if (missing(net)) {
      ### Extracting OSM road structure to build isochrone polygon
      iso_area <- sf::st_buffer(sf::st_convex_hull(
        sf::st_combine(sf::st_geometry(address_location))),
        buffer_distance)
      iso_area <- sf::st_transform(iso_area, crs = 4326)
      bbox <- sf::st_bbox(iso_area)
      q <- osmdata::opq(bbox) %>%
        osmdata::add_osm_feature(key = "highway") %>%
        osmdata::osmdata_sf()

      # extract lines and polygons from the OSM data
      lines <- q$osm_lines
      polys <- q$osm_polygons

      # make the coordinates numeric
      lines$osm_id <- as.numeric(lines$osm_id)
      polys$osm_id <- as.numeric(polys$osm_id)

      # Remove invalid polygons
      polys <- polys[!is.na(polys$highway),]
      polys <- polys[is.na(polys$area),]

      #convert the polygons to lines
      polys <- sf::st_cast(polys, "LINESTRING")
      lines <- sf::st_cast(lines, "LINESTRING")


      #remove invalid geometries
      polys <- polys[sf::st_is_valid(polys) %in% TRUE,]

      # Combine the lines and polygons
      lines <- rbind(lines,polys)

      #convert back to the user projection
      lines <- sf::st_transform(lines, projected_crs)
      lines <- tidygraph::select(lines, "osm_id",  "name")
      region_shp <- sf::st_transform(iso_area, projected_crs)

      # Trim to exact boundary
      lines <- lines[region_shp,]

      # Round coordinates to 0 digits
      sf::st_geometry(lines) <- sf::st_geometry(lines) %>%
        lapply(function(x) round(x, 0)) %>%
        sf::st_sfc(crs = sf::st_crs(lines))

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
      } else  {
        # If the CRS of the network data set is geographic, tranfrom it to the project CRS
        if (sf::st_crs(address_location) != sf::st_crs(net))
        {
          message("The CRS of your network data set is geographic, CRS of main data set will be used to transform")
          net <- sf::st_transform(net, projected_crs)
        }
      }
      # Compute the edge weights bsased on their length
      net <- tidygraph::mutate(tidygraph::activate(net, "edges"),
                               weight = sfnetworks::edge_length())
      # Convert speed to m/s
      net <- net %>%
        tidygraph::activate("edges") %>%
        tidygraph::mutate(speed = units::set_units(speed[dplyr::cur_group_id()], "m/s")) %>%
        tidygraph::mutate(duration = weight / speed) %>%
        tidygraph::ungroup()

      ### Building isochrone polygon
      # activate the node layer of the network object
      net <- tidygraph::activate(net, "nodes")
      # Initialize an empty list to store isochrone polygons
      iso_list <- list()
      # loop through the input points
      for (i in 1:nrow(address_location)) {
        # assign the output of each iteration to the iso_list
        # filter the network object to include nodes that meet certain criteria
        # the code finds the set of nodes within.a certain travel time of each input point,
        # and stores it in a separate isochrone polygons
        iso_list[[i]] <- tidygraph::filter(net, tidygraph::node_distance_from(
          sf::st_nearest_feature(address_location[i,], net), weights = duration) <= time * 60)
      }

      # Building polygons
      iso_poly <- NULL
      for (i in 1:length(iso_list)) {
        iso_poly[i] <- iso_list[[i]] %>%
          # for each isochrone extract the geometry
          sf::st_geometry() %>%
          # combine them into a single geometry
          sf::st_combine() %>%
          # computes the smallest convex polygon that contains the geometry
          sf::st_convex_hull()
      }

      # Create a simple feature collection (sf) with sthe st_sf function and use st_as_sf to convert the sf object to an sf
      # dataframe with the polygons as the geometry column
      calculation_area <- sf::st_as_sf(sf::st_sfc(iso_poly))
    }else {
      message("Buffer distance is used for calculations")
      calculation_area <- sf::st_buffer(address_location, dist = buffer_distance)
    }
  } else {
    calculation_area <- address_location
  }
  ### Calculations
  # Extract the landcover values within the buffer
  class_raster_values <- terra::extract(class_raster, calculation_area)
  # count the amount of landcover values per polygon
  class_raster_values <- dplyr::count(class_raster_values, ID, N = get(rast_value_name))
  # Convert to wide format + replacing NA
  class_raster_values_wide <- tidygraph::mutate_all(tidyr::spread(class_raster_values, N, n), ~replace_na(.,0))
  # Calculate the %
  class_raster_values_perc <- round(cbind(class_raster_values_wide$ID, class_raster_values_wide[,-1]/rowSums(class_raster_values_wide[,-1])), 2)

  # Update UID
  names(class_raster_values_perc)[1] <- "UID"
  if (!missing(UID)){
    class_raster_values_perc$UID <- UID
  }

  missings <- setdiff(c("UID", codes), names(class_raster_values_perc))
  missings_df <- setNames(data.frame(matrix(ncol = length(missings), nrow = nrow(address_location))), missings)
  missings_df <- replace(missings_df, is.na(missings_df), 0)
  landcover_values_perc <- cbind(class_raster_values_perc, missings_df, address_location[2], calculation_area)

  # return the result
  return(landcover_values_perc)


  }
