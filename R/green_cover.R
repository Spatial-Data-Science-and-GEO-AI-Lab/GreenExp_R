#' Calculate the percentage of area covered by each land cover class within a given buffer distance or location
#'
#' @param address_location A spatial object representing the location of interest, the location should be in projected coordinates.
#' @param buffer_distance A distance in meters to create a buffer or isochrone around the address location
#' @param UID A character string representing a unique identifier for each point of interest
#' @param address_calculation A logical, indicating whether to calculate the address location (if not a point) as the centroid of the polygon containing it (default is 'TRUE')
#' @param speed A numeric value representing the speed in km/h to calculate the buffer distance (required if `time` is provided)
#' @param time A numeric value representing the travel time in minutes to calculate the buffer distance (required if `speed` is provided)
#' @param raster raster file with land cover values, if raster file is missing planetary computer will be used.
#' @param network_buffer A logical, the default is an euclidean buffer, when TRUE, a network buffer will be used.
#' @param network_file n optional sfnetwork object representing a road network, If missing the road network will be created.
#' @param city When using a network buffer, you can add a city where your address points are to speed up the process
#' @param year The year of the satellite images. The years 2020 and 2021 can be used for the time being.
#'
#' @return The percentage of each land cover type within a given buffer or isochrone around a set of locations is printed.
#' @export
#'
#'
#' @examples

land_cover <- function(address_location, raster, buffer_distance=NULL, network_buffer=FALSE,
                          network_file=NULL,  UID=NULL, address_calculation = TRUE, speed=NULL, time=NULL,
                          city=NULL, year='2021') {
  ### Preparation
  start_function <- Sys.time()
  # Make sure main data set has projected CRS and save it
  if (sf::st_is_longlat(address_location)){
    warning("The CRS in your main data set has geographic coordinates, the Projected CRS will be set to WGS 84 / World Mercator")
    sf::st_crs(address_location) <- 3395
  }
  projected_crs <- sf::st_crs(address_location)
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
      print("To calculate buffer distance")

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
        start <- Sys.time()
        ### Extracting OSM road structure to build isochrone polygon
        iso_area <- sf::st_buffer(sf::st_convex_hull(
          sf::st_union(sf::st_geometry(address_location))),
          buffer_distance)
        iso_area <- sf::st_transform(iso_area, crs = 4326)
        # bbox might be redundant
        bbox <- sf::st_bbox(iso_area)
        # Use the osmextract package to extract the lines in the area.
        if (!missing(city)) {
          warning('If a city is missing, it will take more time to run the function')
          start<-Sys.time()
          lines <- osmextract::oe_get(city, stringsAsFactors=FALSE, boundary=iso_area, max_file_size = 5e+09, boundary_type = 'spat')
          print(Sys.time()-start)

        } else{
          start<-Sys.time()
          lines <- osmextract::oe_get(iso_area, stringsAsFactors=FALSE, boundary=iso_area, max_file_size = 5e+09, boundary_type = 'spat')
          print(Sys.time()-start)
        }

        ## We might need the multilinestrings as well?
        # start<-Sys.time()
        # b <- osmextract::oe_get(iso_area, stringsAsFactors=FALSE,quiet=T, layer='multilinestrings', boundary=iso_area, boundary_type = 'spat')
        # print(Sys.time()-start)

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
        start=Sys.time()
        net_sf <- network_file %>% tidygraph::activate("edges") %>%
          sf::st_as_sf()
        print(Sys.time()-start)
        print('To activate edges')
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

      }
      else{
        #Check if the address location and the network file that was given have the same CRS.
        if (sf::st_crs(address_location) != sf::st_crs(network_file))
        {
          print("The CRS of your network data set is geographic, CRS of main data set will be used to transform")
          network_file <- sf::st_transform(network_file, sf::st_crs(address_location))
        }

      }
      start <- Sys.time()

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
    }

  }
  else {
    calculation_area <- address_location
  }

  if (missing(raster)){
    # make sure the year is a character
    if (!is.character(year)){
      year <- is.character(year)
    }

    if (year < '2020'||year > '2021'){
      warning('The sattelite have images in 2020 and 2021, please select one of these years,
             it will be set to default now, which is 2021.')
      year <- '2021'

    }
    address <- address_location
    address <- sf::st_transform(address, 4326)
    #calculation_area <- sf::st_geometry(calculation_area)
    calculation_area <- sf::st_transform(calculation_area, 4326)
    bbox <- sf::st_bbox(address)

    # Search in the planetary computer and get the esa world cover collection
    matches <- rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1/") %>%
      rstac::stac_search(collections = "esa-worldcover",
                         bbox = bbox) %>%
      rstac::get_request() %>%   rstac::items_sign(sign_fn = rstac::sign_planetary_computer())
    # Look for the start date times in the esa-worldcover collection
    date_time <- matches %>%
      rstac::items_reap(field = c("properties", "start_datetime"))

    # Select the year whcih is put in by the user, otherwise the default year.
    selected_item <- matches$features[[which(date_time == paste0(year,"-01-01T00:00:00Z"))]]
    #
    land_cover <- terra::rast( paste0("/vsicurl/", selected_item$assets$map$href))
    bbox_proj <- bbox %>%  sf::st_as_sfc() %>%  sf::st_transform(sf::st_crs(land_cover)) %>% terra::vect()
    land_cover <- terra::rast( paste0("/vsicurl/", selected_item$assets$map$href)) %>%  terra::crop(bbox_proj)


    land_cover_values <- terra::extract(land_cover, calculation_area)
    names(land_cover_values) <- c('ID', 'land_cover')

    raster_values <- replace(land_cover_values, is.na(land_cover_values), 0)



    # count the amount of land cover values per polygon
    class_raster_values <- dplyr::count(raster_values, ID, N = get('land_cover'))
    # Convert to wide format + replacing NA
    class_raster_values_wide <- tidygraph::mutate_all(tidyr::spread(class_raster_values, N, n), ~tidyr::replace_na(.,0))
    # Calculate the %
    class_raster_values_perc <- round(cbind(class_raster_values_wide$ID, class_raster_values_wide[,-1]/rowSums(class_raster_values_wide[,-1])), 2)
    codes <- c("UID", unique(c('10', '20', '30', '40', '50', '60', '70', '80', '90', '95', '100')))
    # replace the NA values to <NA>
    codes <- replace(codes, is.na(codes), "<NA>")


    land_cover_values <- terra::extract(land_cover, calculation_area)
    names(land_cover_values) <- c('ID', 'land_cover')

    raster_values <- replace(land_cover_values, is.na(land_cover_values), 0)



    # count the amount of land cover values per polygon
    class_raster_values <- dplyr::count(raster_values, ID, N = get('land_cover'))
    # Convert to wide format + replacing NA
    class_raster_values_wide <- tidygraph::mutate_all(tidyr::spread(class_raster_values, N, n), ~tidyr::replace_na(.,0))
    # Calculate the %
    class_raster_values_perc <- round(cbind(class_raster_values_wide$ID, class_raster_values_wide[,-1]/rowSums(class_raster_values_wide[,-1])), 2)
    names(class_raster_values_perc)[1] <- "UID"
    if (!missing(UID)){
      class_raster_values_perc$UID <- UID
    }

    missings <- setdiff(c("UID", codes), names(class_raster_values_perc))
    missings_df <- setNames(data.frame(matrix(ncol = length(missings), nrow = nrow(address_location))), missings)
    missings_df <- replace(missings_df, is.na(missings_df), 0)
    address <- sf::st_geometry(address_location)

    df <- data.frame(class_raster_values_perc, missings_df, address) %>% sf::st_as_sf()
    #order the columns to give meaningful names to the columns
    df <- df[(c("UID", "geometry", "X10", "X20",
                "X30", "X40", "X50", "X60", "X70",
                "X80", "X90", "X95", "X100"))]
    # Give the columns meaningful names
    names(df) <- c("UID", "geometry", "tree_cover", "shrubland",
                   "grassland", "cropland", "built-up", "bare_vegetation", "snow_ice",
                   "perm_water_bodies", "herbaceous_wetland", "mangroves", "moss_lichen")

  }
  else{

    ### Calculations
    # Extract the landcover values within the buffer
    class_raster_values <- terra::extract(class_raster, calculation_area)
    # count the amount of land cover values per polygon
    class_raster_values <- dplyr::count(class_raster_values, ID, N = get(rast_value_name))
    # Convert to wide format + replacing NA
    class_raster_values_wide <- tidygraph::mutate_all(tidyr::spread(class_raster_values, N, n), ~tidyr::replace_na(.,0))
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
    address <- sf::st_geometry(address_location)

    # names(address) <- "address"
    #buffer <- calculation_area
    #names(buffer) <- "buffer"
    landcover_values_perc <- cbind(class_raster_values_perc, missings_df, address)

    df <- sf::st_as_sf(landcover_values_perc)
  }
  return(df)
}

