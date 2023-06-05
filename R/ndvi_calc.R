
#'  Creating average NDVI values per location
#'
#' @param address_location  A spatial object representing the location of interest, the location should be in projected coordinates.
#' @param raster raster file with NDVI values
#' @param buffer_distance A distance in meters to create a buffer or isochrone around the address location
#' @param network_file An optional sfnetwork object representing a road network, If missing the road network will be created.
#' @param UID A character string representing a unique identifier for each point of interest
#' @param address_calculation A logical, indicating whether to calculate the address location (if not a point) as the centroid of the polygon containing it (default is 'TRUE')
#' @param speed A numeric value representing the speed in km/h to calculate the buffer distance (required if `time` is provided)
#' @param time A numeric value representing the travel time in minutes to calculate the buffer distance (required if `speed` is provided)
#' @param engine When the raster is missing, users can choose whether they want to use Google Earth engine `gee` or Planetary Computer `pc` to calculate the ndvi
#' @param network_buffer A logical, the default is an euclidean buffer, when TRUE, a network buffer will be used.
#' @param city When using a network buffer, you can add a city where your address points are to speed up the process
#' @param start_date The start date from when the satellite images will be filtered `yyyy-mm-dd` default = `2020-01-01`
#' @param end_date  The end date from when the satellite images will be filtered. `yyyy-mm-dd` default = `2021-01-01`
#' @param download_dir A directory to download the network file, the default will be `tempdir()`.
#' @param save_NDVI If you want to save the NDVI values, default is `FALSE`
#' @param plot_NDVI If you want to plot the NDVI, default is `FALSE`
#' @param epsg_code A espg code to get a Projected CRS in the final output, If missing, the default is `3395`
#'
#' @return A `sf` dataframe with the mean ndvi, the geometry and the buffer that was used
#' @export
#'
#' @examples

calc_ndvi<- function(address_location, raster, buffer_distance=NULL, network_buffer=FALSE, download_dir = tempdir(),
                     epsg_code=NULL, network_file=NULL,  UID=NULL, address_calculation = TRUE, speed=NULL, time=NULL, engine='pc',
                           city=NULL, start_date='2020-01-01', end_date='2021-01-01', save_NDVI=FALSE, plot_NDVI=FALSE) {

  start_function <- Sys.time()
######
#Preparation

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
    #sf::st_crs(address_location) <- 3395
  } else{
    projected_crs <- sf::st_crs(address_location)
  }
  ### Address vs area
  if (address_calculation) {
######
#Check for any polygons, convert into centroids if there are any
    if ("POINT" %in% sf::st_geometry_type(address_location)) {
    }else if (missing(buffer_distance)) {
      stop("You do not have a point geometry and did not provide a buffer, please provide a point geometry or a buffer distance")
    }
    else {
      message('There are nonpoint geometries, they will be converted into centroids')
      address_location <- sf::st_centroid(address_location)
    }
#####
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

#####
# If people want to calculate the network buffer.
    if (network_buffer) {
      message('You will use a network to create a buffer around the address location(s),
              Keep in mind that for large files it can take a while to run the funciton.')
#####
# If the network file is missing create the network file using osmextract
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
        ### Extracting OSM road structure to build isochrone polygona
        iso_area <- sf::st_buffer(sf::st_convex_hull(
          sf::st_union(sf::st_geometry(address_location))),
          buffer_distance)
        iso_area <- sf::st_transform(iso_area, crs = 4326)
        # bbox might be redundant
        bbox <- sf::st_bbox(iso_area)
        # Use the osmextract package to extract the lines in the area.
        if (!missing(city)) {
             lines <- osmextract::oe_get(city, stringsAsFactors=FALSE, boundary=iso_area,
                                      downlaod_directory=download_dir, max_file_size = 5e+09, boundary_type = 'spat')

        } else{
          message('If a city is missing, it will take more time to run the function')
          lines <- osmextract::oe_get(iso_area, stringsAsFactors=FALSE, boundary=iso_area,
                                      downlaod_directory=download_dir, max_file_size = 5e+09, boundary_type = 'spat')
        }

      }
######
# If network file is given, make sure the crs are both the same
      else{
        #Check if the address location and the network file that was given have the same CRS.
        if (sf::st_crs(address_location) != sf::st_crs(network_file))
        {
          print("The CRS of your network data set is geographic, CRS of main data set will be used to transform")
          network_file <- sf::st_transform(network_file, sf::st_crs(address_location))
        }


      }

######
# Calculation of the network files


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
      start <- Sys.time()

      # Compute the edge weights bsased on their length
      network_file <- tidygraph::mutate(tidygraph::activate(network_file, "edges"),
                                        weight = sfnetworks::edge_length())

      if(!missing(speed)){

        network_file <- network_file%>%
          tidygraph::activate("edges") %>%
          tidygraph::mutate(speed = units::set_units(speed[dplyr::cur_group_id()], "m/s")) %>%
          tidygraph::mutate(weight = weight / speed) %>%
          tidygraph::ungroup()
      }

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
          nearest_features[i], weights = weight))
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
######
# Calculation of the raster Planetary computer, if raster is missing
    if (engine == 'pc'){
      address <- address_location
      #projected_crs <- sf::st_crs(address)
      address <- sf::st_transform(address, 4326)
      #calculation_area <- sf::st_geometry(calculation_area)
      calculation_area <- sf::st_transform(calculation_area, 4326)
      # Make a bbox around the calculationa area

      # give the time range
      time_range <- rstac::cql2_interval(start_date, end_date)
      # save the area of interest
      area_of_interest <- rstac::cql2_bbox_as_geojson(bbox)

      # stack the tiles that conform the following conditions
      stac_items <- planetary_computer %>%
        rstac::ext_filter(
          # The collection should be in sentinel-2-l2a
          collection %in% c("sentinel-2-l2a") &&
            # The date time has to be within the time frame
            t_intersects(datetime, {{time_range}}) &&
            # The shape file should contain the area of interest
            s_contains(geometry, {{area_of_interest}}) &&
            # And the cloud coverage should be less than 20%
            `eo:cloud_cover` < 20
        ) |>
        rstac::post_request()

      # sign in pc
      matches <- stac_items %>% rstac::items_sign(sign_fn = rstac::sign_planetary_computer())
      # look at the cloud cover for all matches
      cloud_cover <- matches %>%
        rstac::items_reap(field = c("properties", "eo:cloud_cover"))
      # select the leas cloudy day
      selected_item <- matches$features[[which.min(cloud_cover)]]

      cat('Sentinel-2-l2a data is used to retrieve the ndvi values. \n The ID of the selected image is: ', selected_item$id,
          '\n The date of the picture that was taken is: ',selected_item$properties$datetime,
          '\n The cloud cover of this day was ', min(cloud_cover),'%', sep='')
      # extract the red band href from the selected item and raster it.
      red <- terra::rast( paste0("/vsicurl/", selected_item$assets$B04$href))

      # get tge bbox projection of the bbox that was given and create a spat vector
      bbox_proj <- bbox %>%  sf::st_as_sfc() %>%  sf::st_transform(sf::st_crs(red)) %>% terra::vect()

      # crop the spatvector bbox to the nir and red band.
      red <- terra::rast( paste0("/vsicurl/", selected_item$assets$B04$href)) %>%  terra::crop(bbox_proj)
      nir <- terra::rast( paste0("/vsicurl/", selected_item$assets$B08$href) ) %>%  terra::crop(bbox_proj)

      # calculate the ndvi
      ndvi <- (nir-red) / (red+nir)
      # make sure the calculation area has the same crs as ndvi
      calculation_area_proj <- sf::st_transform(calculation_area, terra::crs(ndvi))
      # extract the ndvi values
      ndvi_values <- terra::extract(ndvi, calculation_area_proj)

      names(ndvi_values) <- c('ID', 'NDVI')
      # replace the missing values with 0.
      raster_values <- replace(ndvi_values, is.na(ndvi_values), 0)
      if (save_NDVI){
        # function to save the ndvi

      }
      # Calculate the average NDVI
      average_rast <- dplyr::summarise(tidygraph::group_by(raster_values, ID), mean_NDVI=mean(NDVI), .groups = 'drop')
      calculation_area <- sf::st_transform(calculation_area, projected_crs)
      if (plot_NDVI){
        terra::animate(ndvi)
      }
    }
    else if (engine=='gee') {
#####
# Calculation of the raster with google earth engine if the raster is missing
      rgee::ee_Initialize()
      calculation_area <- sf::st_geometry(calculation_area)
      calculation_area <- sf::st_transform(calculation_area, 4326)
      cal <- calculation_area %>% rgee::sf_as_ee()
      region <- cal$geometry()$bounds()
      s2 <- rgee::ee$ImageCollection("COPERNICUS/S2_SR")
      getNDVI <- function(image) {
        ndvi <- image$normalizedDifference(c("B8", "B4"))$rename('NDVI')
        return(image$addBands(ndvi))
      }

      s2_NDVI <- s2$
        filterBounds(region)$
        filter(rgee::ee$Filter$lte("CLOUDY_PIXEL_PERCENTAGE", 10))$
        filter(rgee::ee$Filter$date(start_date, end_date))$map(getNDVI)$mean()

      s2_NDVI <- s2_NDVI$select('NDVI')
      average_rast <- rgee::ee_extract(s2_NDVI, calculation_area)

      if(plot_NDVI) {
        message('The gee function is not compatible with plotting the ndvi yet.')
      }

      calculation_area <- sf::st_transform(calculation_area, projected_crs)

    } else{
      stop("You should enter whether you want to use Google Earth Engine (gee) or Planetary Computer (pc)")

    }

  } else{
#####
# Calculation of raster when raster is given
    # Extract the NDVI values within the buffer
    raster_values <- terra::extract(raster, calculation_area)
    raster_values <- replace(raster_values, is.na(raster_values), 0)
    # Calculate the average NDVI
    average_rast <- dplyr::summarise(tidygraph::group_by(raster_values, ID), mean_NDVI=mean(NDVI_data_test), .groups = 'drop')

  }


  # Update UID


  if (!missing(UID)){
    average_rast$UID <- UID
  }
  address_location <- sf::st_transform(address_location, projected_crs)

  address <- sf::st_geometry(address_location)

  ndvi_avg <- data.frame(average_rast, address)
  ndvi_avg <- sf::st_as_sf(ndvi_avg)

  print(Sys.time()-start_function)
  print('Amount of run time')

  # Return the result
  return(ndvi_avg)
}





