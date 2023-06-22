#' Viewshed Greenness Visibility Index (VGVI) from sf
#' @description The VGVI expresses the proportion of visible greenness to the total visible area based on a \code{\link[GreenExp]{viewshed}}.
#' The estimated VGVI values range between 0 and 1, where 0 = no green cells are visible, and 1 = all of the visible cells are green.
#' A distance decay function is applied, to account for the reducing visual prominence of an object in space with increasing distance from the address.
#'
#' @param address object of class \code{sf}; address location(s) from where the VGVI should be computed. See ‘Details’ for valid sf geometry types
#' @param dsm_rast object of class \code{\link[terra]{rast}}; \code{\link[terra]{rast}} of the DSM
#' @param dtm_rast object of class \code{\link[terra]{rast}}; \code{\link[terra]{rast}} of the DTM
#' @param greenspace_rast object of class \code{\link[terra]{rast}}; \code{\link[terra]{rast}} of the binary Greenspace mask. Values of the Greenspace mask must be 1 for Green and 0 for No-Green
#' @param max_distance numeric; Buffer distance to calculate the viewshed
#' @param observer_height numeric > 0; Height of the observer (e.g. 1.7 meters)
#' @param raster_res optional; NULL or numeric > 0; Resolution that the GVI raster should be aggregated to. Needs to be a multible of the dsm_rast resolution
#' @param spacing optional; numeric > 0; Only if \code{address} is a linestring (or polygon), points on the line (or on a grid) will be generated.
#' The \code{spacing} parameter sets the distance in between the points on the line/grid
#' @param m numeric; See ‘Details’
#' @param b numeric; See ‘Details’
#' @param mode character; 'logit' or 'exponential'. See ‘Details’
#' @param cores numeric; The number of cores to use, i.e. at most how many child processes will be run simultaneously
#' @param folder_path optional; Folder path to where the output should be saved continuously. Must not include a filename extension (e.g. '.shp', '.gpkg').
#' @param folder_path_random_points optional; Folder path to where the output  of the random generated points should be saved continuously. Must not include a filename extension (e.g. '.shp', '.gpkg').
#' @param progress logical; Show progress bar and computation time?
#' @param buffer_distance  A distance in meters to create a buffer or isochrone around the address location, default is 50m.
#' @param sample_points The amount of points that should be sampled in the buffer created around the address to calculate the mean vgvi.
#'
#' @details
#' address needs to be a geometry of type POINT, LINESTRING, MULTILINESTRING, POLYGON or MULTIPOLYGON. If address is a LINESTRING or MULTILINESTRING,
#' points will be generated along the line(s) every "resolution" meters. If address is a POLYGON or MULTIPOLYGON, a grid with resolution = "resolution"
#' will be generated, and VGVI will be computed for every point.
#' The CRS (\code{\link[sf]{st_crs}}) needs to have a metric unit!
#'
#' The type of function, used for calculating the distance decay weights, can be defined with the \code{mode} parameter.
#' The argument 'logit' uses the logistic function, d = 1 / (1 + e^(b * (x - m))) and 'exponential' the exponential function d = 1 / (1 + (b * x^m)).
#' The decay function can be visualized using the \code{\link[GreenExp]{visualizeWeights}} function.
#'
#' @return sf_object containing the weighted VGVI values as POINT features, where 0 = no green cells are visible, and 1 = all of the visible cells are green.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom sf st_crs
#' @importFrom sf st_geometry_type
#' @importFrom sf st_union
#' @importFrom sf st_cast
#' @importFrom sf st_line_sample
#' @importFrom sf st_as_sf
#' @importFrom sf st_bbox
#' @importFrom sf st_buffer
#' @importFrom sf st_coordinates
#' @importFrom sf st_as_sfc
#' @importFrom sf st_cast
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr relocate
#' @importFrom dplyr n
#' @importFrom dplyr everything
#' @importFrom terra crs
#' @importFrom terra res
#' @importFrom terra rast
#' @importFrom terra crop
#' @importFrom terra mask
#' @importFrom terra vect
#' @importFrom terra xyFromCell
#' @importFrom terra terraOptions
#' @importFrom terra aggregate
#' @importFrom terra extract
#' @importFrom terra cellFromXY
#' @importFrom terra colFromX
#' @importFrom terra rowFromY
#' @importFrom terra writeRaster
#' @importFrom terra rast
#' @importFrom raster raster
#' @importFrom methods is
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#' @useDynLib GreenExp, .registration = TRUE

vgvi_from_address <- function(address, dsm_rast, dtm_rast, greenspace_rast,
                         buffer_distance=50, sample_points = 30,
                         max_distance = 800, observer_height = 1.7,
                         raster_res = NULL, spacing = raster_res,
                         m = 0.5, b = 8, mode = c("logit", "exponential"),
                         cores = 1, folder_path = NULL,
                         folder_path_random_points = NULL, progress = FALSE) {

  # Use st_sample to create samples within buffer
  # group by id, take the average of the vgvi
  #### 1. Check input ####
  # address
  valid_sf_types <- c("POINT", "MULTIPOINT", "LINESTRING", "MULTILINESTRING", "POLYGON", "MULTIPOLYGON")
  if (!is(address, "sf")) {
    stop("address must be a sf object")
  } else if (is.null(sf::st_crs(address)$units) || sf::st_crs(address)$units_gdal == "degree") {
    warning("The CRS in your main data set has geographic coordinates, the Projected CRS will be set to WGS 84 / World Mercator")
    if (missing(epsg_code)) {
      projected_crs <- sf::st_crs(3395)
      address <- sf::st_transform(address, projected_crs)
    }
    else{
      projected_crs<-sf::st_crs(epsg_code)
      address <- sf::st_transform(address, projected_crs)
    }
  } else if (!as.character(sf::st_geometry_type(address, by_geometry = FALSE)) %in% valid_sf_types) {
    stop("address has no valid geometry")
  } else if (as.character(sf::st_geometry_type(address, by_geometry = FALSE)) == "MULTIPOINT") {
    address <- sf::st_cast(address, "POINT")
  }
  rm(valid_sf_types)

  # dsm_rast
  if (!is(dsm_rast, "SpatRaster")) {
    stop("dsm_rast needs to be a SpatRaster object!")
  }  else if(sf::st_crs(terra::crs(dsm_rast))$epsg != sf::st_crs(address)$epsg){
    warning('The crs of your raster is not the same as the projected crs of the input location,
              the projected crs of the raster will be transformed into the projected crs of the address location')
    epsg_dsm_rast <- sf::st_crs(address)$epsg
    dsm_rast <- terra::project(dsm_rast, paste0('EPSG:',epsg_dsm_rast))

  } else if(dsm_rast@ptr$res[1] != dsm_rast@ptr$res[2]) {
    stop("dsm_rast: x and y resolution must be equal.\nSee https://github.com/STBrinkmann/GVI/issues/1")
  }

  # dtm_rast
  if (!is(dtm_rast, "SpatRaster")) {
    stop("dtm_rast needs to be a SpatRaster object!")
  } else if(sf::st_crs(terra::crs(dtm_rast))$epsg != sf::st_crs(address)$epsg){
    warning('The crs of your DTM raster is not the same as the projected crs of the input location,
              the projected crs of the raster will be transformed into the projected crs of the address location')
    epsg_dtm_rast <- sf::st_crs(address)$epsg
    dtm_rast <- terra::project(dtm_rast, paste0('EPSG:',epsg_dtm_rast))

  }

  # greenspace_rast
  if (!is(greenspace_rast, "SpatRaster")) {
    stop("greenspace_rast needs to be a SpatRaster object!")
  } else if(sf::st_crs(terra::crs(greenspace_rast))$epsg != sf::st_crs(address)$epsg){
    warning('The crs of your greenspace raster is not the same as the projected crs of the input location,
              the projected crs of the raster will be transformed into the projected crs of the address location')
    epsg_greenspace_rast <- sf::st_crs(address)$epsg
    greenspace_rast <- terra::project(greenspace_rast, paste0('EPSG:',epsg_greenspace_rast))

  } else if(dsm_rast@ptr$res[1] != dsm_rast@ptr$res[2]) {
    stop("greenspace_rast: x and y resolution must be equal.\nSee https://github.com/STBrinkmann/GVI/issues/1")
  }

  # max_distance
  max_distance <- round(max_distance, digits = 0)

  # raster_res
  dsm_res <- min(terra::res(dsm_rast))
  if (is.null(raster_res)) {
    raster_res = dsm_res
  } else if (raster_res < min(terra::res(dsm_rast))) {
    stop("raster_res must be higher than the resolution of dsm_rast")
  } else if ((raster_res %% dsm_res) != 0) {
    stop(paste0("raster_res must be a multible of the dsm_rast resolution. Try raster_res = ", raster_res - (raster_res %% dsm_res)))
  }
  rm(dsm_res)

  # spacing
  if (is.null(spacing)) {
    spacing <- raster_res
  }

  # mode
  if (is.character(mode) && (mode == "logit")){
    mode = 1
  } else if (is.character(mode) && (mode == "exponential")){
    mode = 2
  } else if (is.character(mode) && (mode == c("logit", "exponential"))){
    mode = 2
  } else {
    stop("mode must be logit or exponential")
  }

  # folder_path
  if (!is.null(folder_path)) {
    if (!dir.exists(folder_path)) {
      dir.create(folder_path)
    }
    folder_path <- tempfile(pattern = "VGVI_MEAN_", tmpdir = file.path(folder_path), fileext = ".gpkg")
  }

  folder_path_random_points
  if (!is.null(folder_path_random_points)) {
    if (!dir.exists(folder_path_random_points)) {
      dir.create(folder_path_random_points)
    }
    folder_path_random_points <- tempfile(pattern = "VGVI_random_points_", tmpdir = file.path(folder_path_random_points), fileext = ".gpkg")
  }


  #### 2. Convert address to points ####
  ######### modify from here
  if(progress) {
    message("Preprocessing:")
    pb = txtProgressBar(min = 0, max = 5, initial = 0, style = 2)
  }

 if (as.character(sf::st_geometry_type(address, by_geometry = FALSE)) %in% c("POLYGON", "MULTIPOLYGON")) {
    address_bbox <- sf::st_bbox(address)
    address <- terra::rast(xmin = address_bbox[1], xmax = address_bbox[3],
                            ymin = address_bbox[2], ymax = address_bbox[4],
                            crs = terra::crs(dsm_rast), resolution = spacing, vals = 0) %>%
      terra::crop(terra::vect(address)) %>%
      terra::mask(terra::vect(address)) %>%
      terra::xyFromCell(which(terra::values(.) == 0)) %>%
      as.data.frame() %>%
      sf::st_as_sf(coords = c("x","y"), crs = sf::st_crs(address)) %>%
      dplyr::rename(geom = geometry)
    rm(address_bbox)
  }
  if (progress) setTxtProgressBar(pb, 1)

  # Get the address geometry
  address_geom <- sf::st_geometry(address)


  #### 3. Prepare data for viewshed analysis ####
  # Max AOI
  max_aoi <- address %>%
    sf::st_bbox() %>%
    sf::st_as_sfc() %>%
    sf::st_buffer(max_distance)

  # Crop DSM to max AOI and change resolution
  dsm_rast <- terra::crop(dsm_rast, terra::vect(max_aoi))
  greenspace_rast <- terra::crop(greenspace_rast, terra::vect(max_aoi))

  if(raster_res != min(raster::res(dsm_rast))) {
    terra::terraOptions(progress = 0)
    dsm_rast <- terra::aggregate(dsm_rast, fact = raster_res/terra::res(dsm_rast))
    terra::terraOptions(progress = 3)
  }

  dsm_vec <- terra::values(dsm_rast, mat = FALSE)
  greenspace_vec <- terra::values(greenspace_rast, mat = FALSE)

  dsm_cpp_rast <- dsm_rast %>% terra::rast() %>% raster::raster()
  greenspace_cpp_rast <- greenspace_rast %>% terra::rast() %>% raster::raster()

  random_points_df <- data.frame(ID = integer(), geometry = sf::st_sfc(), stringsAsFactors = FALSE) %>% sf::st_as_sf() %>%
    sf::st_set_crs(sf::st_crs(address))

  calc_area <- sf::st_buffer(address, buffer_distance)

  for (i in 1:nrow(calc_area)) {
    polygon <- calc_area[i, ]

    # Generate random points within the polygon
    random_points <- sf::st_sample(sf::st_geometry(polygon), sample_points, type = "random")

    # Create a data frame for the random points with the corresponding polygon ID
    points_df <- data.frame(ID = i, geometry = random_points, stringsAsFactors = FALSE)

    # Append the points to the random_points_df data frame
    random_points_df <- dplyr::bind_rows(random_points_df, points_df)
  }



  # Coordinates of start point
  x0 <- sf::st_coordinates(random_points_df)[,1]
  y0 <- sf::st_coordinates(random_points_df)[,2]

  # Observer heights
  height_0_vec <- unlist(terra::extract(dtm_rast, cbind(x0, y0)), use.names = F) + observer_height

  if (progress) setTxtProgressBar(pb, 2)


  #### 4. Remove points outside the DSM or DTM ####
  invalid_points <- unique(c(
    which(is.na(terra::extract(dsm_rast, cbind(x0, y0)))), # points outside the DSM
    which(is.na(height_0_vec)) # points outside the DTM
  ))

  # Remove invalid points
  if (length(invalid_points) > 0) {
    address <- address[-invalid_points, ]
    x0 <- x0[-invalid_points]
    y0 <- y0[-invalid_points]
    height_0_vec <- height_0_vec[-invalid_points]
  }

  if (progress) setTxtProgressBar(pb, 3)


  #### 5. Last steps of PreProcessing ####
  # Prepare address for output

  random_points_df <- random_points_df %>%
    dplyr::mutate(VGVI = as.numeric(NA),
                  id = 1:dplyr::n()) %>%
    dplyr::select(id, VGVI, dplyr::everything())




#
#   address <- address %>%
#     dplyr::mutate(VGVI = as.numeric(NA),
#                   id = 1:dplyr::n()) %>%
#     dplyr::select(id, VGVI, dplyr::everything())


  # convert x0/y0 to col/row
  c0 <- terra::colFromX(dsm_rast, x0)
  r0 <- terra::rowFromY(dsm_rast, y0)

  if (progress) setTxtProgressBar(pb, 4)



  # 6. Final update of Pre-Processing ProgressBar
  if (progress) {
    setTxtProgressBar(pb, 5)
    cat("\n")
  }
  if (length(invalid_points) == 1) {
    message("1 point has been removed, because it was outside of the DSM or DTM")
  } else if (length(invalid_points) > 1) {
    message(paste(length(invalid_points), "points have been removed, because they were outside of the DSM or DTM"))
  }
  invisible(gc())

  #### 7. Calculate viewsheds and VGVI ####
  if (progress) {
    message(paste0("Computing VGVI for ", nrow(address), ifelse(nrow(address)>1, " points:", " point:")))
    #pb = txtProgressBar(min = 0, max = length(address_list), initial = 0, style = 3)
    start_time <- Sys.time()
  }

  vgvi_values <- VGVI_cpp(dsm = dsm_cpp_rast, dsm_values = dsm_vec,
                          greenspace = greenspace_cpp_rast, greenspace_values = greenspace_vec,
                          x0 = c0, y0 = r0, radius = max_distance, h0 = height_0_vec,
                          fun = mode, m = m, b = b, ncores = cores, display_progress = progress)

  valid_values <- unlist(lapply(vgvi_values, is.numeric), use.names = FALSE)



  random_points_df[valid_values,2] <- vgvi_values[valid_values]

  if(!is.null(folder_path_random_points)) {
    df_save <- data.frame(UID = random_points_df$id, VGVI = random_points_df$VGVI,
                          ID = random_points_df$ID, random_points_df$geometry ) %>% sf::st_as_sf()
    sf::st_write(df_save, folder_path_random_points, append=TRUE, quiet = T)


  }


  address <- random_points_df %>%
    dplyr::group_by(ID) %>%
    dplyr::summarize(mean_VGVI = mean(VGVI))

  colnames(address)[3] <- 'geometry'

  address$geometry <- address_geom



  if (!is.null(folder_path)) {
    sf::st_write(address, folder_path, append = TRUE, quiet = T)
  }


  if (progress) {
    time_dif <- round(cores * ((as.numeric(difftime(Sys.time(), start_time, units = "s"))*1000) / nrow(address)), 2)
    cat("\n")

    time_total <- round(as.numeric(difftime(Sys.time(), start_time, units = "m")))
    if(time_total < 1){
      time_total <- round(as.numeric(difftime(Sys.time(), start_time, units = "s")))

      if(time_total < 1){
        time_total <- round(as.numeric(difftime(Sys.time(), start_time, units = "s")))*1000
        message(paste("Total runtime:", time_total, " milliseconds"))
      } else {
        message(paste("Total runtime:", time_total, " seconds"))
      }
    } else {
      message(paste("Total runtime:", time_total, " minutes"))
    }

    message(paste("Average time for a single point:", time_dif, "milliseconds"))
  }

  rm(dsm_cpp_rast, dsm_vec, greenspace_cpp_rast, greenspace_vec, c0, r0, height_0_vec)
  invisible(gc())
  message('This code is retrieved from the GVI function, for more information look at https://github.com/STBrinkmann/GVI ')

  return(address)
}
